package markdown

import (
	"bytes"
	"strconv"
	"strings"

	"github.com/yuin/goldmark/ast"
	extast "github.com/yuin/goldmark/extension/ast"
)

// Excerpt returns the part of source between the byte offsets start and end
// as Markdown that renders the way that part looked in the whole document. A
// selection on screen covers rendered text, while the syntax that styled it
// sits outside the selection, so a plain slice of the source would lose it or
// leave it unbalanced. The offsets are expected to fall in rendered text, as
// a source map of the rendering hands them over.
//
//   - An inline element the selection starts or ends inside is closed with
//     its own delimiters, read from the source so __x__ stays __x__.
//   - A link whose label is partly selected keeps its destination, with the
//     label cut to the selection: [part](url) or, for a full reference,
//     [part][ref]. Links whose text is also their target are taken whole: a
//     collapsed or shortcut reference, an autolink, a bare URL.
//   - Block structure is kept only when the selection crosses blocks. Then
//     the markers of the line it starts on are restored (quote, list item,
//     heading, code fence), without the indentation of a nested list, a code
//     block it ends inside gets its closing fence, and a table is copied as
//     whole rows under its header. A selection within one block copies its
//     inline content alone, so code comes without fences.
//   - Whitespace at either end is dropped, except inside code.
//   - Text that would read as block syntax at the start of the excerpt, such
//     as "# of items", is escaped, and so is punctuation at either end that
//     could now open or close emphasis or code, such as "_case_" cut from
//     "snake_case_name".
//   - An offset that splits syntax (a delimiter, an escape, an entity, a link
//     destination) moves out of it, so the syntax is taken whole or left out.
//     Offsets outside source are clamped to it, and an empty range yields "".
func Excerpt(source string, start, end int) string {
	start, end = max(0, min(start, len(source))), max(0, min(end, len(source)))
	if start >= end {
		return ""
	}
	x := newExcerpter(source)
	start, end = x.trim(x.snap(start, end))
	if start >= end {
		return ""
	}
	first, last := x.leafFrom(start), x.leafUntil(end)
	switch {
	case first < 0 || last < 0 || first > last:
		// Only syntax between blocks is selected, such as a rule.
		return string(x.src[start:end])
	case first == last:
		return x.within(x.leaves[first], start, end)
	}
	return x.across(x.leaves[first], x.leaves[last], start, end)
}

// excerpter holds a parsed document and where its text and inline syntax sit
// in the source.
type excerpter struct {
	src []byte
	// leaves are the blocks that hold text, in document order.
	leaves []leaf
	// inline holds every inline element with syntax of its own, inner ones
	// before the elements around them.
	inline []element
}

// leaf is a block that holds text: prose (a paragraph, heading, or table
// cell) or code. Its text runs from s to e.
type leaf struct {
	node ast.Node
	s, e int
}

// span is where an inline element sits in the source: its content from cs to
// ce inside its syntax from os to oe. Text has no syntax of its own.
type span struct{ os, cs, ce, oe int }

// element is an inline element and where it sits.
type element struct {
	node ast.Node
	span
}

func newExcerpter(source string) *excerpter {
	x := &excerpter{src: []byte(source)}
	ast.Walk(parse(x.src), func(n ast.Node, entering bool) (ast.WalkStatus, error) {
		if !entering {
			return ast.WalkContinue, nil
		}
		switch n.(type) {
		case *ast.Paragraph, *ast.TextBlock, *ast.Heading, *extast.TableCell:
			x.addProse(n)
			return ast.WalkSkipChildren, nil
		case *ast.FencedCodeBlock, *ast.CodeBlock, *ast.HTMLBlock:
			if lines := n.Lines(); lines.Len() > 0 {
				e := lines.At(lines.Len() - 1).Stop
				if x.src[e-1] == '\n' {
					e--
				}
				x.leaves = append(x.leaves, leaf{n, lines.At(0).Start, e})
			}
			return ast.WalkSkipChildren, nil
		}
		return ast.WalkContinue, nil
	})
	return x
}

func (x *excerpter) addProse(n ast.Node) {
	s, e, ok := x.childRange(n)
	if !ok {
		lines := n.Lines()
		if lines.Len() == 0 {
			return
		}
		s, e = lines.At(0).Start, lines.At(lines.Len()-1).Stop
	}
	x.leaves = append(x.leaves, leaf{n, s, e})
	ast.Walk(n, func(c ast.Node, entering bool) (ast.WalkStatus, error) {
		if !entering && c != n {
			if sp, ok := x.span(c); ok && (sp.os < sp.cs || atomic(c)) {
				x.inline = append(x.inline, element{c, sp})
			}
		}
		return ast.WalkContinue, nil
	})
}

// span measures an inline element. goldmark records where text sits but not
// where delimiters do, so an element's content is measured from its children
// and its delimiters from the source around that.
func (x *excerpter) span(n ast.Node) (span, bool) {
	switch v := n.(type) {
	case *ast.Text:
		return span{v.Segment.Start, v.Segment.Start, v.Segment.Stop, v.Segment.Stop}, true
	case *ast.RawHTML:
		if v.Segments.Len() == 0 {
			return span{}, false
		}
		s, e := v.Segments.At(0).Start, v.Segments.At(v.Segments.Len()-1).Stop
		return span{s, s, e, e}, true
	case *ast.AutoLink:
		return x.autoLinkSpan(v)
	}
	cs, ce, ok := x.childRange(n)
	if !ok {
		return span{}, false
	}
	switch v := n.(type) {
	case *ast.CodeSpan:
		// Only padding sits between the backticks and the content: a space or
		// a line ending, with the container markers of the next line.
		os, oe := cs, ce
		for os > 0 && x.src[os-1] != '`' {
			os--
		}
		for oe < len(x.src) && x.src[oe] != '`' {
			oe++
		}
		return span{os - x.runBefore(os, '`'), cs, ce, oe + x.runFrom(oe, '`')}, true
	case *ast.Emphasis:
		return span{cs - v.Level, cs, ce, ce + v.Level}, true
	case *extast.Strikethrough:
		return span{cs - x.runBefore(cs, '~'), cs, ce, ce + x.runFrom(ce, '~')}, true
	case *ast.Link:
		return span{cs - 1, cs, ce, x.linkEnd(ce, v.Reference)}, true
	case *ast.Image:
		return span{cs - 2, cs, ce, x.linkEnd(ce, v.Reference)}, true
	}
	return span{}, false
}

// childRange returns where the children of n sit, from the start of the first
// to the end of the last.
func (x *excerpter) childRange(n ast.Node) (s, e int, ok bool) {
	for c := n.FirstChild(); c != nil; c = c.NextSibling() {
		sp, found := x.span(c)
		if !found {
			continue
		}
		if !ok {
			s, ok = sp.os, true
		}
		e = sp.oe
	}
	return s, e, ok
}

// autoLinkSpan finds an autolink's label, which goldmark keeps without its
// position, by searching from where the node starts.
func (x *excerpter) autoLinkSpan(v *ast.AutoLink) (span, bool) {
	label := v.Label(x.src)
	from := max(0, v.Pos())
	i := bytes.Index(x.src[from:], label)
	if len(label) == 0 || i < 0 {
		return span{}, false
	}
	s := from + i
	e := s + len(label)
	if s > 0 && x.src[s-1] == '<' && e < len(x.src) && x.src[e] == '>' {
		return span{s - 1, s, e, e + 1}, true
	}
	return span{s, s, e, e}, true
}

// atomic reports whether an element must be taken whole: its text is also
// its target, so a cut would point somewhere else.
func atomic(n ast.Node) bool {
	switch v := n.(type) {
	case *ast.AutoLink:
		return true
	case *ast.Link:
		return v.Reference != nil && v.Reference.Type != ast.ReferenceLinkFull
	case *ast.Image:
		return v.Reference != nil && v.Reference.Type != ast.ReferenceLinkFull
	}
	return false
}

// linkEnd returns where a link's syntax ends after a label that ends at at:
// past the closing bracket and then the destination or the reference.
func (x *excerpter) linkEnd(at int, ref *ast.ReferenceLink) int {
	if at >= len(x.src) || x.src[at] != ']' {
		return at
	}
	i := at + 1
	switch {
	case ref == nil:
		return x.destinationEnd(i)
	case ref.Type == ast.ReferenceLinkFull:
		return x.closing(i, ']')
	case ref.Type == ast.ReferenceLinkCollapsed:
		return i + 2
	}
	return i
}

// destinationEnd returns where an inline link's "(destination "title")"
// starting at i ends.
func (x *excerpter) destinationEnd(i int) int {
	src := x.src
	if i >= len(src) || src[i] != '(' {
		return i
	}
	i = x.skipSpace(i + 1)
	if i < len(src) && src[i] == '<' {
		i = x.closing(i, '>')
	} else {
		for depth := 0; i < len(src); i++ {
			c := src[i]
			if c == '\\' {
				i++
				continue
			}
			if c == ' ' || c == '\t' || c == '\n' || c == ')' && depth == 0 {
				break
			}
			if c == '(' {
				depth++
			} else if c == ')' {
				depth--
			}
		}
	}
	i = x.skipSpace(i)
	if i < len(src) && strings.IndexByte("\"'(", src[i]) >= 0 {
		closer := src[i]
		if closer == '(' {
			closer = ')'
		}
		i = x.skipSpace(x.closing(i, closer))
	}
	if i < len(src) && src[i] == ')' {
		i++
	}
	return min(i, len(src))
}

// closing returns the offset past the first unescaped closer after i.
func (x *excerpter) closing(i int, closer byte) int {
	for i++; i < len(x.src) && x.src[i] != closer; i++ {
		if x.src[i] == '\\' {
			i++
		}
	}
	return min(i+1, len(x.src))
}

func (x *excerpter) skipSpace(i int) int {
	for i < len(x.src) && strings.IndexByte(" \t\n", x.src[i]) >= 0 {
		i++
	}
	return i
}

func (x *excerpter) runBefore(i int, c byte) int {
	n := 0
	for i-n > 0 && x.src[i-n-1] == c {
		n++
	}
	return n
}

func (x *excerpter) runFrom(i int, c byte) int {
	n := 0
	for i+n < len(x.src) && x.src[i+n] == c {
		n++
	}
	return n
}

// snap moves an offset that splits syntax out of it, so the syntax is taken
// whole or left out: a delimiter or link destination, an escape, an entity.
// Inner elements come first, so an offset leaving one lands where the element
// around it can move it on.
func (x *excerpter) snap(start, end int) (int, int) {
	for _, el := range x.inline {
		if atomic(el.node) {
			if el.os < start && start < el.oe {
				start = el.os
			}
			if el.os < end && end < el.oe {
				end = el.oe
			}
			continue
		}
		if el.os < start && start < el.cs {
			start = el.os
		} else if el.ce <= start && start < el.oe {
			start = el.oe
		}
		if el.ce < end && end < el.oe {
			end = el.oe
		} else if el.os < end && end <= el.cs {
			end = el.os
		}
	}
	if x.escapes(start - 1) {
		start--
	}
	if x.escapes(end - 1) {
		end++
	}
	if a, _, ok := x.entity(start); ok {
		start = a
	}
	if _, b, ok := x.entity(end); ok {
		end = b
	}
	return start, end
}

// escapes reports whether the byte at i is a backslash escaping the next.
func (x *excerpter) escapes(i int) bool {
	if i < 0 || i+1 >= len(x.src) || x.src[i] != '\\' || !isPunct(x.src[i+1]) || x.inCode(i) {
		return false
	}
	n := 0
	for j := i; j >= 0 && x.src[j] == '\\'; j-- {
		n++
	}
	return n%2 == 1
}

func isPunct(c byte) bool { return strings.IndexByte("!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~", c) >= 0 }

// entity returns the character reference such as "&amp;" that off falls
// strictly inside, from a to b.
func (x *excerpter) entity(off int) (a, b int, ok bool) {
	lo := max(0, off-32)
	a = bytes.LastIndexByte(x.src[lo:off], '&')
	if a < 0 {
		return 0, 0, false
	}
	a += lo
	semi := bytes.IndexByte(x.src[a:min(len(x.src), a+32)], ';')
	if semi < 0 || a+semi < off || x.inCode(a) {
		return 0, 0, false
	}
	name := x.src[a+1 : a+semi]
	if len(name) == 0 || bytes.IndexFunc(name, func(r rune) bool {
		return !('a' <= r && r <= 'z' || 'A' <= r && r <= 'Z' || '0' <= r && r <= '9' || r == '#')
	}) >= 0 {
		return 0, 0, false
	}
	return a, a + semi + 1, true
}

// trim drops whitespace at either end, keeping it inside code, where it is
// content.
func (x *excerpter) trim(start, end int) (int, int) {
	for start < end && isSpace(x.src[start]) && !x.inCode(start) {
		start++
	}
	for end > start && isSpace(x.src[end-1]) && !x.inCode(end-1) {
		end--
	}
	return start, end
}

func isSpace(c byte) bool { return c == ' ' || c == '\t' || c == '\n' || c == '\r' }

func (x *excerpter) inCode(off int) bool {
	for _, l := range x.leaves {
		if !isProse(l.node) && l.s <= off && off < l.e {
			return true
		}
	}
	for _, el := range x.inline {
		if _, ok := el.node.(*ast.CodeSpan); ok && el.cs <= off && off < el.ce {
			return true
		}
	}
	return false
}

// leafFrom returns the index of the first leaf whose text ends after off.
func (x *excerpter) leafFrom(off int) int {
	for i, l := range x.leaves {
		if l.e > off {
			return i
		}
	}
	return -1
}

// leafUntil returns the index of the last leaf whose text starts before off.
func (x *excerpter) leafUntil(off int) int {
	for i := len(x.leaves) - 1; i >= 0; i-- {
		if x.leaves[i].s < off {
			return i
		}
	}
	return -1
}

// within excerpts a selection inside one leaf: its inline content alone.
func (x *excerpter) within(l leaf, start, end int) string {
	start, end = max(start, l.s), min(end, l.e)
	if start >= end {
		return ""
	}
	if !isProse(l.node) {
		return x.lines(l.node, start, end)
	}
	lead, from := x.openAt(l.node, start, end)
	trail, to := x.closeAt(l.node, start, end)
	text := lead + x.escapeEdges(l.node, l.node, x.lines(l.node, from, to), from, to) + trail
	if lead == "" {
		text = escapeLead(text)
	}
	return text
}

// across excerpts a selection that crosses blocks, restoring the block
// syntax its ends cut off.
func (x *excerpter) across(first, last leaf, start, end int) string {
	var head, lead, trail string
	from, to := start, end
	indent := 0
	escape := false
	switch {
	case isCell(first.node):
		head, from = x.tableHead(first)
	case start >= lineStartBefore(x.src, first.s):
		// A selection that starts on the line of the text, markers
		// included, gets those markers back.
		from = max(start, first.s)
		var rest string
		head, rest, indent = x.containerMarkers(first.node, from)
		marker := x.leafMarker(first, rest)
		head += marker
		if isProse(first.node) {
			lead, from = x.openAt(first.node, from, end)
			escape = marker == "" && lead == ""
		}
	}
	switch {
	case isCell(last.node):
		to = x.tableRowEnd(last)
	case isProse(last.node):
		// Syntax after the text on its line, such as a heading's closing
		// hashes, is left behind.
		if to > last.e && x.lineEnd(last.e) >= to {
			to = last.e
		}
		trail, to = x.closeAt(last.node, from, to)
	case isFenced(last.node) && to <= last.e:
		trail = "\n" + x.codeIndent(last.node, to-1) + x.fenceCloser(last)
	}
	mid := x.escapeEdges(first.node, last.node, string(x.src[from:max(from, to)]), from, to)
	body := dedent(lead+mid+trail, indent)
	if escape {
		body = escapeLead(body)
	}
	return head + body
}

// openAt returns what an excerpt starting at start inside prose n leads with:
// the opening syntax of the inline elements it starts inside and, when it
// starts inside a code span, that span rebuilt up to end. from is where the
// source picks up after them.
func (x *excerpter) openAt(n ast.Node, start, end int) (lead string, from int) {
	from = start
	for _, el := range x.enclosing(n, func(sp span) bool { return sp.cs <= start && start < sp.ce }) {
		if _, ok := el.node.(*ast.CodeSpan); ok {
			lead += x.codeSpan(el, start, min(end, el.ce))
			from = min(end, el.oe)
			break
		}
		lead += string(x.src[el.os:el.cs])
	}
	return lead, from
}

// closeAt returns what an excerpt ending at end inside prose n closes with:
// the closing syntax of the inline elements it ends inside, innermost first,
// after the code span it ends inside rebuilt from its start. to is where the
// source is cut before them.
func (x *excerpter) closeAt(n ast.Node, start, end int) (trail string, to int) {
	to = end
	chain := x.enclosing(n, func(sp span) bool { return sp.cs < end && end <= sp.ce })
	for i := len(chain) - 1; i >= 0; i-- {
		el := chain[i]
		if _, ok := el.node.(*ast.CodeSpan); ok {
			// openAt already rebuilt a span the excerpt also starts in.
			if start < el.cs {
				trail, to = x.codeSpan(el, el.cs, end), el.os
			}
			continue
		}
		trail += string(x.src[el.ce:el.oe])
	}
	return trail, to
}

// enclosing returns the inline elements with syntax in n that hold an offset,
// as holds tells, outermost first.
func (x *excerpter) enclosing(n ast.Node, holds func(span) bool) []element {
	var chain []element
	for c := n.FirstChild(); c != nil; {
		if sp, ok := x.span(c); ok && sp.os < sp.cs && holds(sp) {
			chain = append(chain, element{c, sp})
			c = c.FirstChild()
			continue
		}
		c = c.NextSibling()
	}
	return chain
}

// codeSpan rebuilds a code span around its content from s to e. The cut can
// leave a backtick or a space at an edge, where the backticks would absorb or
// strip it, so the content is padded then.
func (x *excerpter) codeSpan(el element, s, e int) string {
	var content []byte
	for c := el.node.FirstChild(); c != nil; c = c.NextSibling() {
		if t, ok := c.(*ast.Text); ok {
			if a, b := max(t.Segment.Start, s), min(t.Segment.Stop, e); a < b {
				content = append(content, x.src[a:b]...)
			}
		}
	}
	fence := strings.Repeat("`", x.runFrom(el.os, '`'))
	text := string(content)
	if strings.HasPrefix(text, "`") || strings.HasSuffix(text, "`") ||
		len(text) > 1 && text[0] == ' ' && text[len(text)-1] == ' ' && strings.Trim(text, " ") != "" {
		return fence + " " + text + " " + fence
	}
	return fence + text + fence
}

// containerMarkers returns the quote and list markers that put the line
// holding at back inside the containers around n, and the prefix a later line
// needs to stay inside them. The list item around n keeps its marker even on
// a line after its first: dropped, it would leave its text a paragraph that a
// later item such as "3." cannot interrupt. An outer item keeps its marker
// only when it begins on that line. Otherwise it is left out, and its
// indentation is taken off the lines after it, so a nested item does not
// turn into a code block.
func (x *excerpter) containerMarkers(n ast.Node, at int) (first, rest string, indent int) {
	line := lineStartBefore(x.src, at)
	var outer []ast.Node
	var item ast.Node
	for p := n.Parent(); p != nil; p = p.Parent() {
		outer = append(outer, p)
		if _, ok := p.(*ast.ListItem); ok && item == nil {
			item = p
		}
	}
	for i := len(outer) - 1; i >= 0; i-- {
		switch v := outer[i].(type) {
		case *ast.Blockquote:
			first += "> "
			rest += "> "
		case *ast.ListItem:
			switch {
			case lineStartBefore(x.src, v.Pos()) == line:
				first += itemMarker(v) + taskMarker(v)
			case v == item:
				first += itemMarker(v)
			default:
				indent += v.Offset
				continue
			}
			rest += strings.Repeat(" ", v.Offset)
		}
	}
	return first, rest, indent
}

// itemMarker returns a list item's bullet, or its number in an ordered list.
func itemMarker(item *ast.ListItem) string {
	list, ok := item.Parent().(*ast.List)
	if !ok {
		return "- "
	}
	if !list.IsOrdered() {
		return string(list.Marker) + " "
	}
	n := list.Start
	for s := item.PreviousSibling(); s != nil; s = s.PreviousSibling() {
		n++
	}
	return strconv.Itoa(n) + string(list.Marker) + " "
}

// taskMarker returns a task item's checkbox, which leads its first line.
func taskMarker(item *ast.ListItem) string {
	block := item.FirstChild()
	if block == nil {
		return ""
	}
	box, ok := block.FirstChild().(*extast.TaskCheckBox)
	switch {
	case !ok:
		return ""
	case box.IsChecked:
		return "[x] "
	}
	return "[ ] "
}

// leafMarker returns the syntax a leaf needs ahead of text cut from it. rest
// is the prefix that keeps a line after the first inside the leaf's
// containers.
func (x *excerpter) leafMarker(l leaf, rest string) string {
	switch v := l.node.(type) {
	case *ast.Heading:
		// A setext heading is marked by the underline below it, which the
		// selection holds; only an ATX heading's marker sits on its line.
		if bytes.IndexByte(x.src[lineStartBefore(x.src, l.s):l.s], '#') >= 0 {
			return strings.Repeat("#", v.Level) + " "
		}
	case *ast.FencedCodeBlock:
		// The fence has a line of its own, so the code after it needs the
		// container prefix again to stay in the quote or list item.
		return x.fenceLine(l) + "\n" + rest
	case *ast.CodeBlock:
		return "    "
	}
	return ""
}

// fenceLine returns a fenced code block's opening fence and info string,
// without the indentation or container markers ahead of them.
func (x *excerpter) fenceLine(l leaf) string {
	open := lineStartBefore(x.src, l.s) - 1
	if open < 0 {
		return "```"
	}
	line := string(x.src[lineStartBefore(x.src, open):open])
	if i := strings.IndexAny(line, "`~"); i >= 0 {
		return line[i:]
	}
	return "```"
}

// fenceCloser returns the fence that closes l: its opening fence's character,
// as many times, so a shorter fence inside stays content.
func (x *excerpter) fenceCloser(l leaf) string {
	line := x.fenceLine(l)
	return line[:len(line)-len(strings.TrimLeft(line, line[:1]))]
}

// codeIndent returns what sits ahead of the code on the line holding at in
// code block n: the container markers a closing fence needs too.
func (x *excerpter) codeIndent(n ast.Node, at int) string {
	lines := n.Lines()
	for i := 0; i < lines.Len(); i++ {
		if seg := lines.At(i); seg.Start <= at && at < seg.Stop {
			return string(x.src[lineStartBefore(x.src, seg.Start):seg.Start])
		}
	}
	return ""
}

// tableHead returns the header and delimiter rows a selection starting in a
// table's body needs to still read as a table, and where the row holding the
// cell l starts.
func (x *excerpter) tableHead(l leaf) (string, int) {
	row := l.node.Parent()
	from := lineStartBefore(x.src, l.s)
	if _, ok := row.(*extast.TableHeader); ok {
		return "", from
	}
	top := lineStartBefore(x.src, row.Parent().Pos())
	return string(x.src[top:x.lineEnd(x.lineEnd(top)+1)]) + "\n", from
}

// tableRowEnd returns where the row holding the cell l ends. A header row
// takes the delimiter row below it along, so it still reads as a table.
func (x *excerpter) tableRowEnd(l leaf) int {
	end := x.lineEnd(l.s)
	if _, ok := l.node.Parent().(*extast.TableHeader); ok {
		end = x.lineEnd(end + 1)
	}
	return end
}

func (x *excerpter) lineEnd(off int) int {
	off = min(off, len(x.src))
	if i := bytes.IndexByte(x.src[off:], '\n'); i >= 0 {
		return off + i
	}
	return len(x.src)
}

// lines returns the source of leaf n from start to end without the container
// syntax, quote markers and list indentation, that begins its later lines.
func (x *excerpter) lines(n ast.Node, start, end int) string {
	segs := n.Lines()
	if segs.Len() == 0 || start >= end {
		return string(x.src[start:max(start, end)])
	}
	var parts []string
	for i := 0; i < segs.Len(); i++ {
		seg := segs.At(i)
		if seg.Stop <= start || seg.Start >= end {
			continue
		}
		s, e := max(seg.Start, start), min(seg.Stop, end)
		part := string(x.src[s:e])
		if s == seg.Start && seg.Padding > 0 {
			part = strings.Repeat(" ", seg.Padding) + part
		}
		parts = append(parts, strings.TrimSuffix(part, "\n"))
	}
	return strings.Join(parts, "\n")
}

func isProse(n ast.Node) bool {
	switch n.(type) {
	case *ast.Paragraph, *ast.TextBlock, *ast.Heading, *extast.TableCell:
		return true
	}
	return false
}

func isCell(n ast.Node) bool {
	_, ok := n.(*extast.TableCell)
	return ok
}

func isFenced(n ast.Node) bool {
	_, ok := n.(*ast.FencedCodeBlock)
	return ok
}

// dedent removes up to n spaces from the start of every line after the first.
func dedent(s string, n int) string {
	if n == 0 {
		return s
	}
	lines := strings.Split(s, "\n")
	for i := 1; i < len(lines); i++ {
		spaces := len(lines[i]) - len(strings.TrimLeft(lines[i], " "))
		lines[i] = lines[i][min(n, spaces):]
	}
	return strings.Join(lines, "\n")
}

// escapeEdges escapes literal punctuation that a cut leaves at either end of
// text, which was cut from the source between from and to: the start in
// prose first, the end in prose last. Where it stood, a run of "*", "_", "~"
// or backticks could not open or close anything, but next to the cut or to a
// delimiter the excerpt adds it can: "_case_" cut from "snake_case_name"
// would turn to emphasis. A backslash left at the end would escape a closing
// delimiter added after it.
func (x *excerpter) escapeEdges(first, last ast.Node, text string, from, to int) string {
	const marks = "*_~`"
	lead := 0
	for lead < len(text) && text[lead] == text[0] && strings.IndexByte(marks, text[0]) >= 0 && x.literal(first, from+lead) {
		lead++
	}
	n := len(text)
	trail := 0
	for trail < n-lead && text[n-1-trail] == text[n-1] && strings.IndexByte(marks, text[n-1]) >= 0 && x.literal(last, to-1-trail) {
		trail++
	}
	escaped := func(s string) string {
		var b strings.Builder
		for i := range len(s) {
			b.WriteByte('\\')
			b.WriteByte(s[i])
		}
		return b.String()
	}
	text = escaped(text[:lead]) + text[lead:n-trail] + escaped(text[n-trail:])
	if trail == 0 && strings.HasSuffix(text, `\`) && x.literal(last, to-1) {
		text += `\`
	}
	return text
}

// literal reports whether the byte at off is plain text in prose n: neither
// syntax, nor escaped, nor code.
func (x *excerpter) literal(n ast.Node, off int) bool {
	if x.escapes(off - 1) {
		return false
	}
	found := false
	ast.Walk(n, func(c ast.Node, entering bool) (ast.WalkStatus, error) {
		switch v := c.(type) {
		case *ast.CodeSpan:
			return ast.WalkSkipChildren, nil
		case *ast.Text:
			if entering && v.Segment.Start <= off && off < v.Segment.Stop {
				found = true
				return ast.WalkStop, nil
			}
		}
		return ast.WalkContinue, nil
	})
	return found
}

// escapeLead escapes what would open a block at the start of an excerpt,
// though it was only text where it was cut from: the "#" of "# of items", a
// "-" or "1." followed by a space, a ">", a rule, a fence.
func escapeLead(s string) string {
	line, _, _ := strings.Cut(s, "\n")
	if line == "" {
		return s
	}
	spaced := func(i int) bool { return i == len(line) || line[i] == ' ' || line[i] == '\t' }
	hashes := len(line) - len(strings.TrimLeft(line, "#"))
	switch {
	case isRule(line),
		strings.HasPrefix(line, "```"), strings.HasPrefix(line, "~~~"),
		line[0] == '>',
		hashes > 0 && hashes <= 6 && spaced(hashes),
		strings.IndexByte("-+*", line[0]) >= 0 && spaced(1):
		return `\` + s
	}
	digits := len(line) - len(strings.TrimLeft(line, "0123456789"))
	if digits > 0 && digits <= 9 && digits < len(line) && (line[digits] == '.' || line[digits] == ')') && spaced(digits+1) {
		return s[:digits] + `\` + s[digits:]
	}
	return s
}

// isRule reports whether line is a thematic break: three or more of one of
// "-", "*", "_", with nothing else but spaces.
func isRule(line string) bool {
	marks := strings.NewReplacer(" ", "", "\t", "").Replace(line)
	return len(marks) >= 3 && strings.Trim(marks, marks[:1]) == "" && strings.IndexByte("-*_", marks[0]) >= 0
}
