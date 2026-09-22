package markdown

import (
	"bytes"
	"fmt"
	"strings"

	"github.com/yuin/goldmark/ast"
	extast "github.com/yuin/goldmark/extension/ast"
	"github.com/yuin/goldmark/text"
)

// block is one closed markdown block: its display lines and the byte offset
// just past it in the full document.
type block struct {
	lines []Line
	end   int // offset just past the block in the document
}

// blockRenderer turns goldmark block nodes into wrapped display lines.
type blockRenderer struct {
	theme Theme
	width int
	buf   []byte
}

func newBlockRenderer(theme Theme, width int) *blockRenderer {
	return &blockRenderer{theme: theme, width: width}
}

// subtreeStop returns the largest segment stop in the node's subtree, or -1
// when the subtree carries no segments (e.g. ThematicBreak, List).
func subtreeStop(n ast.Node) int {
	best := -1
	var walk func(n ast.Node)
	walk = func(n ast.Node) {
		if n.Type() == ast.TypeBlock {
			if segs := n.Lines(); segs.Len() > 0 {
				if s := segs.At(segs.Len() - 1).Stop; s > best {
					best = s
				}
			}
		}
		for c := n.FirstChild(); c != nil; c = c.NextSibling() {
			walk(c)
		}
	}
	walk(n)
	return best
}

// subtreeStart returns the smallest segment start in the node's subtree, or -1
// when the subtree carries no segments.
func subtreeStart(n ast.Node) int {
	best := -1
	var walk func(n ast.Node)
	walk = func(n ast.Node) {
		if n.Type() == ast.TypeBlock {
			if segs := n.Lines(); segs.Len() > 0 {
				if s := segs.At(0).Start; best < 0 || s < best {
					best = s
				}
			}
		}
		for c := n.FirstChild(); c != nil; c = c.NextSibling() {
			walk(c)
		}
	}
	walk(n)
	return best
}

// lineStartBefore returns the offset of the line start at or before off, so a
// freeze boundary never lands mid-line. A subtree's first content segment can
// sit after a prefix like "> " or "- "; resuming the parse from that content
// offset would lose the marker and re-parse a different tree (a blockquote
// would come back as a plain paragraph). Boundaries must point at line starts.
func lineStartBefore(source []byte, off int) int {
	if off > len(source) {
		off = len(source)
	}
	if i := bytes.LastIndexByte(source[:off], '\n'); i >= 0 {
		return i + 1
	}
	return 0
}

// fenceCloserLine reports whether line is a fenced-code closer: a run of at
// least three backticks or tildes (of one kind) with optional trailing
// whitespace. The line must already have leading spaces trimmed.
func fenceCloserLine(line []byte) bool {
	i := 0
	for i < len(line) && (line[i] == '`' || line[i] == '~') && i < 3 {
		if line[i] != line[0] {
			return false
		}
		i++
	}
	if i < 3 {
		return false
	}
	for ; i < len(line); i++ {
		if line[i] != ' ' && line[i] != '\t' && line[i] != '\n' && line[i] != '\r' {
			return false
		}
	}
	return true
}

// opensNewBlock reports whether the line at off begins with a marker that
// cannot be swallowed by a container spanning the boundary: ATX heading,
// thematic break, fence, or blockquote marker. List markers and indented
// text are deliberately excluded — they can continue a list (or indented
// code) from *before* the boundary, so a boundary followed by such a line
// must stay put (see blankSafeBoundary).
func opensNewBlock(line []byte) bool {
	i := 0
	for i < 3 && i < len(line) && line[i] == ' ' {
		i++
	}
	if i >= len(line) {
		return false
	}
	switch line[i] {
	case '#', '>':
		return true
	case '`', '~':
		// A fence opener is a run of at least three matching marker chars.
		// A shorter run ("``g") is paragraph text that may still grow into
		// an opener, so it must not close anything yet.
		n := 0
		for j := i; j < len(line) && line[j] == line[i]; j++ {
			n++
		}
		return n >= 3
	case '-', '*', '_':
		// Distinguish thematic break ("- - -", "***", "___") from a list
		// marker: a thematic break is three or more of the same char with
		// only spaces between. List items cannot close a preceding block.
		n := 0
		for j := i; j < len(line); j++ {
			if line[j] == line[i] {
				n++
			} else if line[j] != ' ' && line[j] != '\t' {
				return false // list item text
			}
		}
		return n >= 3
	}
	return false
}

// blankSafeBoundary reports whether the blank line at off (source[off] is
// inside a blank run) is a safe freeze boundary after a block of kind prev:
// the next non-blank line must not be able to continue that block. Lists
// merge across blanks when a same-marker item follows, and indented code
// merges with a following indented line — freezing either would re-parse the
// tail into a different shape (a tight list losing its loose blank). Hard
// openers (heading, rule, fence, quote marker) can never continue an
// earlier block, so they close it safely.
func blankSafeBoundary(source []byte, off int, prev ast.NodeKind) bool {
	i := off
	for i < len(source) {
		j := i
		for j < len(source) && (source[j] == ' ' || source[j] == '\t') {
			j++
		}
		if j >= len(source) {
			return false // EOF in a blank line: tail still open
		}
		if source[j] == '\n' {
			i = j + 1
			continue
		}
		line := source[j:]
		if opensNewBlock(line) {
			return true
		}
		switch prev {
		case ast.KindList:
			// A list marker, a marker prefix (a bare digit run or bullet
			// char the next delta can complete), or an indented line may
			// continue the list; unindented prose cannot.
			return !listMarkerAt(line) && !listMarkerPrefixAt(line) && !indentedLine(line)
		case ast.KindCodeBlock:
			// An indented line continues indented code; nothing else can.
			return !indentedLine(line)
		default:
			// Paragraphs, quotes, fences, headings, HTML: nothing crosses
			// a blank line into them.
			return true
		}
	}
	return false
}

// listMarkerAt reports whether the line begins with a bullet or ordered
// list marker (up to three leading spaces).
func listMarkerAt(line []byte) bool {
	i := 0
	for i < 3 && i < len(line) && line[i] == ' ' {
		i++
	}
	if i >= len(line) {
		return false
	}
	switch line[i] {
	case '-', '*', '+':
		return i+1 >= len(line) || line[i+1] == ' ' || line[i+1] == '\t' || line[i+1] == '\n'
	case '1', '2', '3', '4', '5', '6', '7', '8', '9':
		j := i
		for j < len(line) && line[j] >= '0' && line[j] <= '9' {
			j++
		}
		return j > i && j < len(line) && (line[j] == '.' || line[j] == ')')
	}
	return false
}

// listMarkerPrefixAt reports whether the line could still become a list
// marker: it starts (after spaces) with a bullet char or a digit run that
// has not yet grown a delimiter and content. A stream stopping mid-marker
// ("1", "12", "1.") leaves exactly this shape, and the next delta can
// complete it into an item that continues the list — so a boundary followed
// by such a line must stay put.
func listMarkerPrefixAt(line []byte) bool {
	i := 0
	for i < 3 && i < len(line) && line[i] == ' ' {
		i++
	}
	if i >= len(line) {
		return false
	}
	switch line[i] {
	case '-', '*', '+':
		// A bare bullet char may grow "- text"; a marker with anything
		// after it is complete (listMarkerAt already covers that).
		return i+1 >= len(line)
	case '1', '2', '3', '4', '5', '6', '7', '8', '9':
		// Any digit run without a following delimiter is a marker prefix:
		// "1", "12", "1." all qualify ("1." may still grow " y" and the
		// delimiter case is covered by listMarkerAt, but treating it as a
		// prefix too is harmless — same refusal).
		return true
	}
	return false
}

// indentedLine reports whether the line starts with four spaces or a tab
// (indented-code territory).
func indentedLine(line []byte) bool {
	n := 0
	for _, b := range line {
		if b == '\t' {
			return true
		}
		if b != ' ' {
			break
		}
		n++
		if n >= 4 {
			return true
		}
	}
	return false
}

// renderClosed walks node's children and renders the provably closed ones.
//
// A block is closed when a later sibling block begins after it (the next
// known segment start marks the boundary past the blank separator), or when it
// is the final block and the source ends with a blank line. Headings and
// thematic breaks close after a single trailing newline: no following line can
// join them. Everything else stays open at EOF so lazy continuations, setext
// underlines, list items, and fence closings can still arrive in later deltas.
//
// This is the equivalent of omp's stableBlockBoundary: the freeze boundary
// only ever sits where a blank line (or a structurally closed line) ends.
func (r *blockRenderer) renderClosed(node ast.Node, source []byte) []block {
	return r.renderChildren(node, source, true)
}

// renderAll renders every child block, including a trailing open one. Used by
// the from-scratch renderer and the live tail, where the open block's current
// lines must be visible (and must match what freezing it later produces).
func (r *blockRenderer) renderAll(node ast.Node, source []byte) []block {
	return r.renderChildren(node, source, false)
}

func (r *blockRenderer) renderChildren(node ast.Node, source []byte, freezeOnly bool) []block {
	var children []ast.Node
	for c := node.FirstChild(); c != nil; c = c.NextSibling() {
		children = append(children, c)
	}
	if len(children) == 0 {
		return nil
	}

	// starts[i] is the first source offset of child i's subtree (-1: none).
	starts := make([]int, len(children))
	for i, c := range children {
		starts[i] = subtreeStart(c)
	}

	blocks := make([]block, 0, len(children))
	for i, c := range children {
		var end int
		closed := false
		switch {
		case i+1 < len(children) && starts[i+1] >= 0:
			// The next sibling's first segment marks the boundary past this
			// block's trailing blank line, snapped to a line start. (An
			// unclosed fence consumes all remaining lines, so any following
			// sibling implies this block is closed.)
			nextStart := starts[i+1]
			if children[i+1].Kind() == ast.KindFencedCodeBlock && nextStart > 0 {
				// A fence's opener line (```lang) sits above its first
				// content line; the boundary must include that opener in the
				// tail so the fence re-opens on re-parse. Every other block
				// kind's first segment shares the line its syntax starts on
				// ("> text", "- text", plain prose).
				nextStart = lineStartBefore(source, nextStart-1)
			}
			end, closed = lineStartBefore(source, nextStart), true
			// The boundary must sit directly after this block's own source
			// plus one blank separator. A larger gap means the next
			// sibling's segment start hides earlier opener lines — e.g.
			// "- \n\n  - a" is one list whose empty first item "- " has no
			// content segment, so the list's real start is its marker line,
			// not "a". Freezing past those lines would orphan them into the
			// tail, where they re-parse as a different tree.
			if stop := subtreeStop(c); stop >= 0 {
				lastLine := lineStartBefore(source, stop)
				if end > lastLine {
					blankLines := bytes.Count(source[lastLine:end], []byte("\n")) - 1
					if blankLines > 1 {
						end, closed = 0, false
					}
				}
			}
			// What follows must also be unable to continue a container
			// across the boundary (a same-marker list item would flip
			// tight→loose; an indented line may continue indented code).
			if closed && !blankSafeBoundary(source, end, c.Kind()) {
				end, closed = 0, false
			}
		case i+1 == len(children):
			// Last child: closed only when the source is blank-line
			// terminated, or the block is structurally line-closed.
			switch {
			case c.Kind() == ast.KindHeading, c.Kind() == ast.KindThematicBreak:
				closed = len(source) > 0 && source[len(source)-1] == '\n'
			case c.Kind() == ast.KindFencedCodeBlock:
				// A fence continues across blank lines until its closing
				// marker, so a blank line does not close it. goldmark's
				// segments never include the closer line, so a fence whose
				// closer has arrived still shows stop < len(source); test
				// for the closer directly: the line at the last content
				// stop must be a complete fence-marker run of 3+ matching
				// chars (a partial closer "``g" is not — it may still be
				// paragraph text). Freezing at stop alone would orphan the
				// closer into the tail, where it re-opens a phantom fence
				// that swallows everything after it. Without the trailing
				// newline the closer line itself may still grow ("``" →
				// "```"), so the fence stays open until that newline too.
				if stop := subtreeStop(c); stop >= 0 {
					closerAt := lineStartBefore(source, stop)
					if closerAt > stop || bytes.IndexByte(source[closerAt:], '\n') >= 0 {
						closerLine := bytes.TrimLeft(source[closerAt:], " ")
						if len(closerLine) > 0 && fenceCloserLine(closerLine) {
							nl := bytes.IndexByte(source[closerAt:], '\n')
							if nl >= 0 {
								closed = true
								end = closerAt + nl + 1
							}
						}
					}
				}
			case c.Kind() == ast.KindHTMLBlock:
				// An HTML block freezes only with a closure line: type-1
				// blocks (script, pre, style, textarea) span blank lines
				// until their closing tag, and goldmark exports no type
				// discriminator, so unclosed blocks stay open regardless
				// of kind. Closing on a blank would split a spanning
				// script block and re-parse its tail as markdown.
				if h, ok := c.(*ast.HTMLBlock); ok && h.HasClosure() {
					closed = true
					end = lineStartBefore(source, h.ClosureLine.Stop)
				}
			default:
				// A blank line closes a block only if what follows cannot
				// continue it: lists and indented code merge across blanks
				// (a later same-marker item would flip tight→loose), so the
				// next non-blank line must be unable to continue this kind.
				closed = bytes.HasSuffix(source, []byte("\n\n")) &&
					blankSafeBoundary(source, len(source), c.Kind())
			}
			if closed && end == 0 {
				end = len(source)
			}
			if !closed && !freezeOnly {
				end = len(source)
			}
		default:
			// The next sibling exists but its start is unknown (e.g. a fence
			// that just opened and has no content segments yet). This block
			// cannot be placed: freezing at len(source) would swallow the
			// sibling's source range.
			closed = false
		}
		if !closed {
			if freezeOnly {
				break // nothing past an open block can be frozen
			}
			end = len(source)
		}
		blocks = append(blocks, r.renderBlock(c, source, end))
	}
	return blocks
}

func (r *blockRenderer) renderBlock(n ast.Node, source []byte, end int) block {
	b := block{end: end}
	switch n.Kind() {
	case ast.KindHeading:
		b.lines = r.headingLines(n.(*ast.Heading), source)
	case ast.KindThematicBreak:
		b.lines = []Line{Plain(ruleLine(r.width))}
	case ast.KindFencedCodeBlock, ast.KindCodeBlock, ast.KindHTMLBlock:
		b.lines = r.codeLines(n.Lines(), source)
	case ast.KindBlockquote:
		b.lines = quoteLines(r.renderAll(n, source), r.theme)
	case ast.KindList:
		b.lines = r.listLines(n.(*ast.List), source)
	case extast.KindTable:
		b.lines = r.tableLines(n, source)
	case ast.KindParagraph, ast.KindTextBlock:
		b.lines = r.proseLines(n, source)
	default:
		b.lines = []Line{Plain("")}
	}
	b.lines = trimBlankEdges(b.lines)
	return b
}

// trimBlankEdges drops blank lines from the start and end of a block's lines.
// Blank separators between blocks belong to the joiner (kon's transcript
// joins slabs with a blank line), never to the block itself. Interior blanks
// (fence content, loose-list item gaps) are content and are kept. This
// canonicalization is also what makes streaming and from-scratch renders
// converge: the frozen source range of a block can end just before or just
// after its trailing blank line depending on when it froze, and the trim
// erases exactly that difference.
func trimBlankEdges(lines []Line) []Line {
	start, end := 0, len(lines)
	for start < end && strings.TrimSpace(lines[start].Text) == "" {
		start++
	}
	for end > start && strings.TrimSpace(lines[end-1].Text) == "" {
		end--
	}
	return lines[start:end]
}

// tableLines renders a GFM table as "cell cell cell" rows. The header row is
// distinguished by a heading-style span so callers can style it apart. Cell
// content walks the same inline path as prose, so inline styles work in
// cells and text is unescaped identically.
func (r *blockRenderer) tableLines(n ast.Node, source []byte) []Line {
	var out []Line
	for row := n.FirstChild(); row != nil; row = row.NextSibling() {
		var cells []string
		isHeader := row.Kind() == extast.KindTableHeader
		for cell := row.FirstChild(); cell != nil; cell = cell.NextSibling() {
			var b strings.Builder
			for _, p := range inlinePieces(cell, source, r.theme) {
				b.WriteString(p.text)
			}
			cells = append(cells, strings.TrimSpace(b.String()))
		}
		text := strings.Join(cells, "  ")
		line := Plain(text)
		if isHeader {
			line = Line{Text: text, Spans: []Styled{{text, r.theme.Attr(StyleHeading)}}}
		}
		out = append(out, line)
	}
	return out
}

// headingLines renders a heading's inline content with heading-style spans.
func (r *blockRenderer) headingLines(h *ast.Heading, source []byte) []Line {
	lines := wrapPieces(inlinePieces(h, source, r.theme), r.width)
	if len(lines) == 0 {
		lines = []Line{Plain("")}
	}
	// Re-style the wrapped lines as headings: wrapPieces returns text
	// spans from the inline walk, so replace the attribute in place.
	return lines
}

// proseLines renders a paragraph or text block's inline content with spans.
// Implemented in inline.go (wrapPieces over the inline walk).

func (r *blockRenderer) codeLines(segments *text.Segments, source []byte) []Line {
	var out []Line
	for i := 0; i < segments.Len(); i++ {
		seg := segments.At(i)
		r.buf = append(r.buf[:0], source[seg.Start:seg.Stop]...)
		if seg.Padding > 0 {
			r.buf = append(bytes.Repeat([]byte{' '}, seg.Padding), r.buf...)
		}
		content := strings.TrimSuffix(string(r.buf), "\n")
		for _, raw := range strings.Split(content, "\n") {
			out = append(out, Line{Text: raw, Spans: []Styled{{raw, r.theme.Attr(StyleCodeBlock)}}})
		}
	}
	return out
}

// listLines renders a list. Loose lists (blank line between items) put a blank
// line between items; tight lists do not. Item content recurses through
// renderBlocks so nested structures indent naturally.
func (r *blockRenderer) listLines(l *ast.List, source []byte) []Line {
	loose := !l.IsTight
	var out []Line
	number := l.Start
	for item := l.FirstChild(); item != nil; item = item.NextSibling() {
		marker := bulletMarker(l, number)
		number++
		indent := strings.Repeat(" ", len(marker)+1)
		// A task checkbox is the first inline child of the item's first
		// content block (TextBlock/Paragraph); it renders as a ☑/☐ marker
		// on the first content line.
		checkbox := ""
		for sub := item.FirstChild(); sub != nil; sub = sub.NextSibling() {
			if cb, ok := sub.(*extast.TaskCheckBox); ok {
				if cb.IsChecked {
					checkbox = "☑ "
				} else {
					checkbox = "☐ "
				}
				continue
			}
			if inner, ok := sub.FirstChild().(*extast.TaskCheckBox); ok && inner != nil {
				if inner.IsChecked {
					checkbox = "☑ "
				} else {
					checkbox = "☐ "
				}
			}
			break
		}
		first := true
		for sub := item.FirstChild(); sub != nil; sub = sub.NextSibling() {
			if _, ok := sub.(*extast.TaskCheckBox); ok {
				continue
			}
			// The checkbox inside the content block's inline list is
			// skipped by inlineText automatically? No: TaskCheckBox has no
			// text; the marker line is added here instead.
			if sub.Kind() == ast.KindList {
				// Nested list: indent under the parent marker column. The
				// nested renderer already emits its own bullets two cells
				// in, so only two more cells are added per level.
				for _, nl := range r.renderBlock(sub, source, 0).lines {
					out = append(out, Line{Text: "  " + nl.Text})
				}
				first = false
				continue
			}
			for _, line := range r.renderBlock(sub, source, 0).lines {
				text := line.Text
				spans := cloneSpans(line.Spans)
				if first && checkbox != "" {
					text = checkbox + text
					if checkAttr := r.theme.Attr(StyleTask); checkAttr != "" {
						spans = append([]Styled{{Text: checkbox, Attr: checkAttr}}, spans...)
					}
				}
				if first {
					out = append(out, Line{
						Text:  marker + " " + text,
						Spans: append([]Styled{{marker, r.theme.Attr(StyleListBullet)}}, spans...),
					})
					first = false
					continue
				}
				out = append(out, Line{
					Text:  indent + text,
					Spans: spans,
				})
			}
		}
		if loose && item.NextSibling() != nil {
			out = append(out, Plain(""))
		}
	}
	return out
}

func bulletMarker(l *ast.List, number int) string {
	if l.IsOrdered() {
		return fmt.Sprintf("%d.", number)
	}
	return "•"
}

func cloneSpans(spans []Styled) []Styled {
	if len(spans) == 0 {
		return nil
	}
	return append([]Styled(nil), spans...)
}

func quoteLines(inner []block, theme Theme) []Line {
	var out []Line
	for _, blk := range inner {
		for _, line := range blk.lines {
			bar := "▏"
			spans := append([]Styled{{bar, theme.Attr(StyleQuoteMark)}}, cloneSpans(line.Spans)...)
			out = append(out, Line{Text: bar + " " + line.Text, Spans: spans})
		}
	}
	return out
}

// ruleLine builds a horizontal rule across width.
func ruleLine(width int) string {
	if width < 1 {
		width = 1
	}
	return strings.Repeat("─", width)
}
