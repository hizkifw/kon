package web

import (
	"net/url"
	"strconv"
	"strings"
	"unicode"
	"unicode/utf8"

	"golang.org/x/net/html"
	"golang.org/x/net/html/atom"
)

// Markdown renders a parsed HTML document as Markdown for a model to read. It
// keeps the structure that carries meaning (headings, paragraphs, lists,
// links, code, quotes, and tables) and drops what renders no readable text,
// such as scripts, buttons, and decoration. base is the page's URL: a link to
// its site prints as a path from the site's root, and any other link as an
// absolute URL.
//
// The reader is a model, not a Markdown renderer, so text is not escaped: a
// stray asterisk costs less than a backslash before every one.
func Markdown(doc *html.Node, base *url.URL) string {
	c := converter{base: base}
	return strings.Join(c.blocks(contentRoot(doc)), "\n\n")
}

// contentRoot is the element holding the page's content: its <main>, or an
// element marked role="main", when it has one, and the whole document
// otherwise.
func contentRoot(doc *html.Node) *html.Node {
	for n := range doc.Descendants() {
		if n.Type == html.ElementNode && (n.DataAtom == atom.Main || attr(n, "role") == "main") {
			return n
		}
	}
	return doc
}

// inlineAtoms are the elements that flow within a line of text. Every other
// element, including custom elements, is a block, so a framework's wrapper
// element never flattens the headings and lists inside it into one paragraph.
var inlineAtoms = map[atom.Atom]bool{
	atom.A: true, atom.Abbr: true, atom.B: true, atom.Bdi: true, atom.Bdo: true,
	atom.Big: true, atom.Br: true, atom.Cite: true, atom.Code: true, atom.Data: true,
	atom.Del: true, atom.Dfn: true, atom.Em: true, atom.Font: true, atom.I: true,
	atom.Img: true, atom.Ins: true, atom.Kbd: true, atom.Label: true, atom.Mark: true,
	atom.Nobr: true, atom.Picture: true, atom.Q: true, atom.S: true, atom.Samp: true,
	atom.Small: true, atom.Span: true, atom.Strike: true, atom.Strong: true,
	atom.Sub: true, atom.Sup: true, atom.Time: true, atom.Tt: true, atom.U: true,
	atom.Var: true, atom.Wbr: true,
}

// dropped are elements whose content is never page text: code and media that
// render nothing readable, and form controls. Navigation, headers, and
// footers stay, since their links are how a reader finds the rest of a site;
// a page with a <main> leaves its site-wide ones outside it anyway.
var dropped = map[atom.Atom]bool{
	atom.Head: true, atom.Script: true, atom.Style: true, atom.Noscript: true,
	atom.Template: true, atom.Svg: true, atom.Canvas: true, atom.Iframe: true,
	atom.Object: true, atom.Video: true, atom.Audio: true, atom.Dialog: true,
	atom.Button: true, atom.Input: true, atom.Select: true, atom.Textarea: true,
}

// skip reports whether an element and everything inside it stays out of the
// output.
func skip(n *html.Node) bool {
	if n.Type != html.ElementNode {
		return n.Type != html.TextNode
	}
	return dropped[n.DataAtom] || hidden(n)
}

// hidden reports whether the page marks an element as decoration, such as an
// icon or a visual duplicate of nearby text. Content hidden only until a
// click stays: an inactive tab often holds the code sample a reader wants.
func hidden(n *html.Node) bool {
	return attr(n, "aria-hidden") == "true"
}

func isBlock(n *html.Node) bool {
	return n.Type == html.ElementNode && !inlineAtoms[n.DataAtom]
}

type converter struct {
	base *url.URL
}

// blocks renders n's children as Markdown blocks, which the caller separates.
// Inline content between block elements forms a paragraph.
func (c converter) blocks(n *html.Node) []string {
	var out []string
	var para line
	flush := func() {
		if text := para.String(); text != "" {
			out = append(out, text)
		}
		para = line{}
	}
	for child := range n.ChildNodes() {
		switch {
		case skip(child):
		case isBlock(child):
			flush()
			out = append(out, c.block(child)...)
		default:
			c.inline(child, &para)
		}
	}
	flush()
	return out
}

// block renders one block element.
func (c converter) block(n *html.Node) []string {
	switch n.DataAtom {
	case atom.H1, atom.H2, atom.H3, atom.H4, atom.H5, atom.H6:
		text := c.singleLine(n)
		if text == "" {
			return nil
		}
		level := int(n.Data[1] - '0')
		return []string{strings.Repeat("#", level) + " " + text}
	case atom.Pre:
		return fence(n)
	case atom.Blockquote:
		inner := strings.Join(c.blocks(n), "\n\n")
		if inner == "" {
			return nil
		}
		return []string{prefixLines(inner, "> ", "> ")}
	case atom.Ul, atom.Ol:
		return c.list(n)
	case atom.Table:
		return c.table(n)
	case atom.Hr:
		return []string{"---"}
	}
	return c.blocks(n)
}

// list renders each item under its marker, indenting the item's later lines
// so a nested list or second paragraph stays with its item.
func (c converter) list(n *html.Node) []string {
	number := 1
	if start, err := strconv.Atoi(attr(n, "start")); err == nil {
		number = start
	}
	var items []string
	for item := range n.ChildNodes() {
		if item.Type != html.ElementNode || skip(item) {
			continue
		}
		marker := "- "
		if n.DataAtom == atom.Ol {
			marker = strconv.Itoa(number) + ". "
			number++
		}
		if body := strings.Join(c.blocks(item), "\n"); body != "" {
			items = append(items, prefixLines(body, marker, strings.Repeat(" ", len(marker))))
		}
	}
	if len(items) == 0 {
		return nil
	}
	return []string{strings.Join(items, "\n")}
}

// table renders a data table as a Markdown table, taking the first row as its
// header because Markdown has no headerless table. A table that holds another
// table arranges the page rather than data, so its cells render as blocks.
func (c converter) table(n *html.Node) []string {
	var rows [][]string
	width := 0
	for row := range n.Descendants() {
		if row.DataAtom == atom.Table {
			return c.blocks(n)
		}
		if row.DataAtom != atom.Tr || skip(row) {
			continue
		}
		var cells []string
		for cell := range row.ChildNodes() {
			if (cell.DataAtom == atom.Td || cell.DataAtom == atom.Th) && !skip(cell) {
				cells = append(cells, strings.ReplaceAll(c.singleLine(cell), "|", `\|`))
			}
		}
		if len(cells) > 0 {
			rows = append(rows, cells)
			width = max(width, len(cells))
		}
	}
	if len(rows) == 0 {
		return nil
	}
	var b strings.Builder
	writeRow := func(cells []string) {
		b.WriteString("|")
		for i := range width {
			cell := ""
			if i < len(cells) {
				cell = cells[i]
			}
			b.WriteString(" " + cell + " |")
		}
	}
	writeRow(rows[0])
	b.WriteString("\n" + strings.Repeat("| --- ", width) + "|")
	for _, cells := range rows[1:] {
		b.WriteString("\n")
		writeRow(cells)
	}
	return []string{b.String()}
}

// inline renders n into the line being built. A block element met inside
// inline content, such as a heading inside a link, is set off by spaces.
func (c converter) inline(n *html.Node, out *line) {
	if skip(n) {
		return
	}
	if n.Type == html.TextNode {
		out.text(n.Data)
		return
	}
	switch n.DataAtom {
	case atom.Br:
		out.newline()
	case atom.Img:
		alt := strings.Join(strings.Fields(attr(n, "alt")), " ")
		if alt == "" {
			return
		}
		if src := c.resolve(attr(n, "src")); src != "" {
			alt = "![" + alt + "](" + src + ")"
		}
		out.word(alt)
	case atom.Code, atom.Kbd, atom.Samp, atom.Tt:
		// Code keeps its text as written; markup inside it is highlighting.
		var inner line
		inner.text(textContent(n))
		out.join(&inner, codeSpan)
	case atom.A:
		href := c.resolve(attr(n, "href"))
		out.join(c.span(n), func(text string) string {
			switch {
			case href == "":
				return text
			case text == href:
				return href
			}
			return "[" + text + "](" + href + ")"
		})
	case atom.Strong, atom.B:
		out.join(c.span(n), func(text string) string { return "**" + text + "**" })
	case atom.Em, atom.I:
		out.join(c.span(n), func(text string) string { return "*" + text + "*" })
	default:
		block := isBlock(n)
		if block {
			out.space = true
		}
		for child := range n.ChildNodes() {
			c.inline(child, out)
		}
		if block {
			out.space = true
		}
	}
}

// span renders n's children into a line of their own, for a caller that
// wraps them in markup.
func (c converter) span(n *html.Node) *line {
	inner := &line{}
	for child := range n.ChildNodes() {
		c.inline(child, inner)
	}
	return inner
}

// singleLine renders n's content as one line, for headings and table cells.
func (c converter) singleLine(n *html.Node) string {
	return strings.ReplaceAll(c.span(n).String(), "\n", " ")
}

// resolve returns where href leads: a path from the root for a link within
// the page's site, and an absolute http, https, or mailto URL for any other.
// The reader knows which site it fetched, and a menu of full URLs would
// repeat its name on every line. A link that leads nowhere a reader could
// follow, an anchor on this page, a script, or inline data, resolves to "".
func (c converter) resolve(href string) string {
	href = strings.TrimSpace(href)
	if href == "" || strings.HasPrefix(href, "#") {
		return ""
	}
	u, err := c.base.Parse(href)
	if err != nil {
		return ""
	}
	switch u.Scheme {
	case "http", "https", "mailto":
	default:
		return ""
	}
	if u.Scheme == c.base.Scheme && u.Host == c.base.Host && u.User == nil {
		path := *u
		path.Scheme, path.Host = "", ""
		if path.Path == "" {
			path.Path = "/"
		}
		return path.String()
	}
	return u.String()
}

// fence renders preformatted text as a fenced code block, naming its language
// when the page's highlighting class does.
func fence(n *html.Node) []string {
	text := strings.Trim(textContent(n), "\n")
	if strings.TrimSpace(text) == "" {
		return nil
	}
	marker := "```"
	for strings.Contains(text, marker) {
		marker += "`"
	}
	return []string{marker + language(n) + "\n" + text + "\n" + marker}
}

// language reads a language-* or lang-* class from a <pre> or the <code>
// inside it, the convention highlighters share.
func language(pre *html.Node) string {
	if name := classLanguage(pre); name != "" {
		return name
	}
	for n := range pre.Descendants() {
		if n.DataAtom == atom.Code {
			return classLanguage(n)
		}
	}
	return ""
}

func classLanguage(n *html.Node) string {
	for _, class := range strings.Fields(attr(n, "class")) {
		for _, prefix := range []string{"language-", "lang-"} {
			if name, ok := strings.CutPrefix(class, prefix); ok && name != "" {
				return name
			}
		}
	}
	return ""
}

// codeSpan wraps text in enough backticks that none inside ends it early.
func codeSpan(text string) string {
	if text == "" {
		return ""
	}
	ticks := "`"
	for strings.Contains(text, ticks) {
		ticks += "`"
	}
	if len(ticks) > 1 {
		return ticks + " " + text + " " + ticks
	}
	return ticks + text + ticks
}

// textContent is the text inside n exactly as written, with line breaks for
// <br>, as preformatted text needs it.
func textContent(n *html.Node) string {
	var b strings.Builder
	for d := range n.Descendants() {
		switch {
		case d.Type == html.TextNode:
			b.WriteString(d.Data)
		case d.DataAtom == atom.Br:
			b.WriteString("\n")
		}
	}
	return b.String()
}

func attr(n *html.Node, key string) string {
	for _, a := range n.Attr {
		if a.Key == key {
			return a.Val
		}
	}
	return ""
}

// prefixLines puts first before the first line of text and rest before each
// later one, leaving no trailing space on an empty line.
func prefixLines(text, first, rest string) string {
	lines := strings.Split(text, "\n")
	for i, l := range lines {
		prefix := rest
		if i == 0 {
			prefix = first
		}
		if l == "" {
			prefix = strings.TrimRight(prefix, " ")
		}
		lines[i] = prefix + l
	}
	return strings.Join(lines, "\n")
}

// line builds inline text with whitespace collapsed as a browser would: a
// run of spaces and newlines in the source becomes one space, and no line
// starts or ends with one.
type line struct {
	b strings.Builder
	// space is a collapsed space waiting for the next word.
	space bool
	// lead is the space the line started with, which the empty line itself
	// cannot show; wrapping markup keeps it outside, as in "a **b**".
	lead bool
}

// text writes source text, collapsing its whitespace.
func (l *line) text(s string) {
	if s == "" {
		return
	}
	if r, _ := utf8.DecodeRuneInString(s); unicode.IsSpace(r) {
		l.space = true
	}
	for i, word := range strings.Fields(s) {
		if i > 0 {
			l.space = true
		}
		l.word(word)
	}
	if r, _ := utf8.DecodeLastRuneInString(s); unicode.IsSpace(r) {
		l.space = true
	}
}

// word writes s as is, after the pending space if there is one.
func (l *line) word(s string) {
	if s == "" {
		return
	}
	if l.b.Len() == 0 {
		l.lead = l.lead || l.space
	} else if l.space && !strings.HasSuffix(l.b.String(), "\n") {
		l.b.WriteByte(' ')
	}
	l.space = false
	l.b.WriteString(s)
}

// newline writes a line break, dropping any space pending before it.
func (l *line) newline() {
	if l.b.Len() > 0 {
		l.b.WriteByte('\n')
	}
	l.space = false
}

// join writes a separately built line as one word marked up by wrap. Its
// leading and trailing spaces stay outside the markup, as in "a **b** c",
// and empty text writes nothing rather than a bare "****" or "[]()".
func (l *line) join(inner *line, wrap func(string) string) {
	if inner.lead {
		l.space = true
	}
	if text := inner.String(); text != "" {
		l.word(wrap(text))
	}
	if inner.space {
		l.space = true
	}
}

func (l *line) String() string {
	return strings.TrimSpace(l.b.String())
}
