package markdown

import (
	"strings"

	"github.com/charmbracelet/x/ansi"
	"github.com/yuin/goldmark/ast"
	extast "github.com/yuin/goldmark/extension/ast"
	"github.com/yuin/goldmark/util"
)

// piece is a run of text with one style, flowing through the span wrapper.
// StyleNone marks unstyled text, which produces no span. link, when set, is
// the hyperlink destination the run belongs to. breakBefore marks a run that
// may start a new line (a URL chunk boundary): it forces the pending word to
// commit first, so the wrapper can break between chunks without inserting a
// space.
type piece struct {
	text        string
	style       Style
	link        string
	breakBefore bool
}

// unescape resolves backslash escapes, numeric references, and HTML entities
// in a Text node's raw source bytes into plain text ("&amp;" -> "&",
// "\*" -> "*", NUL -> U+FFFD). The unescape chain is goldmark's own (the
// same three passes its URL escaper runs); the HTML writer's own Write()
// is not usable here because it re-escapes output for HTML destinations
// ("&" -> "&amp;"), which would leak entities back into terminal text.
func unescape(raw []byte) string {
	if len(raw) == 0 {
		return ""
	}
	if !strings.ContainsAny(string(raw), "&\\\x00") {
		return string(raw)
	}
	v := util.UnescapePunctuations(raw)
	v = util.ResolveNumericReferences(v)
	v = util.ResolveEntityNames(v)
	return string(v)
}

// inlinePieces walks an inline subtree collecting styled pieces. Soft line
// breaks become spaces, hard line breaks become "\n" (a real wrap break), raw
// HTML renders as its literal source text, and image nodes render their alt
// text. Every character emitted comes from a Text node's unescaped source or
// a node's resolved text, so the pieces never contain escape syntax.
func inlinePieces(n ast.Node, source []byte, theme Theme) []piece {
	return appendInlinePieces(n, source, theme, StyleNone)
}

func appendInlinePieces(n ast.Node, source []byte, theme Theme, style Style) []piece {
	var out []piece
	for c := n.FirstChild(); c != nil; c = c.NextSibling() {
		switch v := c.(type) {
		case *ast.Text:
			text := unescape(v.Text(source))
			switch {
			case v.HardLineBreak():
				text += "\n"
			case v.SoftLineBreak():
				text += " "
			}
			// A break may land on an empty text node (goldmark attaches it
			// after an inline node like a code span), so resolve the break
			// before the empty-text guard rather than dropping it.
			if text == "" {
				continue
			}
			out = append(out, piece{text: text, style: style})
		case *ast.String:
			// Synthetic text (e.g. from linkify); already resolved.
			out = append(out, piece{text: string(v.Value), style: style})
		case *ast.CodeSpan:
			// Code spans render their content verbatim (already space-
			// trimmed per CommonMark), never unescaped.
			out = append(out, piece{text: string(v.Text(source)), style: inner(theme, StyleCodeInline, style)})
		case *ast.Emphasis:
			s := StyleEmph
			if v.Level == 2 {
				s = StyleStrong
			}
			out = append(out, appendInlinePieces(c, source, theme, inner(theme, s, style))...)
		case *extast.Strikethrough:
			out = append(out, appendInlinePieces(c, source, theme, inner(theme, StyleStrikethrough, style))...)
		case *ast.Link:
			dest := string(v.Destination)
			out = append(out, linkPieces(c, source, theme, style, dest)...)
		case *ast.AutoLink:
			// Autolinks render the URL as its own label; the destination
			// equals the label, so only one run is emitted, made clickable.
			label := string(v.Label(source))
			out = append(out, piece{text: label, style: inner(theme, StyleLink, style), link: string(v.URL(source))})
		case *ast.Image:
			// Images render their alt text; the URL is terminal-invisible.
			out = append(out, appendInlinePieces(c, source, theme, inner(theme, StyleLink, style))...)
		case *ast.RawHTML:
			// Raw HTML renders as its literal source text rather than being
			// interpreted or dropped, so a tag the model wrote still shows.
			out = append(out, piece{text: string(v.Segments.Value(source)), style: style})
		default:
			out = append(out, appendInlinePieces(c, source, theme, style)...)
		}
	}
	return out
}

// linkPieces renders a link as its label (styled as a link and made
// clickable) followed by the destination in a faint URL style, unless the
// label already is the destination (in which case the label alone suffices).
// The destination is a leading-space-separated parenthesized run so it stays
// clickable and readable in terminals without hyperlink support.
func linkPieces(link ast.Node, source []byte, theme Theme, style Style, dest string) []piece {
	label := appendInlinePieces(link, source, theme, inner(theme, StyleLink, style))
	if dest != "" {
		for i := range label {
			label[i].link = dest
		}
	}
	if dest == "" || labelText(label) == dest {
		return label
	}
	url := piece{
		text:  " (" + dest + ")",
		style: inner(theme, StyleLinkURL, style),
		link:  dest,
	}
	return append(label, urlPieces(url, dest)...)
}

// urlPieces splits a displayed URL into chunks that may each start a new line,
// breaking after path and query punctuation. A long URL then wraps at
// readable boundaries instead of being hard-split mid-token, while the chunks
// concatenate to the exact original text. Chunks with no break points (a
// short URL) stay one piece.
func urlPieces(base piece, dest string) []piece {
	text := base.text
	// Break after these characters (they stay at the end of their chunk).
	const punct = "/-_.?&=#"
	var pieces []piece
	start := 0
	for i := 0; i < len(text); i++ {
		if strings.IndexByte(punct, text[i]) < 0 {
			continue
		}
		// Break after position i; skip a break that would leave an empty
		// chunk or split off a trailing empty tail.
		end := i + 1
		if end >= len(text) || end <= start {
			continue
		}
		pieces = append(pieces, piece{text: text[start:end], style: base.style, link: base.link})
		start = end
	}
	if start < len(text) {
		pieces = append(pieces, piece{text: text[start:], style: base.style, link: base.link})
	}
	// Mark every chunk after the first as a break point.
	for i := 1; i < len(pieces); i++ {
		pieces[i].breakBefore = true
	}
	return pieces
}

// labelText returns the visible text of a run of pieces.
func labelText(pieces []piece) string {
	var b strings.Builder
	for _, p := range pieces {
		b.WriteString(p.text)
	}
	return b.String()
}

// inner resolves a nested inline style against the theme, keeping the
// surrounding style when the theme maps the nested one to nothing.
func inner(theme Theme, style, fallback Style) Style {
	if resolved := theme.Resolve(style); resolved != style {
		return resolved
	}
	if style == StyleNone {
		return fallback
	}
	return style
}

// proseLines renders a paragraph or text block's inline content with spans.
func (r *blockRenderer) proseLines(n ast.Node, source []byte) []Line {
	return wrapPieces(inlinePieces(n, source, r.theme), r.width)
}

// pieceWidth returns the display width of a piece run.
func piecesWidth(pieces []piece) int {
	n := 0
	for _, p := range pieces {
		n += displayWidth(p.text)
	}
	return n
}

// spanWrapper word-wraps styled pieces to a column limit with the same greedy
// rule as a plain wrapper: words are kept whole, an over-long word is
// hard-split between grapheme clusters, and trailing whitespace never starts
// a line. Widths are computed from the piece runs on each decision, so the
// accounting cannot drift as pieces merge.
type spanWrapper struct {
	limit int

	lines [][]piece
	cur   []piece
	word  []piece
	space []piece
}

func newSpanWrapper(limit int) *spanWrapper { return &spanWrapper{limit: max(1, limit)} }

// appendCur appends a piece to the current line, merging into the previous
// piece when both carry the same style and link, so a styled run stays one
// span (and differently-linked runs never merge).
func (w *spanWrapper) appendCur(p piece) {
	if n := len(w.cur); n > 0 && w.cur[n-1].style == p.style && w.cur[n-1].link == p.link {
		w.cur[n-1].text += p.text
		return
	}
	w.cur = append(w.cur, p)
}

// addSpace commits pending spaces to the current line.
func (w *spanWrapper) addSpace() {
	for _, p := range w.space {
		w.appendCur(p)
	}
	w.space = w.space[:0]
}

// addWord commits the pending word (and the spaces before it) to the line.
func (w *spanWrapper) addWord() {
	if len(w.word) == 0 {
		return
	}
	w.addSpace()
	for _, p := range w.word {
		w.appendCur(p)
	}
	w.word = w.word[:0]
}

func (w *spanWrapper) addNewline() {
	w.lines = append(w.lines, w.cur)
	w.cur = nil
	w.space = w.space[:0]
}

// curWidth is the width of the committed current line.
func (w *spanWrapper) curWidth() int { return piecesWidth(w.cur) }

// Write feeds styled pieces; each piece may contain spaces, tabs, and
// newlines, which split it into word/space/break runs of one style.
func (w *spanWrapper) Write(pieces []piece) {
	for _, p := range pieces {
		w.writePiece(p)
	}
}

func (w *spanWrapper) writePiece(p piece) {
	if p.breakBefore && p.text != "" {
		// A break-marked run (a URL chunk) may start a new line. Commit the
		// pending word; if the chunk would not fit, break the line first
		// (dropping the held-back space, since the URL continues a link
		// rather than being a new word).
		w.addWord()
		if w.curWidth()+piecesWidth(w.space)+displayWidth(p.text) > w.limit && w.curWidth() > 0 {
			w.space = w.space[:0]
			w.addNewline()
		}
	}
	rest := p.text
	for rest != "" {
		if rest[0] == ' ' || rest[0] == '\t' {
			// A space ends the pending word, then starts a space run whose
			// width is held back until the next word commits.
			w.addWord()
			for rest != "" && (rest[0] == ' ' || rest[0] == '\t') {
				w.space = append(w.space, piece{text: " ", style: p.style, link: p.link})
				rest = rest[1:]
			}
			continue
		}
		if rest[0] == '\n' {
			w.addWord()
			w.addNewline()
			rest = rest[1:]
			continue
		}
		j := 0
		for j < len(rest) && rest[j] != ' ' && rest[j] != '\t' && rest[j] != '\n' {
			j++
		}
		w.writeWordRun(piece{text: rest[:j], style: p.style, link: p.link})
		rest = rest[j:]
	}
}

// writeWordRun appends a single-style word run to the pending word. If the
// pending word plus the held-back space no longer fits the line, the line
// breaks first (dropping the space, as a line-leading space would be noise);
// a word wider than the whole limit is hard-split straight away.
func (w *spanWrapper) writeWordRun(p piece) {
	if n := len(w.word); n > 0 && w.word[n-1].style == p.style && w.word[n-1].link == p.link {
		w.word[n-1].text += p.text
	} else {
		w.word = append(w.word, p)
	}
	wordW := piecesWidth(w.word)
	if wordW > w.limit {
		w.hardSplit()
		return
	}
	if w.curWidth()+piecesWidth(w.space)+wordW > w.limit {
		w.addNewline()
	}
}

// hardSplit breaks a word run wider than the limit into lines of grapheme
// clusters. The word is moved into the current line first, then clusters are
// emitted until the remainder fits; a cluster wider than an empty line is
// placed alone so the loop always progresses.
func (w *spanWrapper) hardSplit() {
	w.addSpace()
	for _, p := range w.word {
		w.appendCur(p)
	}
	w.word = w.word[:0]

	// Re-split the current line into whole lines. Walk from the last
	// committed line boundary: place clusters until the limit, then break.
	all := w.cur
	w.cur = nil
	for len(all) > 0 {
		space := w.limit - w.curWidth()
		if space <= 0 {
			w.addNewline()
			continue
		}
		p := all[0]
		cluster, width := ansi.FirstGraphemeCluster(p.text, ansi.GraphemeWidth)
		if len(cluster) == 0 {
			all = all[1:]
			continue
		}
		if width > space && w.curWidth() > 0 {
			w.addNewline()
			continue
		}
		w.appendCur(piece{text: cluster, style: p.style, link: p.link})
		all[0] = piece{text: p.text[len(cluster):], style: p.style, link: p.link}
		if all[0].text == "" {
			all = all[1:]
		}
		if piecesWidth(w.cur) >= w.limit {
			w.addNewline()
		}
	}
}

// Lines finishes wrapping and returns the display lines as piece slices.
// Append-only across Write calls.
func (w *spanWrapper) Lines() [][]piece {
	w.addWord()
	w.addNewline()
	// A trailing empty line from the final addNewline is dropped.
	if n := len(w.lines); n > 1 && len(w.lines[n-1]) == 0 {
		w.lines = w.lines[:n-1]
	}
	return w.lines
}

// spanLines converts wrapped pieces into display lines: the line text is the
// concatenation of piece texts, and spans carry each styled piece's text and
// target style. Unstyled lines carry no spans.
func spanLines(pieces [][]piece) []Line {
	var out []Line
	for _, line := range pieces {
		var text strings.Builder
		var spans []Styled
		for _, p := range line {
			text.WriteString(p.text)
			if p.style != StyleNone || p.link != "" {
				spans = append(spans, Styled{Text: p.text, Style: p.style, Link: p.link})
			}
		}
		out = append(out, Line{Text: text.String(), Spans: spans})
	}
	if len(out) == 0 {
		out = []Line{Plain("")}
	}
	return out
}

// wrapPieces wraps styled pieces to limit and returns display lines.
func wrapPieces(pieces []piece, limit int) []Line {
	w := newSpanWrapper(limit)
	w.Write(pieces)
	return spanLines(w.Lines())
}
