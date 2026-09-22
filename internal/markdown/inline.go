package markdown

import (
	"strings"

	"github.com/charmbracelet/x/ansi"
	"github.com/yuin/goldmark/ast"
	extast "github.com/yuin/goldmark/extension/ast"
	"github.com/yuin/goldmark/util"
)

// piece is a run of text with one style, flowing through the span wrapper.
type piece struct {
	text string
	attr string
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
// breaks become spaces, hard line breaks become "\n" (a real wrap break),
// raw HTML tags are dropped from the text, and image nodes render their alt
// text. Every character emitted comes from a Text node's unescaped source or
// a node's resolved text, so the pieces never contain escape syntax.
func inlinePieces(n ast.Node, source []byte, theme Theme) []piece {
	return appendInlinePieces(n, source, theme, "")
}

func appendInlinePieces(n ast.Node, source []byte, theme Theme, attr string) []piece {
	var out []piece
	for c := n.FirstChild(); c != nil; c = c.NextSibling() {
		switch v := c.(type) {
		case *ast.Text:
			text := unescape(v.Text(source))
			if text == "" {
				continue
			}
			switch {
			case v.HardLineBreak():
				text += "\n"
			case v.SoftLineBreak():
				text += " "
			}
			out = append(out, piece{text: text, attr: attr})
		case *ast.String:
			// Synthetic text (e.g. from linkify); already resolved.
			out = append(out, piece{text: string(v.Value), attr: attr})
		case *ast.CodeSpan:
			// Code spans render their content verbatim (already space-
			// trimmed per CommonMark), never unescaped.
			out = append(out, piece{text: string(v.Text(source)), attr: theme.inlineAttr(StyleCodeInline, attr)})
		case *ast.Emphasis:
			style := StyleEmph
			if v.Level == 2 {
				style = StyleStrong
			}
			out = append(out, appendInlinePieces(c, source, theme, theme.inlineAttr(style, attr))...)
		case *extast.Strikethrough:
			out = append(out, appendInlinePieces(c, source, theme, theme.inlineAttr(StyleStrikethrough, attr))...)
		case *ast.Link:
			out = append(out, appendInlinePieces(c, source, theme, theme.inlineAttr(StyleLink, attr))...)
		case *ast.AutoLink:
			// Autolinks render the URL as text; the destination equals the
			// label for these, so one styled run covers both.
			out = append(out, piece{text: string(v.Text(source)), attr: theme.inlineAttr(StyleLink, attr)})
		case *ast.Image:
			// Images render their alt text; the URL is terminal-invisible.
			out = append(out, appendInlinePieces(c, source, theme, theme.inlineAttr(StyleLink, attr))...)
		case *ast.RawHTML:
			// Tags are dropped from the text.
		default:
			out = append(out, appendInlinePieces(c, source, theme, attr)...)
		}
	}
	return out
}

// inlineAttr resolves a style with the theme, falling back to the
// surrounding attribute when the theme maps the style to nothing.
func (t Theme) inlineAttr(style Style, fallback string) string {
	if attr := t.Attr(style); attr != "" {
		return attr
	}
	return fallback
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
// piece when both carry the same attribute, so a styled run stays one span.
func (w *spanWrapper) appendCur(p piece) {
	if n := len(w.cur); n > 0 && w.cur[n-1].attr == p.attr {
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
	rest := p.text
	for rest != "" {
		if rest[0] == ' ' || rest[0] == '\t' {
			// A space ends the pending word, then starts a space run whose
			// width is held back until the next word commits.
			w.addWord()
			for rest != "" && (rest[0] == ' ' || rest[0] == '\t') {
				w.space = append(w.space, piece{text: " ", attr: p.attr})
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
		w.writeWordRun(piece{text: rest[:j], attr: p.attr})
		rest = rest[j:]
	}
}

// writeWordRun appends a single-style word run to the pending word. If the
// pending word plus the held-back space no longer fits the line, the line
// breaks first (dropping the space, as a line-leading space would be noise);
// a word wider than the whole limit is hard-split straight away.
func (w *spanWrapper) writeWordRun(p piece) {
	if n := len(w.word); n > 0 && w.word[n-1].attr == p.attr {
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
		w.appendCur(piece{text: cluster, attr: p.attr})
		all[0] = piece{text: p.text[len(cluster):], attr: p.attr}
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
// theme attribute. Unstyled lines carry no spans.
func spanLines(pieces [][]piece) []Line {
	var out []Line
	for _, line := range pieces {
		var text strings.Builder
		var spans []Styled
		for _, p := range line {
			text.WriteString(p.text)
			if p.attr != "" {
				spans = append(spans, Styled{Text: p.text, Attr: p.attr})
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
