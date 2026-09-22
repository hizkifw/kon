package ui

import (
	"image/color"
	"strings"

	"charm.land/lipgloss/v2"
	"github.com/charmbracelet/x/ansi"
	"github.com/hizkifw/kon/internal/markdown"
)

// markdownStyles maps markdown style roles onto the transcript palette. A
// role with no entry renders with the slab's foreground and no extra
// attribute.
func markdownStyles() map[markdown.Style]part {
	return map[markdown.Style]part{
		markdown.StyleHeading:       {fg: colorAgentFg, bold: true},
		markdown.StyleFaint:         {fg: colorFaint},
		markdown.StyleCodeBlock:     {fg: colorCodeFg},
		markdown.StyleCodeInline:    {fg: colorCodeFg},
		markdown.StyleQuote:         {fg: colorQuoteFg},
		markdown.StyleQuoteMark:     {fg: colorFaint},
		markdown.StyleListBullet:    {fg: colorToolName},
		markdown.StyleEmph:          {fg: colorAgentFg, italic: true},
		markdown.StyleStrong:        {fg: colorAgentFg, bold: true},
		markdown.StyleLink:          {fg: colorLink, underline: true},
		markdown.StyleLinkURL:       {fg: colorFaint, underline: true},
		markdown.StyleStrikethrough: {fg: colorFaint, strike: true},
		markdown.StyleTask:          {fg: colorOK},
	}
}

// markdownPalette is the resolved style table, built once.
var markdownPalette = markdownStyles()

// renderMarkdownBlock paints markdown lines into transcript display lines:
// every line is padded to the viewport width with the slab background, and
// each styled span is painted over it carrying the same background, so a
// span's resets cannot punch holes in the slab (see slabLine). Lines with no
// spans render as plain slab text.
func renderMarkdownBlock(lines []markdown.Line, bg, fg color.Color, width int) []string {
	styles := markdownPalette
	out := make([]string, 0, len(lines))
	for _, line := range lines {
		out = append(out, paintMarkdownLine(line, styles, bg, fg, width))
	}
	return out
}

// paintMarkdownLine renders one markdown line as a full-width slab row. The
// line's own text already carries its spacing, so segments are painted
// back-to-back with no injected separator (unlike slabLine, whose segments
// are distinct fields).
func paintMarkdownLine(line markdown.Line, styles map[markdown.Style]part, bg, fg color.Color, width int) string {
	var segments []part
	if len(line.Spans) == 0 {
		segments = []part{{text: line.Text, fg: fg}}
	} else {
		// Walk the spans, emitting any unstyled text between them as its own
		// segment so the piece order matches the line text.
		pos := 0
		ok := true
		for _, span := range line.Spans {
			idx := strings.Index(line.Text[pos:], span.Text)
			if idx < 0 {
				ok = false
				break
			}
			if idx > 0 {
				segments = append(segments, part{text: line.Text[pos : pos+idx], fg: fg})
			}
			p := styles[span.Style]
			if p.fg == nil {
				p.fg = fg
			}
			p.text = span.Text
			segments = append(segments, p)
			pos += idx + len(span.Text)
		}
		if !ok {
			segments = []part{{text: line.Text, fg: fg}}
		} else if pos < len(line.Text) {
			segments = append(segments, part{text: line.Text[pos:], fg: fg})
		}
	}
	return slabLineContinuous(bg, width, segments...)
}

// slabLineContinuous paints segments with no separator between them and the
// same full-width background fill as slabLine. Each segment carries the slab
// background so its resets cannot punch holes in the slab.
func slabLineContinuous(bg color.Color, width int, segments ...part) string {
	var out strings.Builder
	used := 1 // left padding cell
	out.WriteString(bgSpaces(bg, 1))
	for _, segment := range segments {
		if segment.text == "" {
			continue
		}
		avail := width - used - 1 // keep one cell of right padding
		if avail < 1 {
			break
		}
		text := segment.text
		if lipgloss.Width(text) > avail {
			text = ansi.Truncate(text, avail, "…")
		}
		style := lipgloss.NewStyle().Foreground(segment.fg).Background(bg)
		if segment.bold {
			style = style.Bold(true)
		}
		if segment.italic {
			style = style.Italic(true)
		}
		if segment.underline {
			style = style.Underline(true)
		}
		if segment.strike {
			style = style.Strikethrough(true)
		}
		out.WriteString(style.Render(text))
		used += lipgloss.Width(text)
	}
	out.WriteString(bgSpaces(bg, max(0, width-used)))
	return out.String()
}

// markdownLive renders an assistant message incrementally through the
// markdown package's streaming renderer. Frozen blocks are painted once into
// an append-only line cache; the open tail repaints per frame. Its output is
// byte-identical to renderMarkdownBlock(Render(text)) once the stream ends,
// which is what lets a finished message fold into the stable transcript
// without shifting (guarded by internal/ui tests).
type markdownLive struct {
	stream  *markdown.Stream
	bg, fg  color.Color
	width   int
	painted []string // painted frozen lines, append-only
}

func newMarkdownLive(bg, fg color.Color, width int) *markdownLive {
	return &markdownLive{
		stream: markdown.NewStream(markdown.Theme{}, width),
		bg:     bg,
		fg:     fg,
		width:  width,
	}
}

func (m *markdownLive) append(text string) { m.stream.Write(text) }

// finalized returns the painted frozen-block lines, append-only across calls.
func (m *markdownLive) finalized() []string {
	frozen := m.stream.Lines()
	for len(m.painted) < len(frozen) {
		lo := len(m.painted)
		m.painted = append(m.painted, renderMarkdownBlock(frozen[lo:lo+1], m.bg, m.fg, m.width)...)
	}
	return m.painted
}

// currentLines returns the painted open-tail lines, rebuilt each frame.
func (m *markdownLive) currentLines() []string {
	return renderMarkdownBlock(m.stream.Pending(), m.bg, m.fg, m.width)
}

// pending returns the whole live portion (frozen plus tail) as one string for
// the from-scratch reference path.
func (m *markdownLive) pending() string {
	out := append(append([]string{}, m.finalized()...), m.currentLines()...)
	return strings.Join(out, "\n")
}
