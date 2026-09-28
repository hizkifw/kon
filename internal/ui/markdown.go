package ui

import (
	"image/color"
	"io"
	"strings"
	"unicode"

	"charm.land/lipgloss/v2"
	"github.com/charmbracelet/x/ansi"
	"github.com/hizkifw/kon/internal/markdown"
)

// markdownStyles maps markdown style roles onto the transcript palette. A
// role with no entry renders with the slab's foreground and no extra
// attribute, and a role with no foreground, like emphasis, in the slab's
// foreground with its attributes.
func markdownStyles() map[markdown.Style]part {
	return map[markdown.Style]part{
		markdown.StyleHeading:       {fg: colorHeadingFg, bold: true},
		markdown.StyleFaint:         {fg: colorFaint},
		markdown.StyleCodeBlock:     {fg: colorCodeFg},
		markdown.StyleCodeInline:    {fg: colorCodeFg},
		markdown.StyleQuote:         {fg: colorQuoteFg},
		markdown.StyleQuoteMark:     {fg: colorFaint},
		markdown.StyleListBullet:    {fg: colorToolName},
		markdown.StyleEmph:          {italic: true},
		markdown.StyleStrong:        {bold: true},
		markdown.StyleLink:          {fg: colorLink},
		markdown.StyleLinkURL:       {fg: colorFaint},
		markdown.StyleStrikethrough: {fg: colorFaint, strike: true},
		markdown.StyleTask:          {fg: colorOK},
		markdown.StyleTableHeader:   {bg: colorTableHeaderBg, bold: true},
		markdown.StyleTableRowAlt:   {bg: colorTableRowBg},
	}
}

// markdownPalette is the resolved style table, built once.
var markdownPalette = markdownStyles()

// markdownContentWidth is the text width available inside a slab: the slab
// paints one cell of left padding and one of right padding, so markdown must
// wrap to two cells less than the viewport. Wrapping to the full width would
// make every maxed-out line overflow and get truncated with an ellipsis.
func markdownContentWidth(width int) int { return max(1, width-2) }

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
	segments := markdownSegments(line, styles, fg)
	return slabLineContinuous(bg, width, segments...)
}

// markdownSegments splits a line into styled segments. A zero-width span is
// treated as the line's base style (so a heading's lead marker recolors the
// plain text and any unstyled gaps), and a line with no spans is a single
// segment in the base style.
func markdownSegments(line markdown.Line, styles map[markdown.Style]part, fg color.Color) []part {
	base := styles[markdown.StyleText]
	if base.fg == nil {
		base.fg = fg
	}

	// Find a leading base-style marker (a zero-width span) if present. A table
	// row marker carries a row background: table cells are held back so the
	// marker's background can extend over them (they cannot paint the slab bg,
	// which would punch a hole in the row).
	spans := line.Spans
	if len(spans) > 0 && spans[0].Text == "" {
		if p, ok := styles[spans[0].Style]; ok {
			if p.bg != nil {
				return markdownTableSegments(line, p, styles, fg)
			}
			base = p
			if base.fg == nil {
				base.fg = fg
			}
		}
		spans = spans[1:]
	}
	base.text = ""

	if len(spans) == 0 {
		return []part{{text: line.Text, fg: base.fg, bold: base.bold, italic: base.italic, underline: base.underline, strike: base.strike}}
	}
	var segments []part
	pos := 0
	for _, span := range spans {
		if span.Text == "" {
			continue
		}
		idx := strings.Index(line.Text[pos:], span.Text)
		if idx < 0 {
			// Span text not found (should not happen); fall back to base.
			return []part{{text: line.Text, fg: base.fg, bold: base.bold, italic: base.italic, underline: base.underline, strike: base.strike}}
		}
		if idx > 0 {
			s := base
			s.text = line.Text[pos : pos+idx]
			segments = append(segments, s)
		}
		p := styles[span.Style]
		if p.fg == nil {
			p.fg = fg
		}
		p.text = span.Text
		p.link = span.Link
		segments = append(segments, p)
		pos += idx + len(span.Text)
	}
	if pos < len(line.Text) {
		s := base
		s.text = line.Text[pos:]
		segments = append(segments, s)
	}
	return segments
}

// markdownTableSegments builds the segments for a table row line. The row
// marker (marker.text is empty and marker.bg is set) provides a background for
// the whole row; each cell then paints its own text on that background. Cells
// are found by walking the line's zero-width lead spans, so the split never
// depends on the text: each lead's following span (or run of spans) is one
// cell. Background-coloured filler between cells is emitted as its own segment,
// so cell padding, column gaps, and trailing slab space share the row bg.
func markdownTableSegments(line markdown.Line, marker part, styles map[markdown.Style]part, fg color.Color) []part {
	base := marker
	base.text = ""
	if base.fg == nil {
		base.fg = fg
	}

	var segments []part
	var cell strings.Builder // styled text of the cell being built

	flushCell := func() {
		if cell.Len() == 0 {
			return
		}
		s := base
		s.text = cell.String()
		segments = append(segments, s)
		cell.Reset()
	}

	pos := 0
	for _, span := range line.Spans {
		if span.Text == "" {
			// A cell lead marker: flush the previous cell and skip it.
			flushCell()
			continue
		}
		idx := strings.Index(line.Text[pos:], span.Text)
		if idx < 0 {
			// Span text not found (should not happen); bail to the base style
			// for the whole line so nothing is dropped.
			s := base
			s.text = line.Text
			return []part{s}
		}
		if idx > 0 {
			// The gap belongs after the cell's text, so flush the cell first.
			flushCell()
			segments = append(segments, bgFiller(base, line.Text[pos:pos+idx]))
			pos += idx
		}
		if span.Style == markdown.StyleNone && span.Link == "" {
			cell.WriteString(span.Text)
		} else {
			flushCell()
			p := styles[span.Style]
			if p.fg == nil {
				p.fg = fg
			}
			p.text = span.Text
			p.link = span.Link
			p.bg = base.bg
			segments = append(segments, p)
		}
		pos += len(span.Text)
	}
	flushCell()
	if pos < len(line.Text) {
		segments = append(segments, bgFiller(base, line.Text[pos:]))
	}
	return segments
}

// bgFiller builds a segment for unstyled table text (cell padding, column
// gaps), painting it on the row background.
func bgFiller(base part, text string) part {
	s := base
	s.text = text
	return s
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
		if lipgloss.Width(segment.text) > avail {
			segment.text = ansi.Truncate(segment.text, avail, "…")
		}
		out.WriteString(paintPart(segment, bg))
		used += lipgloss.Width(segment.text)
	}
	out.WriteString(bgSpaces(bg, max(0, width-used)))
	return out.String()
}

// paintPart renders one segment on bg, or on the segment's own background
// when it has one.
func paintPart(segment part, bg color.Color) string {
	style := lipgloss.NewStyle().Foreground(segment.fg).Background(bg)
	if segment.bg != nil {
		style = style.Background(segment.bg)
	}
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
	rendered := style.Render(segment.text)
	if segment.link != "" {
		// OSC 8 hyperlink: terminals that support it make the span
		// clickable; others show the text (and the visible URL) unchanged.
		// The sequence is zero-width, so width accounting is unaffected.
		rendered = osc8Link(segment.link) + rendered + osc8Close()
	}
	return rendered
}

// osc8Link opens an OSC 8 hyperlink to target. Control bytes are stripped
// from the target first: the destination comes from model-authored markdown,
// and an embedded ESC or BEL could otherwise terminate the sequence early and
// inject terminal escapes (the same class kon strips on input).
func osc8Link(target string) string {
	return "\x1b]8;;" + oscSafe(target) + "\x1b\\"
}

// oscSafe removes control characters (C0, DEL, and C1, whose ST and CSI also
// end or start sequences) from an OSC 8 target.
func oscSafe(s string) string {
	return strings.Map(func(r rune) rune {
		if unicode.IsControl(r) {
			return -1
		}
		return r
	}, s)
}

// osc8Close terminates an OSC 8 hyperlink.
func osc8Close() string { return "\x1b]8;;\x1b\\" }

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
		stream: markdown.NewStream(markdown.Theme{}, markdownContentWidth(width)),
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

// PrintMarkdown renders the markdown read from src onto dst the way kon shows
// a reply, without the transcript's padding, and with prose in the terminal's
// own text color. Each block prints once it closes, so a reply piped in from a
// model streams through as it is written; the last block prints when src
// ends. Lines wrap at width, or not at all when width is below 1.
func PrintMarkdown(dst io.Writer, src io.Reader, width int) error {
	stream := markdown.NewStream(markdown.Theme{}, width)
	printed := 0
	// flush writes every line closed since the last flush in one write, so a
	// writer that downsamples colors never sees an escape sequence split.
	flush := func() error {
		lines := stream.Lines()
		if printed == len(lines) {
			return nil
		}
		var out strings.Builder
		for _, line := range lines[printed:] {
			for _, segment := range markdownSegments(line, markdownPalette, lipgloss.NoColor{}) {
				if segment.text != "" {
					out.WriteString(paintPart(segment, colorAgentBg))
				}
			}
			out.WriteByte('\n')
		}
		printed = len(lines)
		_, err := io.WriteString(dst, out.String())
		return err
	}
	buf := make([]byte, 32<<10)
	for {
		n, err := src.Read(buf)
		if n > 0 {
			stream.Write(string(buf[:n]))
			if err := flush(); err != nil {
				return err
			}
		}
		if err == io.EOF {
			break
		}
		if err != nil {
			return err
		}
	}
	stream.Finish()
	return flush()
}
