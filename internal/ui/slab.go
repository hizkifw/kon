package ui

import (
	"image/color"
	"strings"
	"unicode"

	"charm.land/lipgloss/v2"
	"github.com/charmbracelet/x/ansi"
)

// part is one styled segment of a slab line.
type part struct {
	text      string
	fg        color.Color
	bg        color.Color // segment background; nil uses the slab background
	link      string      // OSC 8 hyperlink target, when set
	bold      bool
	italic    bool
	underline bool
	strike    bool
}

// slabLine renders one full-width line of a background slab with individually
// colored segments. Segments are styled separately (each carrying the slab
// background) so their resets cannot punch holes in the slab, and the joins
// and padding are painted with the same background.
func slabLine(bg color.Color, width int, segments ...part) string {
	var out strings.Builder
	used := 1 // left padding cell
	out.WriteString(bgSpaces(bg, 1))
	for _, segment := range segments {
		if segment.text == "" {
			continue
		}
		if used > 1 {
			out.WriteString(bgSpaces(bg, 1))
			used++
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
		out.WriteString(style.Render(text))
		used += lipgloss.Width(text)
	}
	out.WriteString(bgSpaces(bg, max(0, width-used)))
	return out.String()
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

func bgSpaces(bg color.Color, n int) string {
	if n <= 0 {
		return ""
	}
	return lipgloss.NewStyle().Background(bg).Render(strings.Repeat(" ", n))
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
