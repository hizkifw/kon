package ui

import (
	"strings"

	"charm.land/lipgloss/v2"
)

// welcomeBanner is the mark shown at the top of every transcript. It is
// presentation only: the transcript draws it as a stable prefix rather than a
// block, so it stays out of session records and model context while still
// appearing above a resumed conversation.
const welcomeBanner = `┌──┐              ┌──┐
│  ├──┬─────┬─────┤  │
│  ┌─<│  _  │     ├──┤
└──┴──┴─────┴──┴──┴──┘
harness for foxes =˄▾˄=`

// bannerText renders the welcome banner flush against the transcript's left
// edge, or "" when there is nothing to show. A single blank line pads the mark
// from the top of the viewport, the figure carries the brand accent, and the
// trailing caption is drawn faintly, so the mark reads as a header rather than
// content.
func (t *transcript) bannerText(width int) string {
	if t.banner == "" {
		return ""
	}
	lines := strings.Split(t.banner, "\n")
	widest := 0
	for _, line := range lines {
		if w := lipgloss.Width(line); w > widest {
			widest = w
		}
	}
	// A figure cannot wrap, so a terminal too narrow to hold it shows nothing
	// rather than a mangled mark.
	if widest > width {
		return ""
	}
	out := make([]string, 0, len(lines)+1)
	out = append(out, "")
	for i, line := range lines {
		style := lipgloss.NewStyle().Foreground(colorAccent)
		if i == len(lines)-1 {
			style = style.Foreground(colorFaint)
		}
		out = append(out, style.Render(line))
	}
	return strings.Join(out, "\n")
}
