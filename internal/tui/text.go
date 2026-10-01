package tui

import (
	"strings"

	"charm.land/lipgloss/v2"
	"github.com/charmbracelet/x/ansi"
)

// Rect is an area of the screen, in cells.
type Rect struct{ X, Y, W, H int }

// Contains reports whether the cell at x, y is inside r.
func (r Rect) Contains(x, y int) bool {
	return x >= r.X && x < r.X+r.W && y >= r.Y && y < r.Y+r.H
}

// Fit shortens value to width cells, ending it with an ellipsis when it had
// to cut; a width below 1 leaves it whole.
func Fit(value string, width int) string {
	if width <= 0 || lipgloss.Width(value) <= width {
		return value
	}
	if width == 1 {
		return "…"
	}
	var out strings.Builder
	used := 0
	for _, r := range value {
		runeWidth := lipgloss.Width(string(r))
		if used+runeWidth > width-1 {
			break
		}
		out.WriteRune(r)
		used += runeWidth
	}
	out.WriteRune('…')
	return out.String()
}

// Pad fills line out to width cells, or cuts it to them, so nothing under
// it shows through a short line.
func Pad(line string, width int) string {
	if gap := width - ansi.StringWidth(line); gap > 0 {
		return line + strings.Repeat(" ", gap)
	}
	return ansi.Truncate(line, width, "")
}

// Splice replaces the w cells of line from column x with over. Resets on
// both sides keep the under line's styles from bleeding into over or past it.
func Splice(line, over string, x, w, width int) string {
	left := Pad(ansi.Truncate(line, x, ""), x)
	return left + "\x1b[m" + over + "\x1b[m" + ansi.Cut(line, x+w, width)
}

// Sanitize removes escape sequences and control characters other than newline
// and tab from text bound for the screen. It shares Stripper's state
// machine so an OSC (a window title, a hyperlink) is dropped whole rather than
// leaving its payload behind as text once the ESC is gone.
func Sanitize(s string) string {
	var strip Stripper
	return dropC1(strings.ReplaceAll(strip.Strip(s), "\r", ""))
}

// dropC1 removes C1 control characters (U+0080-U+009F), which some terminals
// act on like the ESC sequences they abbreviate: U+009B is CSI. In UTF-8 each
// is 0xC2 followed by 0x80-0x9F, and 0xC2 is only ever a lead byte, so a byte
// scan cannot split another character.
func dropC1(s string) string {
	if strings.IndexByte(s, 0xc2) < 0 {
		return s
	}
	var out strings.Builder
	out.Grow(len(s))
	for i := 0; i < len(s); i++ {
		if s[i] == 0xc2 && i+1 < len(s) && s[i+1] >= 0x80 && s[i+1] <= 0x9f {
			i++
			continue
		}
		out.WriteByte(s[i])
	}
	return out.String()
}
