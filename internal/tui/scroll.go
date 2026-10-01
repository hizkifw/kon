package tui

import (
	"strings"

	tea "charm.land/bubbletea/v2"
)

// Scroll is a minimal vertical scroll container for lines already wrapped to
// its width, so it needs no soft wrap and no per-line width measurement: it
// slices the visible line range and pads to height. It replaces the bubbles
// viewport, whose SetContentLines rescanned and measured every line with
// grapheme-cluster segmentation on each frame, dominating the render cost.
type Scroll struct {
	width, height int
	lines         []string
	yOffset       int
}

// WheelStep is how many lines one mouse wheel notch scrolls.
const WheelStep = 3

// NewScroll returns an empty scroll container.
func NewScroll() Scroll { return Scroll{} }

// SetWidth sets the width lines are rendered at.
func (s *Scroll) SetWidth(w int) { s.width = w }

// Width is the width lines are rendered at.
func (s Scroll) Width() int { return s.width }

// Height is how many lines the window shows.
func (s Scroll) Height() int { return s.height }

// YOffset is the index of the first line in the window.
func (s Scroll) YOffset() int { return s.yOffset }

// LineCount is how many content lines the container holds.
func (s Scroll) LineCount() int { return len(s.lines) }

// SetHeight keeps the bottom edge in place rather than the top: the lines below
// the window stay below it. A reader at the bottom stays there when the window
// or the prompt grows, instead of losing the last lines under the input.
func (s *Scroll) SetHeight(h int) {
	below := s.LinesBelow()
	s.height = h
	s.SetLinesBelow(below)
}

// LinesBelow is the number of scrollable lines past the bottom edge of the
// window; zero means the view is at the bottom.
func (s Scroll) LinesBelow() int {
	return max(0, s.extent()-s.yOffset-s.height)
}

// SetLinesBelow scrolls so that n scrollable lines remain past the bottom edge.
func (s *Scroll) SetLinesBelow(n int) {
	s.SetYOffset(s.extent() - s.height - n)
}

// SetContentLines installs the display lines. The slice aliases the caller's
// cache and is only read.
func (s *Scroll) SetContentLines(lines []string) {
	s.lines = lines
	if s.yOffset > s.maxYOffset() {
		s.yOffset = s.maxYOffset()
	}
}

// bottomPad is the number of blank lines appended to the scrollable extent, so
// the bottom-most scroll position rests one blank line above the status bar.
// Scrolling up fills the window with content lines as usual; only the very
// bottom reveals the padding.
const bottomPad = 1

// extent is the number of scrollable lines: the content plus the trailing
// padding.
func (s Scroll) extent() int { return len(s.lines) + bottomPad }

func (s Scroll) maxYOffset() int {
	return max(0, s.extent()-s.height)
}

// AtBottom reports whether the view is scrolled to its bottom-most position:
// the last content line plus the padding line below it are visible.
func (s Scroll) AtBottom() bool { return s.yOffset >= s.maxYOffset() }

// SetYOffset scrolls so line n is first in the window, within bounds.
func (s *Scroll) SetYOffset(n int) {
	s.yOffset = min(max(0, n), s.maxYOffset())
}

// GotoBottom scrolls to the bottom-most position.
func (s *Scroll) GotoBottom() { s.yOffset = s.maxYOffset() }

// PageUp scrolls up one window.
func (s *Scroll) PageUp() {
	if s.yOffset <= 0 {
		return
	}
	s.SetYOffset(s.yOffset - s.height)
}

// PageDown scrolls down one window.
func (s *Scroll) PageDown() {
	if s.AtBottom() {
		return
	}
	s.SetYOffset(s.yOffset + s.height)
}

// Update scrolls on mouse wheel events. Keyboard scrolling is the caller's,
// so a prompt can keep plain keys.
func (s *Scroll) Update(msg tea.Msg) {
	wheel, ok := msg.(tea.MouseWheelMsg)
	if !ok {
		return
	}
	switch wheel.Button {
	case tea.MouseWheelDown:
		s.SetYOffset(s.yOffset + WheelStep)
	case tea.MouseWheelUp:
		s.SetYOffset(s.yOffset - WheelStep)
	}
}

// View renders exactly height lines starting at the current offset. The window
// reads past the content into the trailing padding, padded further with blank
// lines so the surrounding frame does not reflow.
func (s Scroll) View() string { return s.ViewWith(nil) }

// ViewWith renders like View, passing each content line in the window through
// decorate, with its index in the content, when decorate is set.
func (s Scroll) ViewWith(decorate func(i int, line string) string) string {
	if s.width <= 0 || s.height <= 0 {
		return ""
	}
	out := make([]string, s.height)
	start := min(s.yOffset, len(s.lines))
	end := min(start+s.height, len(s.lines))
	copy(out, s.lines[start:end])
	if decorate != nil {
		for i := range end - start {
			out[i] = decorate(start+i, out[i])
		}
	}
	return strings.Join(out, "\n")
}
