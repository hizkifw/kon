package ui

import (
	"strings"

	tea "charm.land/bubbletea/v2"
)

// scrollView is a minimal vertical scroll container for the transcript. The
// transcript renders every line pre-wrapped to the viewport width, so the
// container needs no soft wrap and no per-line width measurement: it slices the
// visible line range and pads to height. It replaces the bubbles viewport, whose
// SetContentLines rescanned and measured every line with grapheme-cluster
// segmentation on each frame, dominating the render cost.
type scrollView struct {
	width, height int
	lines         []string
	yOffset       int
	mouseDelta    int
}

func newScrollView() scrollView {
	return scrollView{mouseDelta: 3}
}

func (s *scrollView) SetWidth(w int)  { s.width = w }
func (s *scrollView) SetHeight(h int) { s.height = h }
func (s scrollView) Width() int       { return s.width }
func (s scrollView) Height() int      { return s.height }
func (s scrollView) YOffset() int     { return s.yOffset }

// SetContentLines installs the display lines. The slice aliases the caller's
// cache and must not be mutated by the viewport; it is only read.
func (s *scrollView) SetContentLines(lines []string) {
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
func (s scrollView) extent() int { return len(s.lines) + bottomPad }

func (s scrollView) maxYOffset() int {
	return max(0, s.extent()-s.height)
}

// AtBottom reports whether the view is scrolled to its bottom-most position:
// the last content line plus the padding line below it are visible.
func (s scrollView) AtBottom() bool { return s.yOffset >= s.maxYOffset() }

func (s *scrollView) SetYOffset(n int) {
	s.yOffset = min(max(0, n), s.maxYOffset())
}

func (s *scrollView) GotoBottom() { s.yOffset = s.maxYOffset() }

func (s *scrollView) PageUp() {
	if s.yOffset <= 0 {
		return
	}
	s.SetYOffset(s.yOffset - s.height)
}

func (s *scrollView) PageDown() {
	if s.AtBottom() {
		return
	}
	s.SetYOffset(s.yOffset + s.height)
}

// Update scrolls on mouse wheel events. Keyboard scrolling is handled by the
// model so the prompt keeps focus on plain keys.
func (s *scrollView) Update(msg tea.Msg) {
	wheel, ok := msg.(tea.MouseWheelMsg)
	if !ok {
		return
	}
	switch wheel.Button {
	case tea.MouseWheelDown:
		s.SetYOffset(s.yOffset + s.mouseDelta)
	case tea.MouseWheelUp:
		s.SetYOffset(s.yOffset - s.mouseDelta)
	}
}

// View renders exactly height lines starting at the current offset. The window
// reads past the content into the trailing padding, padded further with blank
// lines so the surrounding frame does not reflow.
func (s scrollView) View() string {
	if s.width <= 0 || s.height <= 0 {
		return ""
	}
	out := make([]string, s.height)
	start := min(s.yOffset, len(s.lines))
	end := min(start+s.height, len(s.lines))
	copy(out, s.lines[start:end])
	return strings.Join(out, "\n")
}
