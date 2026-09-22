package ui

import (
	"strings"
	"testing"

	tea "charm.land/bubbletea/v2"
)

func scrollViewWith(n, height int) scrollView {
	s := newScrollView()
	s.SetWidth(40)
	s.SetHeight(height)
	lines := make([]string, n)
	for i := range lines {
		lines[i] = "line"
	}
	s.SetContentLines(lines)
	return s
}

func TestScrollViewClampsOffset(t *testing.T) {
	s := scrollViewWith(100, 10)
	if got := s.maxYOffset(); got != 91 {
		t.Fatalf("maxYOffset = %d, want 91", got)
	}
	s.SetYOffset(1000)
	if !s.AtBottom() || s.YOffset() != 91 {
		t.Fatalf("set beyond bottom: offset = %d", s.YOffset())
	}
	s.SetYOffset(-5)
	if s.YOffset() != 0 {
		t.Fatalf("set above top: offset = %d", s.YOffset())
	}
	if s.AtBottom() {
		t.Fatal("reported at bottom at offset 0 with overflow content")
	}
}

func TestScrollViewPaging(t *testing.T) {
	s := scrollViewWith(100, 10)
	s.PageDown()
	if s.YOffset() != 10 {
		t.Fatalf("page down offset = %d, want 10", s.YOffset())
	}
	s.PageUp()
	if s.YOffset() != 0 {
		t.Fatalf("page up offset = %d, want 0", s.YOffset())
	}
	s.GotoBottom()
	if !s.AtBottom() {
		t.Fatalf("goto bottom left offset %d", s.YOffset())
	}
	s.PageDown()
	if s.YOffset() != 91 {
		t.Fatalf("page down at bottom moved to %d", s.YOffset())
	}
}

func TestScrollViewWheel(t *testing.T) {
	s := scrollViewWith(100, 10)
	s.Update(tea.MouseWheelMsg{Button: tea.MouseWheelDown})
	if s.YOffset() != s.mouseDelta {
		t.Fatalf("wheel down offset = %d, want %d", s.YOffset(), s.mouseDelta)
	}
	s.Update(tea.MouseWheelMsg{Button: tea.MouseWheelUp})
	if s.YOffset() != 0 {
		t.Fatalf("wheel up offset = %d, want 0", s.YOffset())
	}
	// Non-wheel messages are ignored.
	s.Update(tea.KeyPressMsg{Code: 'j', Text: "j"})
	if s.YOffset() != 0 {
		t.Fatalf("key press scrolled the view to %d", s.YOffset())
	}
}

func TestScrollViewRendersExactlyHeight(t *testing.T) {
	s := scrollViewWith(100, 4)
	if got := len(strings.Split(s.View(), "\n")); got != 4 {
		t.Fatalf("view lines = %d, want 4", got)
	}
	// Near the bottom there are fewer source lines than height; the view must
	// still be padded to the full height so the frame does not reflow.
	s.SetYOffset(999)
	if got := len(strings.Split(s.View(), "\n")); got != 4 {
		t.Fatalf("bottom view lines = %d, want 4", got)
	}
}

func TestScrollViewShrinksContentClampsOffset(t *testing.T) {
	s := scrollViewWith(100, 10)
	s.GotoBottom()
	s.SetContentLines([]string{"a", "b", "c"})
	if s.YOffset() != 0 {
		t.Fatalf("offset = %d after content shrank, want 0", s.YOffset())
	}
}

// TestScrollViewBottomPadding verifies the one-line padding at the bottom-most
// scroll position: the last visible line is blank, while one wheel notch up the
// window is filled entirely with content lines again.
func TestScrollViewBottomPadding(t *testing.T) {
	s := scrollViewWith(100, 10)
	s.GotoBottom()
	view := strings.Split(s.View(), "\n")
	if got := view[len(view)-1]; got != "" {
		t.Fatalf("bottom view should end in padding: %q", got)
	}
	if got := view[len(view)-2]; got != "line" {
		t.Fatalf("bottom view lost the last content line: %q", got)
	}
	// Scrolling up one line re-covers the padding with content.
	s.SetYOffset(s.maxYOffset() - 1)
	view = strings.Split(s.View(), "\n")
	for i, line := range view {
		if line != "line" {
			t.Fatalf("line %d = %q, want content once scrolled up", i, line)
		}
	}
	// Short content still renders a fully blank window without underflow.
	short := scrollViewWith(2, 4)
	if got := strings.Split(short.View(), "\n"); len(got) != 4 {
		t.Fatalf("short view lines = %d, want 4", len(got))
	}
}
