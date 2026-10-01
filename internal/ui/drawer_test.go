package ui

import (
	"strconv"
	"strings"
	"testing"

	tea "charm.land/bubbletea/v2"
	"github.com/charmbracelet/x/ansi"
)

// testDrawer opens a drawer showing text, one block per line, and returns it.
func testDrawer(m *Model, title string, lines ...string) *drawer {
	t := &transcript{cwd: "/tmp"}
	for _, line := range lines {
		t.add(block{kind: blockAssistant, text: line})
	}
	d := &drawer{Title: title, Content: t}
	m.openDrawer(d)
	return d
}

func sizedModel(t *testing.T, width, height int) Model {
	t.Helper()
	m := newTestModel(t)
	m, _ = update(m, tea.WindowSizeMsg{Width: width, Height: height})
	return m
}

func TestDrawerPaintsOverADimmedScreen(t *testing.T) {
	for _, size := range [][2]int{{80, 24}, {50, 60}} {
		m := sizedModel(t, size[0], size[1])
		m.transcript.add(block{kind: blockAssistant, text: "main reply"})
		m.refreshTranscript(true)
		testDrawer(&m, "stats", "drawer text")
		lines := strings.Split(m.View().Content, "\n")
		if len(lines) != m.height {
			t.Fatalf("%v: %d lines, want %d", size, len(lines), m.height)
		}
		for i, line := range lines {
			if w := ansi.StringWidth(line); w > m.width {
				t.Fatalf("%v: line %d is %d cells wide: %q", size, i, w, ansi.Strip(line))
			}
		}
		got := plain(m.View().Content)
		if !strings.Contains(got, " stats") || !strings.Contains(got, "drawer text") {
			t.Fatalf("%v: drawer missing:\n%s", size, got)
		}
		if m.View().Cursor != nil {
			t.Fatalf("%v: the prompt's cursor shows under a drawer", size)
		}
	}
}

func TestDrawersStackAndCloseFromTheTop(t *testing.T) {
	m := sizedModel(t, 80, 24)
	var closed []string
	for _, title := range []string{"lower", "upper"} {
		d := testDrawer(&m, title)
		d.OnClose = func(*Model) { closed = append(closed, title) }
	}
	lower := m.drawers.Rect(0)
	upper := m.drawers.Rect(1)
	if upper.W >= lower.W || upper.X <= lower.X || upper.X+upper.W != lower.X+lower.W {
		t.Fatalf("upper %+v does not nest inside lower %+v", upper, lower)
	}
	if got := plain(m.View().Content); !strings.Contains(got, "lower") || !strings.Contains(got, "upper") {
		t.Fatalf("the lower drawer's edge is hidden:\n%s", got)
	}
	m, _ = update(m, tea.KeyPressMsg{Code: tea.KeyEscape})
	if m.drawers.Len() != 1 || m.topDrawer().Title != "lower" || len(closed) != 1 || closed[0] != "upper" {
		t.Fatalf("Esc closed %v, leaving %d drawers", closed, m.drawers.Len())
	}
}

func TestClickOutsideTheDrawerClosesIt(t *testing.T) {
	m := sizedModel(t, 80, 24)
	testDrawer(&m, "stats", "drawer text")
	r := m.drawers.Rect(0)
	// The title row is inside the drawer and does nothing.
	m = pressAt(m, r.X+2, r.Y)
	if m.drawers.Len() != 1 {
		t.Fatal("a click on the title closed the drawer")
	}
	m = pressAt(m, r.X-1, 5)
	if m.drawers.Len() != 0 {
		t.Fatal("a click on the dimmed area left the drawer open")
	}
	// The click that closed the drawer does not start a selection underneath.
	if _, cmd := release(m, r.X-1, 5); cmd != nil {
		t.Fatal("the closing click copied a selection")
	}
}

func TestDrawerTakesTheWheelAndKeys(t *testing.T) {
	m := sizedModel(t, 80, 24)
	for i := range 60 {
		m.transcript.add(block{kind: blockAssistant, text: "main " + strconv.Itoa(i)})
	}
	m.refreshTranscript(true)
	lines := make([]string, 60)
	for i := range lines {
		lines[i] = "side " + strconv.Itoa(i)
	}
	d := testDrawer(&m, "stats", lines...)
	mainOffset, drawerOffset := m.viewport.YOffset(), d.View().YOffset()
	m, _ = update(m, tea.MouseWheelMsg{X: 5, Y: 5, Button: tea.MouseWheelUp})
	if m.viewport.YOffset() != mainOffset || d.View().YOffset() >= drawerOffset {
		t.Fatalf("wheel moved main %d→%d, drawer %d→%d", mainOffset, m.viewport.YOffset(), drawerOffset, d.View().YOffset())
	}
	m, _ = update(m, tea.KeyPressMsg{Code: tea.KeyHome})
	if d.View().YOffset() != 0 || m.viewport.YOffset() != mainOffset {
		t.Fatal("Home did not scroll the drawer to its start")
	}
	m, _ = update(m, tea.KeyPressMsg{Code: 'x', Text: "x"})
	if m.input.Value() != "" {
		t.Fatal("typing under a drawer reached the prompt")
	}
}

func TestResizeRefitsTheDrawer(t *testing.T) {
	m := sizedModel(t, 80, 24)
	d := testDrawer(&m, "stats", "drawer text")
	m, _ = update(m, tea.WindowSizeMsg{Width: 120, Height: 40})
	if want := m.drawers.Body(0); d.View().Width() != want.W || d.View().Height() != want.H {
		t.Fatalf("drawer view is %dx%d after resize, want %dx%d", d.View().Width(), d.View().Height(), want.W, want.H)
	}
	if got := plain(m.View().Content); !strings.Contains(got, "drawer text") {
		t.Fatalf("drawer lost its text on resize:\n%s", got)
	}
}
