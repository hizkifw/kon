package tui

import (
	"strings"
	"testing"

	tea "charm.land/bubbletea/v2"
	"github.com/charmbracelet/x/ansi"
)

// lines is Content that shows fixed lines.
type lines []string

func (l lines) Lines(int) []string { return l }

// host is the state actions see in these tests.
type host struct{ ran []string }

func stack(width, height int) *Stack[*host] {
	s := &Stack[*host]{}
	s.Resize(width, height)
	return s
}

func TestStackNestsEachDrawerInsideTheOneBelow(t *testing.T) {
	s := stack(80, 24)
	lower, upper := s.Rect(0), s.Rect(1)
	if upper.W >= lower.W || upper.X <= lower.X || upper.X+upper.W != lower.X+lower.W || upper.H != 24 {
		t.Fatalf("upper %+v does not nest inside lower %+v", upper, lower)
	}
	if body := s.Body(0); body.Y != 1 || body.H != 22 || body.X != lower.X+1 {
		t.Fatalf("body %+v leaves no room for the title, hint row, and rule of %+v", body, lower)
	}
}

func TestActionAsksTwiceWhenItConfirms(t *testing.T) {
	s := stack(80, 24)
	h := &host{}
	s.Open(&Drawer[*host]{Title: "jobs", Content: lines{"a"}, Actions: func(*host) []Action[*host] {
		return []Action[*host]{{Key: "K", Hint: "⇧K", Label: "kill", Confirm: "press ⇧K again", Run: func(h *host) tea.Cmd {
			h.ran = append(h.ran, "kill")
			return nil
		}}}
	}})
	s.Key(h, "K")
	if len(h.ran) != 0 || s.Hints(h)[0].Text != "press ⇧K again" {
		t.Fatalf("first press ran %v, hints %+v", h.ran, s.Hints(h))
	}
	s.Key(h, "x")
	s.Key(h, "K")
	if len(h.ran) != 0 {
		t.Fatal("another key between presses did not disarm the action")
	}
	s.Key(h, "K")
	if len(h.ran) != 1 {
		t.Fatalf("second press ran %v", h.ran)
	}
}

func TestListSkipsHeadingsAndClicksOpenTheHighlightedRow(t *testing.T) {
	s := stack(80, 24)
	list := &List{Items: []Item{{Label: "running", Heading: true}, {Label: "one"}, {Label: "finished", Heading: true}, {Label: "two"}}, Index: 1}
	s.Open(&Drawer[*host]{Title: "jobs", List: list})
	s.Key(nil, "down")
	if list.Index != 3 {
		t.Fatalf("down from row 1 went to %d, want 3 past the heading", list.Index)
	}
	s.Key(nil, "home")
	if list.Index != 1 {
		t.Fatalf("home went to %d, want the first row that is not a heading", list.Index)
	}
	body := s.Body(0)
	if key, handled := s.Click(nil, body.X+2, body.Y); key != "" || !handled || list.Index != 1 {
		t.Fatalf("clicking a heading: key %q, handled %v, index %d", key, handled, list.Index)
	}
	if key, _ := s.Click(nil, body.X+2, body.Y+3); key != "" || list.Index != 3 {
		t.Fatalf("first click on a row: key %q, index %d", key, list.Index)
	}
	if key, _ := s.Click(nil, body.X+2, body.Y+3); key != "enter" {
		t.Fatalf("click on the highlighted row pressed %q, want enter", key)
	}
	hints := s.Hints(nil)
	esc := hints[len(hints)-1]
	if key, _ := s.Click(nil, body.X+esc.X, body.Y+body.H); key != "esc" {
		t.Fatalf("click on the esc hint pressed %q", key)
	}
}

func TestPaintFitsTheScreenAndRunsOnClose(t *testing.T) {
	s := stack(50, 10)
	closed := 0
	s.Open(&Drawer[*host]{Title: "side", Content: lines{"drawer text"}, OnClose: func(*host) { closed++ }})
	screen := s.Paint(nil, []string{strings.Repeat("x", 50)})
	if len(screen) != 10 {
		t.Fatalf("painted %d rows, want 10", len(screen))
	}
	for i, line := range screen {
		if w := ansi.StringWidth(line); w != 50 && strings.TrimSpace(ansi.Strip(line)) != "" {
			t.Fatalf("row %d is %d cells wide: %q", i, w, ansi.Strip(line))
		}
	}
	if got := ansi.Strip(strings.Join(screen, "\n")); !strings.Contains(got, " side") || !strings.Contains(got, "drawer text") || !strings.Contains(got, "esc close") {
		t.Fatalf("drawer missing from:\n%s", got)
	}
	s.Pop(nil)
	if s.Len() != 0 || closed != 1 {
		t.Fatalf("Pop left %d drawers and ran OnClose %d times", s.Len(), closed)
	}
}
