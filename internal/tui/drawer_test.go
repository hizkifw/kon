package tui

import (
	"strings"
	"testing"

	tea "charm.land/bubbletea/v2"
	"charm.land/lipgloss/v2"
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

func TestTrailHeadsTheDrawerAndItsCrumbsAreClicked(t *testing.T) {
	s := stack(80, 24)
	h := &host{}
	s.Open(&Drawer[*host]{Title: "guide", Content: lines{"a"}, Trail: func(*host) []Crumb[*host] {
		return []Crumb[*host]{
			{Label: "guide", Go: func(h *host) { h.ran = append(h.ran, "guide") }},
			{Label: "section"},
		}
	}})
	body := s.Body(0)
	if title := ansi.Strip(s.Paint(h, nil)[0]); !strings.Contains(title, " guide › section") {
		t.Fatalf("title row %q does not show the trail", title)
	}
	// The crumb with nowhere to go, and the separator, take the press and
	// do nothing.
	for _, x := range []int{body.X + 1 + len("guide"), body.X + 1 + len("guide › ")} {
		_, handled := s.Click(h, x, body.Y-1)
		s.Release(h, x, body.Y-1)
		if !handled || len(h.ran) != 0 {
			t.Fatalf("click at %d: handled %v, ran %v", x, handled, h.ran)
		}
	}
	// A crumb shows as pressed while the button is held, and goes where it
	// leads only when the button is let go on it.
	s.Theme.Pressed = lipgloss.NewStyle().Reverse(true)
	s.Click(h, body.X+1, body.Y-1)
	if title := s.Paint(h, nil)[0]; !strings.Contains(title, "\x1b[7mguide\x1b[m") || len(h.ran) != 0 {
		t.Fatalf("pressed title row %q, ran %v", title, h.ran)
	}
	s.Release(h, body.X+1, body.Y)
	if title := s.Paint(h, nil)[0]; strings.Contains(title, "\x1b[7m") || len(h.ran) != 0 {
		t.Fatalf("after a release off the crumb: title row %q, ran %v", title, h.ran)
	}
	s.Click(h, body.X+1, body.Y-1)
	s.Release(h, body.X+len("guide"), body.Y-1)
	if len(h.ran) != 1 {
		t.Fatalf("a release on the crumb ran %v", h.ran)
	}
}

func TestAPressedCrumbIsDroppedWhenTheTrailChanges(t *testing.T) {
	s := stack(80, 24)
	h := &host{}
	deep := true
	s.Open(&Drawer[*host]{Content: lines{"a"}, Trail: func(*host) []Crumb[*host] {
		trail := []Crumb[*host]{{Label: "guide", Go: func(h *host) { h.ran = append(h.ran, "guide") }}}
		if deep {
			trail = append(trail, Crumb[*host]{Label: "page", Go: func(h *host) { h.ran = append(h.ran, "page") }})
		}
		return trail
	}})
	body := s.Body(0)
	page := body.X + 1 + len("guide") + ansi.StringWidth(crumbSeparator)
	s.Theme.Pressed = lipgloss.NewStyle().Reverse(true)

	// The trail loses the crumb held with no key pressed: the row paints
	// without it, and the release goes nowhere.
	s.Click(h, page, body.Y-1)
	deep = false
	if title := s.Paint(h, nil)[0]; strings.Contains(title, "\x1b[7m") {
		t.Fatalf("title row %q shows a crumb pressed that is gone", title)
	}
	s.Release(h, page, body.Y-1)

	// A key in between lets go of the crumb, which may be another by now.
	s.Click(h, body.X+1, body.Y-1)
	s.Key(h, "down")
	if title := s.Paint(h, nil)[0]; strings.Contains(title, "\x1b[7m") {
		t.Fatalf("title row %q still shows the crumb pressed after a key", title)
	}
	s.Release(h, body.X+1, body.Y-1)
	if len(h.ran) != 0 {
		t.Fatalf("the releases ran %v", h.ran)
	}
}
