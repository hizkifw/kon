package ui

import (
	"strings"
	"testing"

	tea "charm.land/bubbletea/v2"
	productdocs "kon.kitsu.red/docs/product"
)

// openHelpAt opens /help in a model of the given size and turns it to page.
func openHelpAt(t *testing.T, width, height int, page string) (Model, *help) {
	t.Helper()
	m := sizedModel(t, width, height)
	updated, _ := m.openHelp()
	m = updated.(Model)
	h := m.topDrawer().Content.(*help)
	if page != helpIndex {
		h.visit(page, "")
	}
	return m, h
}

func keyPress(m Model, code rune) Model {
	m, _ = update(m, tea.KeyPressMsg{Code: code})
	return m
}

func TestEveryGuideLinkLeadsToAHeading(t *testing.T) {
	for _, width := range []int{60, 120} {
		for _, page := range productdocs.Pages() {
			_, h := openHelpAt(t, width, 30, page)
			for _, link := range h.links {
				target, anchor, _ := helpTarget(page, link.dest)
				_, other := openHelpAt(t, width, 30, target)
				found := anchor == ""
				for _, heading := range other.headings {
					found = found || heading.anchor == anchor
				}
				if !found {
					t.Errorf("width %d: %s links to %s, and %s has no such heading", width, page, link.dest, target)
				}
			}
		}
	}
}

func TestHelpOpensTheIndexAtItsTop(t *testing.T) {
	m := sizedModel(t, 100, 30)
	m.input.SetValue("/help")
	m = keyPress(m, tea.KeyEnter)
	if m.drawers.Len() != 1 {
		t.Fatalf("/help opened %d drawers: %q", m.drawers.Len(), m.message)
	}
	got := plain(m.View().Content)
	if !strings.Contains(got, " kon user guide") || !strings.Contains(got, "Getting started") {
		t.Fatalf("the index is not shown from its top:\n%s", got)
	}
	// The drawer follows links to the guide itself, so where they lead is
	// not spelled out after them.
	if strings.Contains(got, "getting-started.md") {
		t.Fatalf("a link to the guide shows its destination:\n%s", got)
	}
}

func TestHelpFollowsAFocusedLinkAndComesBack(t *testing.T) {
	m, h := openHelpAt(t, 100, 30, helpIndex)
	m = keyPress(m, tea.KeyTab)
	if h.focus != 0 {
		t.Fatalf("Tab focused link %d, want the first in view", h.focus)
	}
	first := h.links[0]
	target, anchor, _ := helpTarget(helpIndex, first.dest)
	m = keyPress(m, tea.KeyEnter)
	if h.page != target || len(h.back) != 1 || h.focus != -1 {
		t.Fatalf("Enter on %s left page %s, back %d, focus %d", first.dest, h.page, len(h.back), h.focus)
	}
	if anchor != "" {
		top := h.drawer.View().YOffset()
		at := -1
		for _, heading := range h.headings {
			if heading.anchor == anchor {
				at = heading.line
			}
		}
		// A heading near the end of a page cannot reach the top row.
		if at < 0 || top > at || at >= top+h.drawer.View().Height() {
			t.Fatalf("heading %s on line %d is not in view from line %d", anchor, at, top)
		}
	}
	m = keyPress(m, tea.KeyBackspace)
	if h.page != helpIndex || len(h.back) != 0 || h.focus != 0 || h.drawer.View().YOffset() != 0 {
		t.Fatalf("Backspace left page %s, back %d, focus %d, line %d", h.page, len(h.back), h.focus, h.drawer.View().YOffset())
	}
	for _, hint := range m.drawers.Hints(&m) {
		if hint.Key == "backspace" {
			t.Fatal("the drawer offers to go back with nowhere to go")
		}
	}
	if m.drawers.Len() != 1 {
		t.Fatalf("following a link left %d drawers", m.drawers.Len())
	}
}

func TestHelpTabStartsFromTheLinksInView(t *testing.T) {
	_, h := openHelpAt(t, 100, 12, "reference.md")
	view := h.drawer.View()
	view.GotoBottom()
	h.step(-1)
	if line := h.links[h.focus].runs[0].line; line < view.YOffset() || line >= view.YOffset()+view.Height() {
		t.Fatalf("Shift+Tab focused a link on line %d, out of view from line %d", line, view.YOffset())
	}
	// From the last link Tab goes around to the first, which scrolls into view.
	h.focus = len(h.links) - 1
	view.GotoBottom()
	h.step(1)
	if line := h.links[0].runs[0].line; h.focus != 0 || line < view.YOffset() || line >= view.YOffset()+view.Height() {
		t.Fatalf("Tab focused link %d, with the first on line %d and the view at %d", h.focus, line, view.YOffset())
	}
}

func TestHelpFollowsAClickedLink(t *testing.T) {
	m, h := openHelpAt(t, 100, 30, helpIndex)
	body := m.drawers.Body(0)
	var run helpRun
	for _, link := range h.links {
		if link.dest == "configuration.md" {
			run = link.runs[0]
		}
	}
	if run.to == 0 {
		t.Fatal("the index has no link to configuration.md")
	}
	h.drawer.View().SetYOffset(run.line)
	row := body.Y + run.line - h.drawer.View().YOffset()
	// One cell past the link is not it.
	m, _ = clicks(m, body.X+run.to, row, 1)
	if h.page != helpIndex {
		t.Fatalf("a click beside the link went to %s", h.page)
	}
	m, _ = clicks(m, body.X+run.from, row, 1)
	if h.page != "configuration.md" || h.drawer.View().YOffset() != 0 {
		t.Fatalf("the click left page %s at line %d", h.page, h.drawer.View().YOffset())
	}
	if got := plain(m.View().Content); !strings.Contains(got, " kon user guide › Configuration") {
		t.Fatalf("the trail does not name the page:\n%s", got)
	}
}

func TestHelpTrailFollowsTheSectionAndGoesUp(t *testing.T) {
	m, h := openHelpAt(t, 100, 30, "usage.md")
	section := h.headings[2]
	h.drawer.View().SetYOffset(section.line)
	want := " kon user guide › Working with kon › " + section.text
	if got := plain(m.View().Content); !strings.Contains(got, want) {
		t.Fatalf("trail is not %q:\n%s", want, got)
	}
	body := m.drawers.Body(0)
	// The page's crumb goes to its top, and the guide's to the index.
	m, _ = clicks(m, body.X+1+len("kon user guide › "), body.Y-1, 1)
	if h.page != "usage.md" || h.drawer.View().YOffset() != 0 {
		t.Fatalf("the page crumb left %s at line %d", h.page, h.drawer.View().YOffset())
	}
	m, _ = clicks(m, body.X+1, body.Y-1, 1)
	if h.page != helpIndex || len(h.back) != 2 {
		t.Fatalf("the guide crumb left %s with %d places to go back to", h.page, len(h.back))
	}
	if m.drawers.Len() != 1 {
		t.Fatal("a click on the trail closed the drawer")
	}
}

func TestHelpDragCopiesTheMarkdownBehindThePage(t *testing.T) {
	m, h := openHelpAt(t, 100, 30, helpIndex)
	var run helpRun
	for _, link := range h.links {
		if link.dest == "configuration.md" {
			run = link.runs[0]
		}
	}
	h.drawer.View().SetYOffset(run.line)
	body := m.drawers.Body(0)
	row := body.Y + run.line - h.drawer.View().YOffset()
	// A drag over a link selects it, and only a click follows it.
	m, _ = move(pressAt(m, body.X+run.from, row), body.X+run.to-1, row)
	if got := highlighted(m); len(got) != 1 || got[0] != "Configuration" {
		t.Fatalf("highlighted %q", got)
	}
	_, cmd := release(m, body.X+run.to-1, row)
	if got := selectedText(t, cmd); got != "[Configuration](configuration.md)" {
		t.Fatalf("copied %q", got)
	}
	if h.page != helpIndex || h.text.selection != nil {
		t.Fatalf("the drag left page %s, selection %v", h.page, h.text.selection)
	}
}

func TestHelpMouseFocusesALinkOnlyWhileItIsPressed(t *testing.T) {
	m, h := openHelpAt(t, 100, 30, helpIndex)
	link := -1
	for i, l := range h.links {
		if l.dest == "configuration.md" {
			link = i
		}
	}
	run := h.links[link].runs[0]
	h.drawer.View().SetYOffset(run.line)
	body := m.drawers.Body(0)
	row := body.Y + run.line - h.drawer.View().YOffset()

	// A press beside the link drops the focus Tab left.
	m = keyPress(m, tea.KeyTab)
	if h.focus < 0 {
		t.Fatal("Tab focused no link")
	}
	m = pressAt(m, body.X+run.to, row)
	if h.focus != -1 {
		t.Fatalf("a press elsewhere left link %d in focus", h.focus)
	}
	m, _ = release(m, body.X+run.to, row)

	// A press on the link focuses it, and a drag from there selects
	// instead, with the focus gone and the link not followed.
	m = pressAt(m, body.X+run.from, row)
	if h.focus != link {
		t.Fatalf("a press on link %d focused %d", link, h.focus)
	}
	m, _ = move(m, body.X+run.from+3, row)
	if h.focus != -1 || h.text.selection == nil {
		t.Fatalf("the drag left focus %d, selection %v", h.focus, h.text.selection)
	}
	m, _ = release(m, body.X+run.from+3, row)
	if h.page != helpIndex {
		t.Fatalf("the drag went to %s", h.page)
	}

	// A press and release follows it, and coming back shows no focus. The
	// press comes too late after the last to be a double click.
	m.click = click{}
	m = pressAt(m, body.X+run.from, row)
	m, _ = release(m, body.X+run.from, row)
	if h.page != "configuration.md" || h.focus != -1 {
		t.Fatalf("the click left page %s, focus %d", h.page, h.focus)
	}
	keyPress(m, tea.KeyBackspace)
	if h.page != helpIndex || h.focus != -1 {
		t.Fatalf("Backspace left page %s, focus %d", h.page, h.focus)
	}
}

func TestHelpDropsAFocusPastTheLinksOfANewWidth(t *testing.T) {
	m, h := openHelpAt(t, 100, 30, "reference.md")
	// Two links to one place on rows that follow each other count as one,
	// so a wider page can have fewer links than the focus was counted in.
	h.focus = len(h.links)
	m, _ = update(m, tea.WindowSizeMsg{Width: 150, Height: 30})
	if h.focus != -1 {
		t.Fatalf("focus %d of %d links", h.focus, len(h.links))
	}
	// Enter has no link to follow, and Tab starts over.
	m = keyPress(m, tea.KeyEnter)
	keyPress(m, tea.KeyTab)
	if h.page != "reference.md" || h.focus < 0 {
		t.Fatalf("page %s, focus %d", h.page, h.focus)
	}
}
