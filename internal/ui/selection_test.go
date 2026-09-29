package ui

import (
	"regexp"
	"strings"
	"testing"
	"time"

	tea "charm.land/bubbletea/v2"
	"github.com/charmbracelet/x/ansi"
)

// The commands these tests run only work out a selection's text. The one that
// copies it is never run: it would write to the clipboard of whoever runs the
// tests, and inside tmux to their paste buffers.

const selectionReply = "Use **fmt.Println** to print.\n\n```go\nfunc main() {\n\tx()\n}\n```\n\n- one\n- two"

func selectionModel(t *testing.T) Model {
	t.Helper()
	return transcriptModel(t, block{kind: blockUser, text: "how do I print?"}, block{kind: blockAssistant, text: selectionReply})
}

func transcriptModel(t *testing.T, blocks ...block) Model {
	t.Helper()
	m := newTestModel(t)
	for _, b := range blocks {
		m.transcript.add(b)
	}
	m.refreshTranscript(true)
	return m
}

// cellOf returns the screen cell showing the first byte of text in the surface
// the mouse works on, which must be on screen and preceded only by single-cell characters on its line.
func cellOf(t *testing.T, m Model, text string) (x, y int) {
	t.Helper()
	view, area := m.surface()
	for i, line := range m.activeTranscript().lines {
		if col := strings.Index(ansi.Strip(line), text); col >= 0 {
			return area.x + col, area.y + i - view.YOffset()
		}
	}
	t.Fatalf("%q is not in the transcript", text)
	return 0, 0
}

func update(m Model, msg tea.Msg) (Model, tea.Cmd) {
	updated, cmd := m.Update(msg)
	return updated.(Model), cmd
}

func pressAt(m Model, x, y int) Model {
	m, _ = update(m, tea.MouseClickMsg{X: x, Y: y, Button: tea.MouseLeft})
	return m
}

func move(m Model, x, y int) (Model, tea.Cmd) {
	return update(m, tea.MouseMotionMsg{X: x, Y: y, Button: tea.MouseLeft})
}

func release(m Model, x, y int) (Model, tea.Cmd) {
	return update(m, tea.MouseReleaseMsg{X: x, Y: y, Button: tea.MouseLeft})
}

// drag presses at one cell, moves to another, and releases there.
func drag(m Model, fromX, fromY, toX, toY int) (Model, tea.Cmd) {
	m = pressAt(m, fromX, fromY)
	m, _ = move(m, toX, toY)
	return release(m, toX, toY)
}

// clicks presses and releases at a cell n times in a row, returning the
// command of the last release.
func clicks(m Model, x, y, n int) (Model, tea.Cmd) {
	var cmd tea.Cmd
	for range n {
		m, cmd = release(pressAt(m, x, y), x, y)
	}
	return m, cmd
}

// selectedText runs the command that works out a selection's text.
func selectedText(t *testing.T, cmd tea.Cmd) string {
	t.Helper()
	if cmd == nil {
		t.Fatal("nothing was copied")
	}
	msg, ok := cmd().(selectionTextMsg)
	if !ok {
		t.Fatalf("the release's command did not work out a selection")
	}
	return msg.text
}

// highlighted returns the runs of text the view shows in reverse video.
func highlighted(m Model) []string {
	var runs []string
	for _, match := range regexp.MustCompile("\x1b\\[7m([^\x1b]*)").FindAllStringSubmatch(m.View().Content, -1) {
		runs = append(runs, match[1])
	}
	return runs
}

func TestDragCopiesTheMarkdownBehindAReply(t *testing.T) {
	m := selectionModel(t)
	x, y := cellOf(t, m, "fmt.Println")
	_, cmd := drag(m, x, y, x+len("fmt.Println")-1, y)
	if got := selectedText(t, cmd); got != "**fmt.Println**" {
		t.Fatalf("copied %q", got)
	}
}

func TestDragIntoCodeClosesItsFence(t *testing.T) {
	m := selectionModel(t)
	x, y := cellOf(t, m, "to print")
	endX, endY := cellOf(t, m, "x()")
	_, cmd := drag(m, x, y, endX+2, endY)
	if got, want := selectedText(t, cmd), "to print.\n\n```go\nfunc main() {\n\tx()\n```"; got != want {
		t.Fatalf("copied %q, want %q", got, want)
	}
}

func TestDragBackwardsCopiesTheSame(t *testing.T) {
	m := selectionModel(t)
	x, y := cellOf(t, m, "fmt.Println")
	_, cmd := drag(m, x+len("fmt.Println")-1, y, x, y)
	if got := selectedText(t, cmd); got != "**fmt.Println**" {
		t.Fatalf("copied %q", got)
	}
}

func TestDragFromAPromptQuotesIt(t *testing.T) {
	m := selectionModel(t)
	x, y := cellOf(t, m, "how do I")
	endX, endY := cellOf(t, m, "Use")
	_, cmd := drag(m, x, y, endX+2, endY)
	if got, want := selectedText(t, cmd), "> how do I print?\n\nUse"; got != want {
		t.Fatalf("copied %q, want %q", got, want)
	}
}

func TestToolOutputCopiesAsShownWithoutTheSlabIndent(t *testing.T) {
	m := transcriptModel(t, toolDoneBlock("read", `{"path":"/tmp/a.go"}`, "func main() {\n\tif true {\n\t}\n}", false, "/tmp"))
	x, y := cellOf(t, m, "func main")
	endX, endY := cellOf(t, m, "if true")
	_, cmd := drag(m, x, y, endX+len("if true {")-1, endY)
	if got, want := selectedText(t, cmd), "func main() {\n    if true {"; got != want {
		t.Fatalf("copied %q, want %q", got, want)
	}
}

func TestPressAloneSelectsNothing(t *testing.T) {
	m := selectionModel(t)
	x, y := cellOf(t, m, "fmt.Println")
	m = pressAt(m, x, y)
	if m.transcript.selection != nil || len(highlighted(m)) > 0 {
		t.Fatalf("a press selected: %+v", m.transcript.selection)
	}
	// A move within the pressed cell is not a drag yet either.
	if m, _ = move(m, x, y); m.transcript.selection != nil {
		t.Fatal("a move within the pressed cell started a selection")
	}
	if _, cmd := release(m, x, y); cmd != nil {
		t.Fatal("a click copied")
	}
}

func TestPressOffTheTranscriptSelectsNothing(t *testing.T) {
	m := selectionModel(t)
	for _, y := range []int{0, transcriptTop + m.viewport.Height()} {
		if _, cmd := drag(m, 2, y, 8, y); cmd != nil {
			t.Fatalf("a drag on row %d copied", y)
		}
	}
}

func TestSelectionIsHighlightedUntilReleased(t *testing.T) {
	m := selectionModel(t)
	x, y := cellOf(t, m, "fmt.Println")
	m = pressAt(m, x, y)
	m, _ = move(m, x+3, y)
	if got := highlighted(m); len(got) != 1 || got[0] != "fmt." {
		t.Fatalf("highlighted %q while dragging", got)
	}
	m, cmd := release(m, x+3, y)
	if cmd == nil || m.transcript.selection != nil || len(highlighted(m)) > 0 {
		t.Fatalf("after the release: copied %v, selection %+v, highlighted %q", cmd != nil, m.transcript.selection, highlighted(m))
	}
}

// A line's highlight covers its text only: not the slab's margin before it,
// the padding after it, or a blank line between paragraphs.
func TestHighlightCoversOnlyText(t *testing.T) {
	m := transcriptModel(t, block{kind: blockAssistant, text: "first paragraph line\n\nsecond paragraph"})
	x, y := cellOf(t, m, "paragraph line")
	endX, endY := cellOf(t, m, "second")
	m = pressAt(m, x, y)
	m, _ = move(m, endX+2, endY)
	if got, want := highlighted(m), []string{"paragraph line", "sec"}; strings.Join(got, "|") != strings.Join(want, "|") {
		t.Fatalf("highlighted %q, want %q", got, want)
	}
}

func TestResizeDropsTheSelection(t *testing.T) {
	m := selectionModel(t)
	x, y := cellOf(t, m, "fmt.Println")
	m = pressAt(m, x, y)
	m, _ = move(m, x+3, y)
	if m, _ = update(m, tea.WindowSizeMsg{Width: 60, Height: 24}); m.transcript.selection != nil {
		t.Fatal("a rewrap kept a selection of lines that moved")
	}
}

func TestDoubleClickCopiesAWord(t *testing.T) {
	m := selectionModel(t)
	x, y := cellOf(t, m, "Println")
	_, cmd := clicks(m, x, y, 2)
	if got := selectedText(t, cmd); got != "**fmt.Println**" {
		t.Fatalf("copied %q", got)
	}
}

func TestDoubleClickThenDragGrowsByWords(t *testing.T) {
	m := selectionModel(t)
	x, y := cellOf(t, m, "Use")
	m, _ = clicks(m, x+1, y, 1)
	m = pressAt(m, x+1, y)
	endX, _ := cellOf(t, m, "Println")
	m, _ = move(m, endX, y)
	if got := highlighted(m); len(got) != 1 || got[0] != "Use fmt.Println" {
		t.Fatalf("highlighted %q", got)
	}
	_, cmd := release(m, endX, y)
	if got := selectedText(t, cmd); got != "Use **fmt.Println**" {
		t.Fatalf("copied %q", got)
	}
}

func TestTripleClickCopiesAParagraph(t *testing.T) {
	m := selectionModel(t)
	x, y := cellOf(t, m, "to print")
	_, cmd := clicks(m, x, y, 3)
	if got := selectedText(t, cmd); got != "Use **fmt.Println** to print." {
		t.Fatalf("copied %q", got)
	}
}

func TestSlowClicksAreSeparate(t *testing.T) {
	m := selectionModel(t)
	x, y := cellOf(t, m, "Println")
	m, _ = clicks(m, x, y, 1)
	m.click.when = m.click.when.Add(-time.Second)
	if _, cmd := clicks(m, x, y, 1); cmd != nil {
		t.Fatal("two slow clicks selected a word")
	}
}

// A link's destination is the renderer's own text, with no source to cut out,
// so a word selected in it copies as shown.
func TestDoubleClickOnALinkDestinationCopiesIt(t *testing.T) {
	m := transcriptModel(t, block{kind: blockAssistant, text: "See [the docs](https://x.test/doc) now."})
	x, y := cellOf(t, m, "x.test")
	_, cmd := clicks(m, x, y, 2)
	if got := selectedText(t, cmd); got != "https://x.test/doc" {
		t.Fatalf("copied %q", got)
	}
}

func TestWheelWhileDraggingGrowsTheSelection(t *testing.T) {
	m := selectionModel(t)
	m.height = 8
	m.resize()
	m.refreshTranscript(true)
	m.viewport.GotoBottom()
	y := transcriptTop + 3
	m = pressAt(m, 4, y)
	m, _ = move(m, 5, y)
	before := m.transcript.selection.head.from.line
	m, _ = update(m, tea.MouseWheelMsg{X: 5, Y: y, Button: tea.MouseWheelUp})
	if got, want := m.transcript.selection.head.from.line, before-m.viewport.mouseDelta; got != want {
		t.Fatalf("head on line %d after a wheel up, want %d", got, want)
	}
}

func TestDragPastTheTopScrollsUp(t *testing.T) {
	m := selectionModel(t)
	m.height = 8
	m.resize()
	m.refreshTranscript(true)
	m.viewport.GotoBottom()
	before := m.viewport.YOffset()
	m = pressAt(m, 2, transcriptTop+1)
	m, cmd := move(m, 2, 0)
	if cmd == nil {
		t.Fatal("a drag past the top did not start scrolling")
	}
	m, next := update(m, selectScrollMsg{epoch: m.selectEpoch})
	if m.viewport.YOffset() != before-1 || next == nil {
		t.Fatalf("offset %d after a tick, was %d; next tick %v", m.viewport.YOffset(), before, next)
	}
	if head := m.transcript.selection.head; head.from.line != m.viewport.YOffset() {
		t.Fatalf("head on line %d, top line is %d", head.from.line, m.viewport.YOffset())
	}
	// Back on the transcript and past the top again, the drag scrolls on a
	// new chain of ticks, and one still in flight from the first is dropped,
	// or the two would scroll twice as fast.
	first := m.selectEpoch
	m, _ = move(m, 2, transcriptTop+1)
	m, _ = move(m, 2, 0)
	offset := m.viewport.YOffset()
	if updated, stale := update(m, selectScrollMsg{epoch: first}); stale != nil || updated.viewport.YOffset() != offset {
		t.Fatal("a tick from the first chain scrolled")
	}
	// A tick from a drag that has since let go does nothing.
	m, _ = release(m, 2, 0)
	if _, stale := update(m, selectScrollMsg{epoch: m.selectEpoch}); stale != nil {
		t.Fatal("scrolling went on after the release")
	}
}

func TestWordAt(t *testing.T) {
	for _, c := range []struct {
		line string
		col  int
		want string
	}{
		{`call fmt.Println("hi") now`, 10, "fmt.Println"},
		{"see https://go.dev/doc.", 6, "https://go.dev/doc"},
		{"a snake_case_name here", 5, "snake_case_name"},
		{"the end.", 5, "end"},
		{"what...", 1, "what"},
		{"a ... b", 3, "..."},
		{"日本語 text", 2, "日本語"},
		{"a  b", 1, ""},
	} {
		from, to, ok := wordAt(c.line, c.col)
		got := ""
		if ok {
			got = ansi.Cut(c.line, from, to+1)
		}
		if got != c.want {
			t.Errorf("wordAt(%q, %d) = %q, want %q", c.line, c.col, got, c.want)
		}
	}
}

func TestCellsToBytes(t *testing.T) {
	// "a" takes cell 0, "界" cells 1 and 2, "b" cell 3.
	text := "a界b"
	for _, c := range []struct{ col, at, end int }{
		{0, 0, 0}, {1, 1, 1}, {2, 1, 4}, {3, 4, 4}, {4, 5, 5}, {9, 5, 5},
	} {
		if at, end := byteAt(text, c.col), byteEnd(text, c.col); at != c.at || end != c.end {
			t.Errorf("col %d: byteAt %d byteEnd %d, want %d %d", c.col, at, end, c.at, c.end)
		}
	}
}

func TestDragFurtherPastTheEdgeScrollsFaster(t *testing.T) {
	m := selectionModel(t)
	m.height = 8
	m.resize()
	m.refreshTranscript(true)
	m.viewport.SetYOffset(0)
	bottom := transcriptTop + m.viewport.Height() - 1
	m = pressAt(m, 2, bottom)
	m, _ = move(m, 2, bottom+3)
	if m, _ = update(m, selectScrollMsg{epoch: m.selectEpoch}); m.viewport.YOffset() != 3 {
		t.Fatalf("offset %d after a tick three rows past the bottom, want 3", m.viewport.YOffset())
	}
}
