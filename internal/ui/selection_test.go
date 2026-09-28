package ui

import (
	"strings"
	"testing"

	tea "charm.land/bubbletea/v2"
	"github.com/charmbracelet/x/ansi"
)

// The commands these tests run only work out a selection's text. The one that
// copies it is never run: it would write to the clipboard of whoever runs the
// tests, and inside tmux to their paste buffers.

const selectionReply = "Use **fmt.Println** to print.\n\n```go\nfunc main() {\n\tx()\n}\n```\n\n- one\n- two"

func selectionModel(t *testing.T) Model {
	t.Helper()
	m := newTestModel(t)
	m.transcript.add(block{kind: blockUser, text: "how do I print?"})
	m.transcript.add(block{kind: blockAssistant, text: selectionReply})
	m.refreshTranscript(true)
	return m
}

// cellOf returns the screen cell showing the first byte of text, which must be
// on screen and preceded only by single-cell characters on its line.
func cellOf(t *testing.T, m Model, text string) (x, y int) {
	t.Helper()
	for i, line := range m.transcript.lines {
		if col := strings.Index(ansi.Strip(line), text); col >= 0 {
			return col, transcriptTop + i - m.viewport.YOffset()
		}
	}
	t.Fatalf("%q is not in the transcript", text)
	return 0, 0
}

// drag presses at one cell, moves to another, and releases there.
func drag(m Model, fromX, fromY, toX, toY int) (Model, tea.Cmd) {
	updated, _ := m.Update(tea.MouseClickMsg{X: fromX, Y: fromY, Button: tea.MouseLeft})
	updated, _ = updated.(Model).Update(tea.MouseMotionMsg{X: toX, Y: toY, Button: tea.MouseLeft})
	updated, cmd := updated.(Model).Update(tea.MouseReleaseMsg{X: toX, Y: toY, Button: tea.MouseLeft})
	return updated.(Model), cmd
}

// selectedText runs the command that works out a selection's text.
func selectedText(t *testing.T, cmd tea.Cmd) string {
	t.Helper()
	if cmd == nil {
		t.Fatal("the drag copied nothing")
	}
	msg, ok := cmd().(selectionTextMsg)
	if !ok {
		t.Fatalf("the drag's command did not work out a selection")
	}
	return msg.text
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
	m := newTestModel(t)
	m.transcript.add(toolDoneBlock("read", `{"path":"/tmp/a.go"}`, "func main() {\n\tif true {\n\t}\n}", false, "/tmp"))
	m.refreshTranscript(true)
	x, y := cellOf(t, m, "func main")
	endX, endY := cellOf(t, m, "if true")
	_, cmd := drag(m, x, y, endX+len("if true {")-1, endY)
	if got, want := selectedText(t, cmd), "func main() {\n    if true {"; got != want {
		t.Fatalf("copied %q, want %q", got, want)
	}
}

func TestClickWithoutDragCopiesNothing(t *testing.T) {
	m := selectionModel(t)
	x, y := cellOf(t, m, "fmt.Println")
	m, cmd := drag(m, x, y, x, y)
	if cmd != nil || m.transcript.selection != nil {
		t.Fatalf("a click selected: cmd = %v, selection = %+v", cmd, m.transcript.selection)
	}
}

func TestPressOffTheTranscriptSelectsNothing(t *testing.T) {
	m := selectionModel(t)
	for _, y := range []int{0, transcriptTop + m.viewport.Height()} {
		if m, _ := drag(m, 2, y, 8, y); m.transcript.selection != nil {
			t.Fatalf("a drag on row %d selected", y)
		}
	}
}

func TestSelectionIsHighlightedUntilTheNextClick(t *testing.T) {
	m := selectionModel(t)
	if strings.Contains(m.View().Content, "\x1b[7m") {
		t.Fatal("reverse video with nothing selected")
	}
	x, y := cellOf(t, m, "fmt.Println")
	m, _ = drag(m, x, y, x+3, y)
	if !strings.Contains(m.View().Content, "\x1b[7mfmt.") {
		t.Fatalf("selection not highlighted:\n%q", m.View().Content)
	}
	updated, _ := m.Update(tea.MouseClickMsg{X: 1, Y: y, Button: tea.MouseLeft})
	updated, _ = updated.(Model).Update(tea.MouseReleaseMsg{X: 1, Y: y, Button: tea.MouseLeft})
	if strings.Contains(updated.(Model).View().Content, "\x1b[7m") {
		t.Fatal("a click left the highlight")
	}
}

func TestResizeDropsTheSelection(t *testing.T) {
	m := selectionModel(t)
	x, y := cellOf(t, m, "fmt.Println")
	m, _ = drag(m, x, y, x+3, y)
	updated, _ := m.Update(tea.WindowSizeMsg{Width: 60, Height: 24})
	if updated.(Model).transcript.selection != nil {
		t.Fatal("a rewrap kept a selection of lines that moved")
	}
}

func TestDragPastTheTopScrollsUp(t *testing.T) {
	m := selectionModel(t)
	m.height = 8
	m.resize()
	m.refreshTranscript(true)
	m.viewport.GotoBottom()
	before := m.viewport.YOffset()
	updated, _ := m.Update(tea.MouseClickMsg{X: 2, Y: transcriptTop + 1, Button: tea.MouseLeft})
	updated, cmd := updated.(Model).Update(tea.MouseMotionMsg{X: 2, Y: 0, Button: tea.MouseLeft})
	if cmd == nil {
		t.Fatal("a drag past the top did not start scrolling")
	}
	m = updated.(Model)
	updated, next := m.Update(selectScrollMsg{epoch: m.selectEpoch})
	m = updated.(Model)
	if m.viewport.YOffset() != before-1 || next == nil {
		t.Fatalf("offset %d after a tick, was %d; next tick %v", m.viewport.YOffset(), before, next)
	}
	if head := m.transcript.selection.head; head.line != m.viewport.YOffset() {
		t.Fatalf("head on line %d, top line is %d", head.line, m.viewport.YOffset())
	}
	// Back on the transcript and past the top again, the drag scrolls on a
	// new chain of ticks, and one still in flight from the first is dropped,
	// or the two would scroll twice as fast.
	first := m.selectEpoch
	updated, _ = m.Update(tea.MouseMotionMsg{X: 2, Y: transcriptTop + 1, Button: tea.MouseLeft})
	updated, _ = updated.(Model).Update(tea.MouseMotionMsg{X: 2, Y: 0, Button: tea.MouseLeft})
	m = updated.(Model)
	offset := m.viewport.YOffset()
	if updated, stale := m.Update(selectScrollMsg{epoch: first}); stale != nil || updated.(Model).viewport.YOffset() != offset {
		t.Fatal("a tick from the first chain scrolled")
	}
	// A tick from a drag that has since let go does nothing.
	updated, _ = m.Update(tea.MouseReleaseMsg{X: 2, Y: 0, Button: tea.MouseLeft})
	m = updated.(Model)
	if _, stale := m.Update(selectScrollMsg{epoch: m.selectEpoch}); stale != nil {
		t.Fatal("scrolling went on after the release")
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
