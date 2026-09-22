package ui

import (
	"encoding/json"
	"strings"
	"testing"

	tea "charm.land/bubbletea/v2"
	"github.com/hizkifw/kon/internal/agent"
	"github.com/hizkifw/kon/internal/tools"
)

// TestToolOutputEventStreamsIntoTranscript drives a running shell call's live
// display: snapshots replace the request block's lines, and the done event
// finalizes the display through the owning tool.
func TestToolOutputEventStreamsIntoTranscript(t *testing.T) {
	model := newTestModel(t)
	model.busy = true
	model.runEvents = make(chan tea.Msg)

	start := agent.Event{Kind: agent.EventToolStart, Tool: "shell", Arguments: `{"command":"./build"}`}
	updated, _ := model.Update(runEventMsg{event: start})
	model = updated.(Model)

	// First snapshot: two lines of build output.
	snap := tools.Display{
		State: tools.StateRunning, Summary: "./build",
		Lines: []string{"go build ./..."}, More: 0,
	}
	updated, _ = model.Update(runEventMsg{event: agent.Event{Kind: agent.EventToolOutput, Tool: "shell", Display: snap}})
	model = updated.(Model)
	updated, _ = model.Update(flushTranscriptMsg{})
	model = updated.(Model)
	got := plain(model.viewport.View())
	if !strings.Contains(got, "go build ./...") {
		t.Fatalf("live shell output missing from the transcript: %q", got)
	}

	// A later snapshot with more output replaces the previous one.
	snap.Lines = []string{"go build ./...", "compile errors below"}
	updated, _ = model.Update(runEventMsg{event: agent.Event{Kind: agent.EventToolOutput, Tool: "shell", Display: snap}})
	model = updated.(Model)
	updated, _ = model.Update(flushTranscriptMsg{})
	model = updated.(Model)
	got = plain(model.viewport.View())
	if !strings.Contains(got, "compile errors below") {
		t.Fatalf("later snapshot not shown: %q", got)
	}

	// The done event finalizes: the owned display replaces the running one.
	done := agent.Event{
		Kind: agent.EventToolDone, Tool: "shell", Arguments: `{"command":"./build"}`,
		Text: "exit code: 0 (took 1.0s)", IsError: false,
	}
	updated, _ = model.Update(runEventMsg{event: done})
	model = updated.(Model)
	updated, _ = model.Update(flushTranscriptMsg{})
	model = updated.(Model)
	got = plain(model.viewport.View())
	if !strings.Contains(got, "exit 0") || !strings.Contains(got, "took 1.0s") || !strings.Contains(got, "✓") {
		t.Fatalf("done display not finalized: %q", got)
	}
	if strings.Contains(got, "compile errors below") {
		t.Fatalf("stale live output survived the done event: %q", got)
	}
}

// TestToolOutputEventWithoutRunningToolIsDropped guards the ordering race: a
// snapshot that arrives after the result block must not resurrect a live
// display.
func TestToolOutputEventWithoutRunningToolIsDropped(t *testing.T) {
	var tr transcript
	tr.cwd = "/tmp"
	tr.add(toolCallBlock("shell", `{"command":"./build"}`, "/tmp"))
	tr.add(toolDoneBlock("shell", `{"command":"./build"}`, "exit code: 0 (took 1.0s)", false, "/tmp"))
	tr.updateToolLive(tools.Display{State: tools.StateRunning, Summary: "./build", Lines: []string{"late"}})
	got := plain(tr.render(80))
	if strings.Contains(got, "late") {
		t.Fatalf("late snapshot resurrected a live display: %q", got)
	}
	if !strings.Contains(got, "exit 0") {
		t.Fatalf("done display lost: %q", got)
	}
}

// TestDescribeFallbackUnknownTool verifies unknown tools degrade to the
// generic display instead of panicking.
func TestDescribeFallbackUnknownTool(t *testing.T) {
	d := tools.Describe("mystery", json.RawMessage(`{"a":1}`), "some output", false, "/tmp")
	if d.Summary != `{"a":1}` || d.State != tools.StateDone || len(d.Lines) != 1 || d.Lines[0] != "some output" {
		t.Fatalf("fallback display = %#v", d)
	}
	failed := tools.Describe("mystery", json.RawMessage(`{"a":1}`), "boom", true, "/tmp")
	if failed.State != tools.StateFailed || failed.Lines[0] != "boom" {
		t.Fatalf("fallback failure display = %#v", failed)
	}
}
