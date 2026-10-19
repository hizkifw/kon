package ui

import (
	"context"
	"strings"
	"testing"

	"kon.kitsu.red/core/session"
	"kon.kitsu.red/internal/app"
)

func TestBangRunsShellCommand(t *testing.T) {
	m := newTestModel(t)
	m.input.SetValue("!  git status ")
	if !strings.HasSuffix(m.statusText(), "shell") {
		t.Fatalf("status = %q, want shell", m.statusText())
	}
	accented := false
	for _, line := range strings.Split(m.View().Content, "\n") {
		if strings.HasSuffix(strings.TrimSpace(plain(line)), "· shell") {
			accented = strings.Contains(line, fgSeq(colorAccent))
		}
	}
	if !accented {
		t.Fatal("shell mode is not shown in the accent color")
	}
	got := press(t, m, "enter")
	if !got.busy() || got.input.Value() != "" {
		t.Fatalf("busy = %v, input = %q", got.busy(), got.input.Value())
	}
	if last := got.transcript.blocks[len(got.transcript.blocks)-1]; last.kind != blockUser || last.text != "!git status" {
		t.Fatalf("prompt line = %+v", last)
	}
	if msg, ok := got.turn.wait()().(runMsg); !ok || !msg.done {
		t.Fatal("shell run did not report its end")
	}
	runtime := got.runtime.(*fakeRuntime)
	if command, _ := runtime.shell.Load().(string); command != "git status" || runtime.runs.Load() != 0 {
		t.Fatalf("shell ran %q, runs = %d", command, runtime.runs.Load())
	}
}

// While the agent works, Enter steers a command, to run at its next request,
// and Tab queues it, like prose.
func TestBangWhileBusySteersOrQueues(t *testing.T) {
	m := busyModel(t)
	m.input.SetValue("!  ls ")
	m = press(t, m, "enter")
	m.input.SetValue("!make test")
	m = press(t, m, "tab")
	if got := m.inbox.Pending(); len(got) != 1 || got[0] != "!ls" || len(m.queued) != 1 || m.queued[0] != "!make test" {
		t.Fatalf("steering = %q, queued = %q", got, m.queued)
	}
	if view := plain(m.View().Content); !strings.Contains(view, "↳ steer  !ls") || !strings.Contains(view, "⏵ queue  !make test") {
		t.Fatalf("pending strip missing from view:\n%s", view)
	}
	m.input.SetValue("!")
	m = press(t, m, "enter")
	if len(m.inbox.Pending()) != 1 || !strings.Contains(m.message, "type a shell command") {
		t.Fatalf("an empty command was steered: steering = %q, status = %q", m.inbox.Pending(), m.message)
	}
}

func TestQueuedShellCommandRunsInTurn(t *testing.T) {
	m := busyModel(t)
	m.queued = []string{"!ls", "later"}
	updated, _ := m.Update(turnDone(m, nil))
	m = updated.(Model)
	if !m.busy() || len(m.queued) != 1 || m.queued[0] != "later" {
		t.Fatalf("busy = %v, queued = %q", m.busy(), m.queued)
	}
	if msg, ok := m.turn.wait()().(runMsg); !ok || !msg.done {
		t.Fatal("shell run did not report its end")
	}
	runtime := m.runtime.(*fakeRuntime)
	if command, _ := runtime.shell.Load().(string); command != "ls" || runtime.runs.Load() != 0 {
		t.Fatalf("shell ran %q, runs = %d", command, runtime.runs.Load())
	}
}

// Steering the runner never read is sent as it would have been delivered: a
// command on its own, and the prose around it joined.
func TestLeftoverShellSteerRunsOnItsOwn(t *testing.T) {
	m := busyModel(t)
	m.inbox.Push("a")
	m.inbox.Push("b")
	m.inbox.PushCall("!ls", app.ShellCall("ls"))
	m.inbox.Push("c")
	m.syncSteering()
	updated, _ := m.Update(turnDone(m, context.Canceled))
	m = updated.(Model)
	if !m.busy() || len(m.queued) != 2 || m.queued[0] != "!ls" || m.queued[1] != "c" {
		t.Fatalf("busy = %v, queued = %q", m.busy(), m.queued)
	}
	if last := m.transcript.blocks[len(m.transcript.blocks)-1]; last.kind != blockUser || last.text != "a\n\nb" {
		t.Fatalf("next run started with %+v, want the leading prose joined", last)
	}
}

func TestShellCommandHasNoMentions(t *testing.T) {
	if _, _, _, ok := mentionAt("!git log @{u}", len("!git log @{u}")); ok {
		t.Fatal("an @ in a shell command opened a file mention")
	}
}

func TestReplayShowsShellCommandAsItRan(t *testing.T) {
	m := newTestModel(t)
	text := "The user ran this shell command themselves and shared its output with you.\n\n<user-shell-command>\nls\n</user-shell-command>\n<user-shell-output>\ngo.mod\nexit code: 0 (took 1ms)\n</user-shell-output>\n\nwhat is this?"
	m.applyHistory([]session.Entry{{Type: session.EntryTypeMessage, Message: &session.Message{Role: session.RoleUser, Parts: []session.Part{{Type: session.PartText, Text: text}}}}})
	var kinds []blockKind
	for _, b := range m.transcript.blocks {
		kinds = append(kinds, b.kind)
	}
	blocks := m.transcript.blocks
	if len(blocks) != 4 || blocks[0].text != "!ls" || blocks[1].kind != blockTool || blocks[2].kind != blockResult || blocks[3].text != "what is this?" {
		t.Fatalf("blocks = %v", kinds)
	}
	if lines := blocks[2].display.Lines; len(lines) != 1 || lines[0] != "go.mod" {
		t.Fatalf("output lines = %q", lines)
	}
}
