package ui

import (
	"strings"
	"testing"

	"kon.kitsu.red/core/session"
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

func TestBangWhileBusyIsNotSteeredOrQueued(t *testing.T) {
	m := busyModel(t)
	m.input.SetValue("!ls")
	m = press(t, m, "enter")
	m = press(t, m, "tab")
	if len(m.inbox.Pending()) != 0 || len(m.queued) != 0 || m.input.Value() != "!ls" {
		t.Fatalf("steering = %q, queued = %q, input = %q", m.inbox.Pending(), m.queued, m.input.Value())
	}
	if !strings.Contains(m.message, "wait for the turn") {
		t.Fatalf("status = %q", m.message)
	}
}

func TestShellCommandHasNoMentions(t *testing.T) {
	if _, _, _, ok := mentionAt("!git log @{u}", len("!git log @{u}")); ok {
		t.Fatal("an @ in a shell command opened a file mention")
	}
}

func TestReplayShowsShellCommandAsItRan(t *testing.T) {
	m := newTestModel(t)
	text := "The user ran this shell command themselves and shared its output with you.\n\n<user-shell-command>\nls\n</user-shell-command>\n<user-shell-output>\ngo.mod\nexit code: 0 (took 1ms)\n</user-shell-output>"
	m.applyHistory([]session.Entry{{Type: session.EntryTypeMessage, Message: &session.Message{Role: session.RoleUser, Parts: []session.Part{{Type: session.PartText, Text: text}}}}})
	var kinds []blockKind
	for _, b := range m.transcript.blocks {
		kinds = append(kinds, b.kind)
	}
	blocks := m.transcript.blocks
	if len(blocks) != 3 || blocks[0].text != "!ls" || blocks[1].kind != blockTool || blocks[2].kind != blockResult {
		t.Fatalf("blocks = %v", kinds)
	}
	if lines := blocks[2].display.Lines; len(lines) != 1 || lines[0] != "go.mod" {
		t.Fatalf("output lines = %q", lines)
	}
}
