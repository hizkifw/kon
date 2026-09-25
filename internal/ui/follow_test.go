package ui

import (
	"context"
	"strings"
	"testing"
	"time"

	"github.com/hizkifw/kon/internal/app"
	"github.com/hizkifw/kon/internal/history"
	"github.com/hizkifw/kon/internal/session"
	"github.com/hizkifw/kon/internal/typedid"
)

// newFollowingModel opens a session another kon has open, with its writer
// partway through a turn.
func newFollowingModel(t *testing.T) (Model, *fakeRuntime) {
	t.Helper()
	id, err := typedid.NewSessionID()
	if err != nil {
		t.Fatal(err)
	}
	model := app.Model{Name: "fast", WireFormat: "openai", ExternalID: "gpt", ContextWindow: 100}
	runtime := &fakeRuntime{
		state: app.State{Active: model, Phase: app.PhaseFollowing}, models: []app.Model{model}, id: id,
		entries: []session.Entry{turnStart(), replayMessage(session.RoleUser, "fix it")},
	}
	m := New(context.Background(), "/tmp", "/tmp/config.json", runtime, history.New(t.TempDir()+"/history.jsonl"), nil)
	m.width, m.height = 80, 24
	m.resize()
	return m, runtime
}

func transcriptText(m Model) string { return plain(strings.Join(m.transcript.linesFor(80), "\n")) }

func TestFollowingSessionOpensReadOnlyWithItsTurnRunning(t *testing.T) {
	m, _ := newFollowingModel(t)
	if m.follow == nil || m.status != "read-only: open in another session · working" {
		t.Fatalf("follow = %v, status = %q", m.follow, m.status)
	}
	if got := transcriptText(m); !strings.Contains(got, "fix it") || strings.Contains(got, "Stopped abruptly") {
		t.Fatalf("a running turn was replayed as stopped:\n%s", got)
	}
	if strings.Contains(transcriptText(m), "needs configuration") {
		t.Fatal("a followed session introduced itself as unconfigured")
	}
}

func TestFollowAppendsWhatTheWriterWrote(t *testing.T) {
	m, runtime := newFollowingModel(t)
	updated, cmd := m.Update(followedMsg{epoch: m.followEpoch, followed: app.Followed{
		Session: runtime.id,
		Entries: []session.Entry{replayMessage(session.RoleAssistant, "fixed"), turnEnd(3 * time.Second)},
	}})
	m = updated.(Model)
	if cmd == nil {
		t.Fatal("following stopped after a read")
	}
	if got := transcriptText(m); !strings.Contains(got, "fixed") || !strings.Contains(got, "Worked for 3s") {
		t.Fatalf("transcript after follow:\n%s", got)
	}
	if m.status != "read-only: open in another session" {
		t.Fatalf("status = %q", m.status)
	}
	updated, _ = m.Update(followedMsg{epoch: m.followEpoch, followed: app.Followed{Session: runtime.id, Free: true}})
	if status := updated.(Model).status; status != "read-only: session is free, send a prompt to continue here" {
		t.Fatalf("status once free = %q", status)
	}
}

func TestFollowDropsAStaleRead(t *testing.T) {
	m, runtime := newFollowingModel(t)
	before := transcriptText(m)
	stale := followedMsg{epoch: m.followEpoch - 1, followed: app.Followed{Session: runtime.id, Entries: []session.Entry{replayMessage(session.RoleAssistant, "stale")}}}
	updated, cmd := m.Update(stale)
	if cmd != nil || transcriptText(updated.(Model)) != before {
		t.Fatal("a read from an ended follow was applied")
	}
}

func TestPromptWhileHeldStaysReadOnly(t *testing.T) {
	m, runtime := newFollowingModel(t)
	runtime.takeOverErr = session.ErrInUse
	m.input.SetValue("my turn")
	updated, _ := m.submit()
	m = updated.(Model)
	if m.status != "read-only: still open in another session" || m.follow == nil || m.busy || runtime.runs.Load() != 0 {
		t.Fatalf("status = %q, following = %v, busy = %v", m.status, m.follow != nil, m.busy)
	}
	if m.input.Value() != "my turn" {
		t.Fatal("a refused prompt was cleared from the input")
	}
	// The refusal stays up until the follow state has something new to say.
	updated, _ = m.Update(followedMsg{epoch: m.followEpoch, followed: app.Followed{Session: runtime.id}})
	if status := updated.(Model).status; status != "read-only: still open in another session" {
		t.Fatalf("status after an idle read = %q", status)
	}
}

func TestPromptTakesOverAFreeSession(t *testing.T) {
	m, runtime := newFollowingModel(t)
	runtime.missed = []session.Entry{replayMessage(session.RoleAssistant, "last words")}
	m.input.SetValue("my turn")
	updated, _ := m.submit()
	m = updated.(Model)
	if m.follow != nil || !m.busy {
		t.Fatalf("following = %v, busy = %v after taking over", m.follow != nil, m.busy)
	}
	got := transcriptText(m)
	// The writer's unfinished turn is closed before this kon's prompt.
	lastWords, stopped, prompt := strings.Index(got, "last words"), strings.Index(got, "Stopped abruptly"), strings.Index(got, "my turn")
	if lastWords < 0 || stopped < lastWords || prompt < stopped {
		t.Fatalf("transcript after taking over:\n%s", got)
	}
}
