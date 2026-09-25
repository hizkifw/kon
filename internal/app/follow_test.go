package app

import (
	"context"
	"errors"
	"testing"

	"github.com/hizkifw/kon/internal/agent"
	"github.com/hizkifw/kon/internal/session"
)

// liveSession opens a session with a conversation in one runtime and leaves
// it open, as another kon would.
func liveSession(t *testing.T) (*Runtime, func() *Runtime) {
	t.Helper()
	paths, cwd := resumePaths(t), t.TempDir()
	writer, err := New(twoModels(), paths, cwd, "test")
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = writer.Close() })
	if _, err := writer.store.AppendMessage(session.TextMessage(session.RoleUser, "hello")); err != nil {
		t.Fatal(err)
	}
	if err := writer.SwitchModel("review"); err != nil {
		t.Fatal(err)
	}
	id := writer.SessionID()
	follow := func() *Runtime {
		t.Helper()
		follower, err := NewResumedID(twoModels(), paths, cwd, "test", id)
		if err != nil {
			t.Fatal(err)
		}
		t.Cleanup(func() { _ = follower.Close() })
		return follower
	}
	return writer, follow
}

func TestResumingAnOpenSessionFollowsIt(t *testing.T) {
	writer, follow := liveSession(t)
	follower := follow()
	state := follower.State()
	if !state.Following() || state.Active.Name != "review" {
		t.Fatalf("state = %+v, want following on the writer's model", state)
	}
	if follower.SessionID() != writer.SessionID() || len(follower.SessionHistory()) == 0 {
		t.Fatal("follower does not show the followed session")
	}
	for name, err := range map[string]error{
		"run":     follower.Run(context.Background(), "hi", nil, func(agent.Event) {}),
		"compact": follower.Compact(context.Background(), func(agent.Event) {}),
		"switch":  follower.SwitchModel("fast"),
	} {
		if !errors.Is(err, ErrReadOnly) {
			t.Fatalf("%s while following = %v, want ErrReadOnly", name, err)
		}
	}
	if _, err := follower.CycleEffort(); !errors.Is(err, ErrReadOnly) {
		t.Fatalf("effort while following = %v, want ErrReadOnly", err)
	}

	if _, err := writer.store.AppendMessage(session.TextMessage(session.RoleAssistant, "hi there")); err != nil {
		t.Fatal(err)
	}
	followed, err := follower.Follow()
	if err != nil || followed.Session != writer.SessionID() || followed.Free || len(followed.Entries) != 1 {
		t.Fatalf("follow = %+v, %v", followed, err)
	}
	if _, err := follower.TakeOver(); !errors.Is(err, session.ErrInUse) {
		t.Fatalf("take over while held = %v, want ErrInUse", err)
	}
	if !follower.State().Following() {
		t.Fatal("a refused take over left the following phase")
	}
}

func TestTakeOverAfterTheWriterLetsGo(t *testing.T) {
	writer, follow := liveSession(t)
	follower := follow()
	if _, err := writer.store.AppendMessage(session.TextMessage(session.RoleAssistant, "last words")); err != nil {
		t.Fatal(err)
	}
	if err := writer.Close(); err != nil {
		t.Fatal(err)
	}
	followed, err := follower.Follow()
	if err != nil || !followed.Free {
		t.Fatalf("follow after the writer closed = %+v, %v", followed, err)
	}
	// The entry written before close arrived with that Follow, so taking over
	// has nothing left to report.
	missed, err := follower.TakeOver()
	if err != nil || len(missed) != 0 {
		t.Fatalf("take over = %+v, %v", missed, err)
	}
	if state := follower.State(); !state.Ready() || state.Active.Name != "review" {
		t.Fatalf("state after take over = %+v", state)
	}
	if follower.store == nil || follower.view != nil || follower.runner == nil {
		t.Fatal("take over did not install a writer")
	}
}

func TestTakeOverReportsWhatTheLastFollowMissed(t *testing.T) {
	writer, follow := liveSession(t)
	follower := follow()
	if _, err := writer.store.AppendMessage(session.TextMessage(session.RoleAssistant, "unseen")); err != nil {
		t.Fatal(err)
	}
	if err := writer.Close(); err != nil {
		t.Fatal(err)
	}
	missed, err := follower.TakeOver()
	if err != nil || len(missed) != 1 || missed[0].Message.Text() != "unseen" {
		t.Fatalf("missed = %+v, %v", missed, err)
	}
}

func TestNewSessionLeavesAFollowedSession(t *testing.T) {
	_, follow := liveSession(t)
	follower := follow()
	if err := follower.NewSession(); err != nil {
		t.Fatal(err)
	}
	if state := follower.State(); !state.Ready() || follower.view != nil {
		t.Fatalf("state after /new = %+v", state)
	}
}
