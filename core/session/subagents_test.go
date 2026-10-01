package session

import (
	"path/filepath"
	"strings"
	"testing"

	"github.com/hizkifw/kon/core/tokens"
	"github.com/hizkifw/kon/core/typedid"
)

// reply is an assistant message that used and cost what it is given.
func reply(prompt tokens.Count, cost float64) Message {
	message := TextMessage(RoleAssistant, "done")
	message.Usage = &Usage{PromptTokens: prompt, CompletionTokens: 1, TotalTokens: prompt + 1, Cost: cost}
	return message
}

func newChild(t *testing.T, root, cwd string, parent typedid.SessionID) *Store {
	t.Helper()
	store, err := NewChild(root, cwd, "test", "system", parent)
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { store.Close() })
	return store
}

func mustAppend(t *testing.T, store *Store, message Message) {
	t.Helper()
	if _, err := store.AppendMessage(message); err != nil {
		t.Fatal(err)
	}
}

func TestTotalUsageCountsRepliesAndCompactions(t *testing.T) {
	answer := reply(100, 0.5)
	entries := []Entry{
		{Type: EntryTypeMessage, Message: &Message{Role: RoleUser}},
		{Type: EntryTypeMessage, Message: &answer},
		{Type: EntryTypeCompaction, Usage: &Usage{PromptTokens: 40, CompletionTokens: 4, CachedTokens: 30, Cost: 0.25}},
	}
	got := TotalUsage(entries)
	want := Usage{PromptTokens: 140, CompletionTokens: 5, TotalTokens: 101, CachedTokens: 30, Cost: 0.75}
	if got != want {
		t.Fatalf("usage = %+v, want %+v", got, want)
	}
}

// TestSubagentsCountEveryDescendantAsItWrites follows a subagent and its own
// subagent, reading what each appends, while sessions that are not beneath
// the parent are left out.
func TestSubagentsCountEveryDescendantAsItWrites(t *testing.T) {
	root, cwd := t.TempDir(), t.TempDir()
	parent := newChild(t, root, cwd, typedid.SessionID{})
	mustAppend(t, parent, TextMessage(RoleUser, "delegate"))
	mustAppend(t, parent, reply(1000, 8))
	subagents := NewSubagents(parent.Path(), parent.ID())
	if got := subagents.Usage(); got != (Usage{}) {
		t.Fatalf("usage before any subagent = %+v", got)
	}

	child := newChild(t, root, cwd, parent.ID())
	mustAppend(t, child, TextMessage(RoleUser, "subtask"))
	mustAppend(t, child, reply(100, 1))
	grandchild := newChild(t, root, cwd, child.ID())
	mustAppend(t, grandchild, TextMessage(RoleUser, "leaf"))
	mustAppend(t, grandchild, reply(10, 0.5))
	unrelated := newChild(t, root, cwd, typedid.SessionID{})
	mustAppend(t, unrelated, TextMessage(RoleUser, "other"))
	mustAppend(t, unrelated, reply(5, 100))
	if got := subagents.Usage(); got.Cost != 1.5 || got.PromptTokens != 110 {
		t.Fatalf("usage = %+v, want the child and grandchild only", got)
	}

	mustAppend(t, child, reply(200, 2))
	if got := subagents.Usage(); got.Cost != 3.5 || got.PromptTokens != 310 {
		t.Fatalf("usage after the child replied again = %+v", got)
	}
}

// TestSubagentsForgetADiscardedSession checks that a subagent that never got
// past its system prompt, whose session is removed on close, stops being read.
func TestSubagentsForgetADiscardedSession(t *testing.T) {
	root, cwd := t.TempDir(), t.TempDir()
	parent := newChild(t, root, cwd, typedid.SessionID{})
	mustAppend(t, parent, TextMessage(RoleUser, "delegate"))
	empty, err := NewChild(root, cwd, "test", "system", parent.ID())
	if err != nil {
		t.Fatal(err)
	}
	subagents := NewSubagents(parent.Path(), parent.ID())
	if got := subagents.Usage(); got != (Usage{}) || len(subagents.followed) != 1 {
		t.Fatalf("usage = %+v, following %d", got, len(subagents.followed))
	}
	if err := empty.Close(); err != nil {
		t.Fatal(err)
	}
	if got := subagents.Usage(); got != (Usage{}) || len(subagents.followed) != 0 {
		t.Fatalf("usage = %+v, still following %d", got, len(subagents.followed))
	}
}

// TestSubagentsSkipSessionsOlderThanTheParent checks that a file named for a
// time before the parent was created is never read: no subagent can be older
// than the session that started it.
func TestSubagentsSkipSessionsOlderThanTheParent(t *testing.T) {
	root, cwd := t.TempDir(), t.TempDir()
	parent := newChild(t, root, cwd, typedid.SessionID{})
	mustAppend(t, parent, TextMessage(RoleUser, "delegate"))
	// An impossible session that names the parent but predates it.
	older := writeSession(t, filepath.Dir(parent.Path()), 0, parent.ID(), benchBody(t, 1, 10))
	subagents := NewSubagents(parent.Path(), parent.ID())
	if got := subagents.Usage(); got != (Usage{}) {
		t.Fatalf("usage = %+v, counted a session older than the parent", got)
	}
	for name := range subagents.checked {
		if strings.Contains(name, older.String()) {
			t.Fatalf("read the header of %s, which predates the parent", name)
		}
	}
}
