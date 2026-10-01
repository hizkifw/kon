package sessions

import (
	"encoding/json"
	"path/filepath"
	"strings"
	"testing"

	"kon.kitsu.red/core/session"
	"kon.kitsu.red/core/tokens"
	"kon.kitsu.red/core/typedid"
)

// reply is an assistant message that used and cost what it is given.
func reply(prompt tokens.Count, cost float64) session.Message {
	message := session.TextMessage(session.RoleAssistant, "done")
	message.Usage = &session.Usage{PromptTokens: prompt, CompletionTokens: 1, TotalTokens: prompt + 1, Cost: cost}
	return message
}

func newChild(t *testing.T, root, cwd string, parent typedid.SessionID) *session.Store {
	t.Helper()
	store, err := New(root, cwd, "test", "system", parent)
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { store.Close() })
	return store
}

func mustAppend(t *testing.T, store *session.Store, message session.Message) {
	t.Helper()
	if _, err := store.AppendMessage(message); err != nil {
		t.Fatal(err)
	}
}

// TestSubagentsCountEveryDescendantAsItWrites follows a subagent and its own
// subagent, reading what each appends, while sessions that are not beneath
// the parent are left out.
func TestSubagentsCountEveryDescendantAsItWrites(t *testing.T) {
	root, cwd := t.TempDir(), t.TempDir()
	parent := newChild(t, root, cwd, typedid.SessionID{})
	mustAppend(t, parent, session.TextMessage(session.RoleUser, "delegate"))
	mustAppend(t, parent, reply(1000, 8))
	subagents := NewSubagents(parent.Path(), parent.ID())
	if got := subagents.Usage(); got != (session.Usage{}) {
		t.Fatalf("usage before any subagent = %+v", got)
	}

	child := newChild(t, root, cwd, parent.ID())
	mustAppend(t, child, session.TextMessage(session.RoleUser, "subtask"))
	mustAppend(t, child, reply(100, 1))
	grandchild := newChild(t, root, cwd, child.ID())
	mustAppend(t, grandchild, session.TextMessage(session.RoleUser, "leaf"))
	mustAppend(t, grandchild, reply(10, 0.5))
	unrelated := newChild(t, root, cwd, typedid.SessionID{})
	mustAppend(t, unrelated, session.TextMessage(session.RoleUser, "other"))
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
	mustAppend(t, parent, session.TextMessage(session.RoleUser, "delegate"))
	empty, err := New(root, cwd, "test", "system", parent.ID())
	if err != nil {
		t.Fatal(err)
	}
	subagents := NewSubagents(parent.Path(), parent.ID())
	if got := subagents.Usage(); got != (session.Usage{}) || len(subagents.followed) != 1 {
		t.Fatalf("usage = %+v, following %d", got, len(subagents.followed))
	}
	if err := empty.Close(); err != nil {
		t.Fatal(err)
	}
	if got := subagents.Usage(); got != (session.Usage{}) || len(subagents.followed) != 0 {
		t.Fatalf("usage = %+v, still following %d", got, len(subagents.followed))
	}
}

// TestSubagentsSkipSessionsOlderThanTheParent checks that a file named for a
// time before the parent was created is never read: no subagent can be older
// than the session that started it.
func TestSubagentsSkipSessionsOlderThanTheParent(t *testing.T) {
	root, cwd := t.TempDir(), t.TempDir()
	parent := newChild(t, root, cwd, typedid.SessionID{})
	mustAppend(t, parent, session.TextMessage(session.RoleUser, "delegate"))
	// An impossible session that names the parent but predates it.
	older := writeSession(t, filepath.Dir(parent.Path()), 0, parent.ID(), benchBody(t, 1, 10))
	subagents := NewSubagents(parent.Path(), parent.ID())
	if got := subagents.Usage(); got != (session.Usage{}) {
		t.Fatalf("usage = %+v, counted a session older than the parent", got)
	}
	for name := range subagents.checked {
		if strings.Contains(name, older.String()) {
			t.Fatalf("read the header of %s, which predates the parent", name)
		}
	}
}

// TestSubagentsCountASubagentNamedBeforeItsParent follows a grandchild whose
// file sorts before its parent's. Names carry the creation time to the
// millisecond, so two sessions made within one sort by their random IDs.
func TestSubagentsCountASubagentNamedBeforeItsParent(t *testing.T) {
	dir := t.TempDir()
	body := func(cost float64) []byte {
		b, err := json.Marshal(session.Entry{Type: session.EntryTypeMessage, ID: mustEntryID(t), Message: ptr(reply(10, cost))})
		if err != nil {
			t.Fatal(err)
		}
		return append(b, '\n')
	}
	parent := writeSession(t, dir, 0, typedid.SessionID{}, nil)
	child := writeSession(t, dir, 2, parent, body(1))
	writeSession(t, dir, 1, child, body(0.5))
	path, err := filepath.Glob(filepath.Join(dir, "*_"+parent.String()+fileSuffix))
	if err != nil || len(path) != 1 {
		t.Fatalf("parent file: %v, %v", path, err)
	}
	if got := NewSubagents(path[0], parent).Usage(); got.Cost != 1.5 {
		t.Fatalf("usage = %+v, want the child and the grandchild", got)
	}
}

func mustEntryID(t *testing.T) typedid.EntryID {
	t.Helper()
	id, err := typedid.NewEntryID()
	if err != nil {
		t.Fatal(err)
	}
	return id
}

func ptr[T any](v T) *T { return &v }
