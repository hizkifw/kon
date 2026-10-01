package agent

import (
	"context"
	"testing"
	"time"

	"kon.kitsu.red/core/session"
	"kon.kitsu.red/core/tokens"
	"kon.kitsu.red/core/tool"
	"kon.kitsu.red/core/typedid"
)

// sliceStore keeps a conversation in a slice, as a program with its own
// storage would, using only what core exports.
type sliceStore struct{ entries []session.Entry }

func (s *sliceStore) Context() ([]session.ContextMessage, error) { return session.Project(s.entries) }
func (s *sliceStore) ActivePath() []session.Entry                { return s.entries }

func (s *sliceStore) append(entry session.Entry) (typedid.EntryID, error) {
	id, err := typedid.NewEntryID()
	if err != nil {
		return typedid.EntryID{}, err
	}
	entry.ID = id
	s.entries = append(s.entries, entry)
	return id, nil
}

func (s *sliceStore) AppendMessage(message session.Message) (typedid.EntryID, error) {
	return s.append(session.Entry{Type: session.EntryTypeMessage, Message: &message})
}

func (s *sliceStore) AppendCompaction(summary string, firstKeptID typedid.EntryID, tokensBefore tokens.Count, estimated bool, usage *session.Usage) (typedid.EntryID, error) {
	return s.append(session.Entry{Type: session.EntryTypeCompaction, Summary: summary, FirstKeptEntryID: &firstKeptID, TokensBefore: tokensBefore, Usage: usage})
}

func (s *sliceStore) AppendTurnStart() (typedid.EntryID, error) {
	return s.append(session.Entry{Type: session.EntryTypeTurnStart})
}

func (s *sliceStore) AppendTurnEnd(time.Duration) (typedid.EntryID, error) {
	return s.append(session.Entry{Type: session.EntryTypeTurnEnd})
}

func (s *sliceStore) SaveMedia([]byte, string) (session.Part, error) {
	return session.Part{}, nil
}

func TestRunnerRunsOnAnyStore(t *testing.T) {
	store := &sliceStore{}
	if _, err := store.AppendMessage(session.TextMessage(session.RoleSystem, "system")); err != nil {
		t.Fatal(err)
	}
	fake := &fakeProvider{}
	runner := New(Config{Provider: fake, Store: store, Tools: tool.NewExecutor(tool.NewRegistry(), "", nil)})
	if err := runner.Run(context.Background(), "hello", nil, func(Event) {}); err != nil {
		t.Fatal(err)
	}
	if len(fake.streams) != 1 || len(fake.streams[0]) != 2 || fake.streams[0][1].Text() != "hello" {
		t.Fatalf("model was sent %#v, want the system prompt and the prompt", fake.streams)
	}
	if last := store.entries[len(store.entries)-1]; last.Type != session.EntryTypeTurnEnd {
		t.Fatalf("last entry = %q, want the turn closed", last.Type)
	}
}
