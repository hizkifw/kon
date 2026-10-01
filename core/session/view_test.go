package session

import (
	"errors"
	"os"
	"testing"
)

func TestViewFollowsTheWriter(t *testing.T) {
	writer := keptSession(t)
	view, err := OpenView(writer.Path())
	if err != nil {
		t.Fatal(err)
	}
	if path := view.ActivePath(); len(path) != 2 || path[1].Message.Text() != "hello" {
		t.Fatalf("initial path = %+v", path)
	}
	if added, err := view.Poll(); err != nil || len(added) != 0 {
		t.Fatalf("idle poll = %v, %v", added, err)
	}
	if _, err := writer.AppendMessage(TextMessage(RoleAssistant, "hi")); err != nil {
		t.Fatal(err)
	}
	if _, err := writer.AppendTurnEnd(0); err != nil {
		t.Fatal(err)
	}
	added, err := view.Poll()
	if err != nil || len(added) != 2 || added[0].Message.Text() != "hi" || added[1].Type != EntryTypeTurnEnd {
		t.Fatalf("poll = %+v, %v", added, err)
	}
	if path := view.ActivePath(); len(path) != 4 {
		t.Fatalf("path after poll has %d entries, want 4", len(path))
	}
	if view.Free() {
		t.Fatal("view reports the session free while its writer is open")
	}
	if err := writer.Close(); err != nil {
		t.Fatal(err)
	}
	if !view.Free() {
		t.Fatal("view does not see the writer let go")
	}
}

// TestViewWaitsForTheNewline checks that a record the writer is partway through
// is neither returned nor trimmed, and is read once it completes.
func TestViewWaitsForTheNewline(t *testing.T) {
	writer := keptSession(t)
	view, err := OpenView(writer.Path())
	if err != nil {
		t.Fatal(err)
	}
	if _, err := writer.AppendMessage(TextMessage(RoleAssistant, "whole")); err != nil {
		t.Fatal(err)
	}
	b, err := os.ReadFile(writer.Path())
	if err != nil {
		t.Fatal(err)
	}
	// Cut the last record short, as a follower would see it mid-write.
	full := len(b)
	if err := os.Truncate(writer.Path(), int64(full-10)); err != nil {
		t.Fatal(err)
	}
	if added, err := view.Poll(); err != nil || len(added) != 0 {
		t.Fatalf("poll of a partial record = %+v, %v", added, err)
	}
	if err := os.WriteFile(writer.Path(), b, 0o600); err != nil {
		t.Fatal(err)
	}
	added, err := view.Poll()
	if err != nil || len(added) != 1 || added[0].Message.Text() != "whole" {
		t.Fatalf("poll after the record completed = %+v, %v", added, err)
	}
}

func TestViewReportsARemovedSession(t *testing.T) {
	writer := keptSession(t)
	view, err := OpenView(writer.Path())
	if err != nil {
		t.Fatal(err)
	}
	if err := os.Remove(writer.Path()); err != nil {
		t.Fatal(err)
	}
	if _, err := view.Poll(); !errors.Is(err, ErrRemoved) {
		t.Fatalf("poll error = %v, want ErrRemoved", err)
	}
}
