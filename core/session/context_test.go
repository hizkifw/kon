package session

import (
	"testing"

	"kon.kitsu.red/core/typedid"
)

// TestProjectNeedsNoStore projects a path held in memory, the way a program
// with its own storage builds a runner's context.
func TestProjectNeedsNoStore(t *testing.T) {
	entry := func(message Message) Entry {
		id, err := typedid.NewEntryID()
		if err != nil {
			t.Fatal(err)
		}
		return Entry{Type: EntryTypeMessage, ID: id, Message: &message}
	}
	system := entry(TextMessage(RoleSystem, "system"))
	old := entry(TextMessage(RoleUser, "old"))
	kept := entry(TextMessage(RoleUser, "kept"))
	compaction := Entry{Type: EntryTypeCompaction, ID: typedid.EntryID{}, Summary: "summary", FirstKeptEntryID: &kept.ID}
	context, err := Project([]Entry{system, old, kept, {Type: EntryTypeTurnStart}, compaction})
	if err != nil {
		t.Fatal(err)
	}
	var texts []string
	for _, item := range context {
		texts = append(texts, item.Message.Text())
	}
	if len(texts) != 3 || texts[0] != "system" || texts[1] != "summary" || !context[1].Summary || texts[2] != "kept" {
		t.Fatalf("Project = %q, want the system prompt, the summary, and the kept message", texts)
	}
	if context, err := Project(nil); err != nil || context != nil {
		t.Fatalf("Project(nil) = (%v, %v), want nothing", context, err)
	}
}
