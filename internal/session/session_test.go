package session

import (
	"encoding/json"
	"os"
	"strings"
	"testing"

	"github.com/hizkifw/kon/internal/typedid"
)

func TestDiscoverFindsSessionsNewestFirst(t *testing.T) {
	root, cwd := t.TempDir(), t.TempDir()
	first, err := New(root, cwd, "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	if _, err := first.AppendMessage(Message{Role: RoleUser, Content: "one"}); err != nil {
		t.Fatal(err)
	}
	first.Close()
	second, err := New(root, cwd, "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	secondID := second.ID()
	second.Close()

	// Another workspace must not leak into this one's results.
	other, err := New(root, t.TempDir(), "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	other.Close()

	summaries, err := Discover(root, cwd)
	if err != nil {
		t.Fatal(err)
	}
	if len(summaries) != 2 {
		t.Fatalf("Discover returned %d sessions, want 2", len(summaries))
	}
	if summaries[0].ID != secondID {
		t.Fatalf("newest session = %s, want %s", summaries[0].ID, secondID)
	}
	if summaries[0].Entries != 1 {
		t.Fatalf("entry count = %d, want 1 (the system message)", summaries[0].Entries)
	}
	if summaries[0].CWD == "" {
		t.Fatal("summary is missing its working directory")
	}
}

func TestLatestAndFind(t *testing.T) {
	root, cwd := t.TempDir(), t.TempDir()
	if _, ok, err := Latest(root, cwd); err != nil || ok {
		t.Fatalf("Latest on empty root = (%v, %v), want (zero, false)", ok, err)
	}
	first, err := New(root, cwd, "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	firstID := first.ID()
	first.Close()

	latest, ok, err := Latest(root, cwd)
	if err != nil || !ok || latest.ID != firstID {
		t.Fatalf("Latest = (%#v, %v, %v), want the only session", latest, ok, err)
	}
	found, err := Find(root, cwd, firstID)
	if err != nil || found.Path != latest.Path {
		t.Fatalf("Find = (%#v, %v)", found, err)
	}
	missing, err := typedid.ParseSessionID("ses_00000000000000000000")
	if err != nil {
		t.Fatal(err)
	}
	if _, err := Find(root, cwd, missing); err == nil {
		t.Fatal("Find returned a session that does not exist")
	}
}

func TestDiscoverSkipsUnreadableFiles(t *testing.T) {
	root, cwd := t.TempDir(), t.TempDir()
	store, err := New(root, cwd, "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	path := store.Path()
	store.Close()
	if err := os.WriteFile(path[:len(path)-len(".jsonl")]+"-corrupt.jsonl", []byte("not a session\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	summaries, err := Discover(root, cwd)
	if err != nil {
		t.Fatal(err)
	}
	if len(summaries) != 1 {
		t.Fatalf("Discover returned %d sessions, want the one valid file", len(summaries))
	}
}

func TestActivePathIncludesMessagesInOrder(t *testing.T) {
	store, err := New(t.TempDir(), t.TempDir(), "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	defer store.Close()
	if _, err := store.AppendMessage(Message{Role: RoleUser, Content: "hello"}); err != nil {
		t.Fatal(err)
	}
	path := store.ActivePath()
	if len(path) != 2 || path[0].Message.Role != RoleSystem || path[1].Message.Content != "hello" {
		t.Fatalf("ActivePath = %#v", path)
	}
}

func TestCompactionProjectsRetainedMessages(t *testing.T) {
	store, err := New(t.TempDir(), t.TempDir(), "test", "system prompt")
	if err != nil {
		t.Fatal(err)
	}
	defer store.Close()
	if _, err := store.AppendMessage(Message{Role: RoleUser, Content: "old question"}); err != nil {
		t.Fatal(err)
	}
	if _, err := store.AppendMessage(Message{Role: RoleAssistant, Content: "old answer"}); err != nil {
		t.Fatal(err)
	}
	kept, err := store.AppendMessage(Message{Role: RoleUser, Content: "new question"})
	if err != nil {
		t.Fatal(err)
	}
	if _, err := store.AppendMessage(Message{Role: RoleAssistant, Content: "new answer"}); err != nil {
		t.Fatal(err)
	}
	if _, err := store.AppendCompaction("old work summary", kept, 1000, false, nil); err != nil {
		t.Fatal(err)
	}
	context, err := store.Context()
	if err != nil {
		t.Fatal(err)
	}
	if len(context) != 4 {
		t.Fatalf("context has %d messages, want 4", len(context))
	}
	if context[0].Message.Content != "system prompt" {
		t.Fatalf("system message was rewritten: %q", context[0].Message.Content)
	}
	if !context[1].Summary || !strings.Contains(context[1].Message.Content, "old work summary") {
		t.Fatalf("compaction summary not projected as its own message: %#v", context[1])
	}
	if context[2].Message.Content != "new question" || context[3].Message.Content != "new answer" {
		t.Fatalf("wrong retained messages: %#v", context)
	}
}

// TestCompactionKeepsSystemPromptStable guards the prompt-cache contract: the
// system message must stay byte-identical across repeated compactions so the
// cached leading prefix survives.
func TestCompactionKeepsSystemPromptStable(t *testing.T) {
	store, err := New(t.TempDir(), t.TempDir(), "test", "stable system prompt")
	if err != nil {
		t.Fatal(err)
	}
	defer store.Close()
	for _, content := range []string{"q1", "a1", "q2", "a2"} {
		role := RoleUser
		if strings.HasPrefix(content, "a") {
			role = RoleAssistant
		}
		if _, err := store.AppendMessage(Message{Role: role, Content: content}); err != nil {
			t.Fatal(err)
		}
	}
	kept, err := store.AppendMessage(Message{Role: RoleUser, Content: "q3"})
	if err != nil {
		t.Fatal(err)
	}
	if _, err := store.AppendCompaction("first summary", kept, 1000, false, nil); err != nil {
		t.Fatal(err)
	}
	first, err := store.Context()
	if err != nil {
		t.Fatal(err)
	}
	if first[0].Message.Content != "stable system prompt" {
		t.Fatalf("system prompt changed after first compaction: %q", first[0].Message.Content)
	}
	if _, err := store.AppendCompaction("second summary", kept, 2000, false, nil); err != nil {
		t.Fatal(err)
	}
	second, err := store.Context()
	if err != nil {
		t.Fatal(err)
	}
	if second[0].Message.Content != first[0].Message.Content {
		t.Fatalf("system prompt changed between compactions: %q then %q", first[0].Message.Content, second[0].Message.Content)
	}
	if !strings.Contains(second[1].Message.Content, "second summary") {
		t.Fatalf("newest summary not projected: %q", second[1].Message.Content)
	}
}

// TestCompactionKeepsSummaryPrefixBeforeRetainedTail verifies the newest
// compaction's summary is the first conversation message after the system
// prompt, ahead of the retained tail.
func TestCompactionKeepsSummaryPrefixBeforeRetainedTail(t *testing.T) {
	store, err := New(t.TempDir(), t.TempDir(), "test", "system prompt")
	if err != nil {
		t.Fatal(err)
	}
	defer store.Close()
	if _, err := store.AppendMessage(Message{Role: RoleUser, Content: "old"}); err != nil {
		t.Fatal(err)
	}
	kept, err := store.AppendMessage(Message{Role: RoleUser, Content: "kept"})
	if err != nil {
		t.Fatal(err)
	}
	if _, err := store.AppendCompaction("summary text", kept, 500, false, nil); err != nil {
		t.Fatal(err)
	}
	context, err := store.Context()
	if err != nil {
		t.Fatal(err)
	}
	if len(context) != 3 {
		t.Fatalf("context has %d messages, want 3", len(context))
	}
	if !context[1].Summary || !strings.Contains(context[1].Message.Content, "summary text") {
		t.Fatalf("summary is not the first projected message: %#v", context[1])
	}
	if context[2].Message.Content != "kept" {
		t.Fatalf("retained tail = %q, want %q", context[2].Message.Content, "kept")
	}
}

func TestOpenRepairsMalformedTrailingLine(t *testing.T) {
	store, err := New(t.TempDir(), t.TempDir(), "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	path := store.Path()
	if err := store.Close(); err != nil {
		t.Fatal(err)
	}
	f, err := os.OpenFile(path, os.O_APPEND|os.O_WRONLY, 0)
	if err != nil {
		t.Fatal(err)
	}
	if _, err := f.WriteString(`{"type":"message"`); err != nil {
		t.Fatal(err)
	}
	f.Close()
	reopened, err := Open(path)
	if err != nil {
		t.Fatal(err)
	}
	defer reopened.Close()
	if _, err := reopened.AppendMessage(Message{Role: RoleUser, Content: "after repair"}); err != nil {
		t.Fatal(err)
	}
	context, err := reopened.Context()
	if err != nil {
		t.Fatal(err)
	}
	if got := context[len(context)-1].Message.Content; got != "after repair" {
		t.Fatalf("last message = %q", got)
	}
}

func TestSessionSerializesTypedPrefixes(t *testing.T) {
	store, err := New(t.TempDir(), t.TempDir(), "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	path := store.Path()
	if err := store.Close(); err != nil {
		t.Fatal(err)
	}
	b, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	lines := strings.Split(strings.TrimSpace(string(b)), "\n")
	var header map[string]any
	var entry map[string]any
	if json.Unmarshal([]byte(lines[0]), &header) != nil || json.Unmarshal([]byte(lines[1]), &entry) != nil {
		t.Fatal("session is not valid JSONL")
	}
	if !strings.HasPrefix(header["id"].(string), "ses_") {
		t.Fatalf("session ID = %v", header["id"])
	}
	if !strings.HasPrefix(entry["id"].(string), "ent_") {
		t.Fatalf("entry ID = %v", entry["id"])
	}
}

func TestOpenRejectsLegacyBareIDs(t *testing.T) {
	path := t.TempDir() + "/legacy.jsonl"
	content := `{"type":"session","version":1,"id":"550e8400-e29b-41d4-a716-446655440000","app_version":"dev","timestamp":"2026-01-01T00:00:00Z","cwd":"/tmp"}` + "\n"
	if err := os.WriteFile(path, []byte(content), 0o600); err != nil {
		t.Fatal(err)
	}
	if _, err := Open(path); err == nil {
		t.Fatal("legacy bare ID was accepted")
	}
}

func TestAssistantMessageWithOnlyReasoningIsValid(t *testing.T) {
	// A turn interrupted before any answer text persists reasoning only; it must
	// remain a valid durable message so the partial turn survives a resume.
	message := Message{
		Role:        RoleAssistant,
		Interrupted: true,
		Parts:       []Part{{Type: "reasoning", Text: "still thinking"}},
	}
	if err := message.Validate(); err != nil {
		t.Fatalf("reasoning-only assistant message was rejected: %v", err)
	}
	if err := (Message{Role: RoleAssistant}).Validate(); err == nil {
		t.Fatal("truly empty assistant message was accepted")
	}
}

func TestMessageValidationRejectsDuplicateExternalToolCallIDs(t *testing.T) {
	callID := typedid.ExternalToolCallID("provider-call")
	message := Message{
		Role: RoleAssistant,
		ToolCalls: []ToolCall{
			{ID: callID, Function: ToolFunction{Name: "read"}},
			{ID: callID, Function: ToolFunction{Name: "write"}},
		},
	}
	if err := message.Validate(); err == nil {
		t.Fatal("duplicate tool call IDs were accepted")
	}
}

func TestModelChangeIsDurableButExcludedFromContext(t *testing.T) {
	store, err := New(t.TempDir(), t.TempDir(), "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	defer store.Close()
	if _, err := store.AppendModelChange(ModelSelection{Name: "review", Provider: "anthropic", ExternalID: typedid.ExternalModelID("claude")}); err != nil {
		t.Fatal(err)
	}
	if _, err := store.AppendMessage(Message{Role: RoleUser, Content: "hello"}); err != nil {
		t.Fatal(err)
	}
	context, err := store.Context()
	if err != nil {
		t.Fatal(err)
	}
	if len(context) != 2 {
		t.Fatalf("context contains %d messages, want 2", len(context))
	}
	b, err := os.ReadFile(store.Path())
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(string(b), `"type":"model_change"`) || !strings.Contains(string(b), `"external_id":"claude"`) {
		t.Fatal("model change missing from session log")
	}
}
