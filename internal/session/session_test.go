package session

import (
	"encoding/json"
	"os"
	"strings"
	"testing"

	"github.com/hizkifw/kon/internal/typedid"
)

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
	if len(context) != 3 {
		t.Fatalf("context has %d messages, want 3", len(context))
	}
	if !strings.Contains(context[0].Message.Content, "old work summary") {
		t.Fatalf("system message lacks summary: %q", context[0].Message.Content)
	}
	if context[1].Message.Content != "new question" || context[2].Message.Content != "new answer" {
		t.Fatalf("wrong retained messages: %#v", context)
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
