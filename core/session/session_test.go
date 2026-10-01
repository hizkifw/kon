package session

import (
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"
	"time"

	"github.com/hizkifw/kon/core/typedid"
)

func messageWithCalls(calls []ToolCall) Message {
	m := Message{Role: RoleAssistant}
	for _, call := range calls {
		m.Parts = append(m.Parts, Part{Type: PartToolCall, ToolCallID: call.ID, ToolName: call.Function.Name, ToolInput: call.Function.Arguments})
	}
	return m
}

func TestEmptySessionIsDiscardedOnClose(t *testing.T) {
	root, cwd := t.TempDir(), t.TempDir()
	store, err := newStore(root, cwd, "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	path := store.Path()
	if !store.Empty() {
		t.Fatal("new session is not empty")
	}
	if err := store.Close(); err != nil {
		t.Fatal(err)
	}
	if _, err := os.Stat(path); !errors.Is(err, os.ErrNotExist) {
		t.Fatalf("empty session file still exists: %v", err)
	}
	if entries, err := os.ReadDir(root); err != nil || len(entries) != 0 {
		t.Fatalf("session directory holds %v (%v), want nothing", entries, err)
	}
}

func TestModelChangeAloneKeepsSessionEmpty(t *testing.T) {
	root, cwd := t.TempDir(), t.TempDir()
	store, err := newStore(root, cwd, "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	path := store.Path()
	if _, err := store.AppendModelChange(ModelSelection{Name: "review", WireFormat: "anthropic", ExternalID: typedid.ExternalModelID("claude")}); err != nil {
		t.Fatal(err)
	}
	if !store.Empty() {
		t.Fatal("a model change alone should leave the session empty")
	}
	if err := store.Close(); err != nil {
		t.Fatal(err)
	}
	if _, err := os.Stat(path); !errors.Is(err, os.ErrNotExist) {
		t.Fatalf("empty session file still exists: %v", err)
	}
}

func TestSessionWithMessageIsKeptOnClose(t *testing.T) {
	root, cwd := t.TempDir(), t.TempDir()
	store, err := newStore(root, cwd, "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	if _, err := store.AppendMessage(Message{Role: RoleUser, Parts: []Part{{Type: PartText, Text: "hello"}}}); err != nil {
		t.Fatal(err)
	}
	if store.Empty() {
		t.Fatal("session with a message is still marked empty")
	}
	path := store.Path()
	if err := store.Close(); err != nil {
		t.Fatal(err)
	}
	reopened, err := Open(path)
	if err != nil {
		t.Fatalf("kept session does not reopen: %v", err)
	}
	reopened.Close()
}

func TestCompactionProjectsRetainedMessages(t *testing.T) {
	store, err := newStore(t.TempDir(), t.TempDir(), "test", "system prompt")
	if err != nil {
		t.Fatal(err)
	}
	defer store.Close()
	if _, err := store.AppendMessage(Message{Role: RoleUser, Parts: []Part{{Type: PartText, Text: "old question"}}}); err != nil {
		t.Fatal(err)
	}
	if _, err := store.AppendMessage(Message{Role: RoleAssistant, Parts: []Part{{Type: PartText, Text: "old answer"}}}); err != nil {
		t.Fatal(err)
	}
	kept, err := store.AppendMessage(Message{Role: RoleUser, Parts: []Part{{Type: PartText, Text: "new question"}}})
	if err != nil {
		t.Fatal(err)
	}
	if _, err := store.AppendMessage(Message{Role: RoleAssistant, Parts: []Part{{Type: PartText, Text: "new answer"}}}); err != nil {
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
	if context[0].Message.Text() != "system prompt" {
		t.Fatalf("system message was rewritten: %q", context[0].Message.Text())
	}
	if !context[1].Summary || context[1].Message.Text() != "old work summary" {
		t.Fatalf("compaction summary not projected as its own message: %#v", context[1])
	}
	// The summary must be a user message: the Messages backend folds every
	// system message into the system prompt, which must stay byte-identical
	// across compactions for the prompt cache.
	if context[1].Message.Role != RoleUser {
		t.Fatalf("compaction summary role = %q, want %q", context[1].Message.Role, RoleUser)
	}
	if context[2].Message.Text() != "new question" || context[3].Message.Text() != "new answer" {
		t.Fatalf("wrong retained messages: %#v", context)
	}
}

func TestContextRepairsUnansweredToolCalls(t *testing.T) {
	store, err := newStore(t.TempDir(), t.TempDir(), "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	defer store.Close()
	callID := typedid.ExternalToolCallID("interrupted-call")
	if _, err := store.AppendMessage(messageWithCalls([]ToolCall{{
		ID: callID, Type: "function", Function: ToolFunction{Name: "shell", Arguments: json.RawMessage(`{"command":"sleep"}`)},
	}})); err != nil {
		t.Fatal(err)
	}
	context, err := store.Context()
	if err != nil {
		t.Fatal(err)
	}
	resultID, _ := context[2].Message.ToolResult()
	if len(context) != 3 || context[2].Message.Role != RoleTool || resultID != callID || context[2].Message.Text() != InterruptedToolResult {
		t.Fatalf("repaired context = %#v", context)
	}
	// Projection repairs are ephemeral and do not change the append-only log.
	path := store.ActivePath()
	if len(path) != 2 {
		t.Fatalf("durable entries = %d, want 2", len(path))
	}
}

// TestContextRepairKeepsToolResultsInCallOrder covers a batch that was cut off
// mid-flight: some calls have durable results and some do not. Repair must
// present every result against its own call, in the order the calls were made,
// not appendix the synthetic ones after the batch.
func TestContextRepairKeepsToolResultsInCallOrder(t *testing.T) {
	store, err := newStore(t.TempDir(), t.TempDir(), "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	defer store.Close()
	calls := []typedid.ToolCallID{
		typedid.ExternalToolCallID("first"),
		typedid.ExternalToolCallID("second"),
		typedid.ExternalToolCallID("third"),
	}
	toolCalls := make([]ToolCall, len(calls))
	for i, id := range calls {
		toolCalls[i] = ToolCall{ID: id, Type: "function", Function: ToolFunction{Name: "shell", Arguments: json.RawMessage(`{}`)}}
	}
	if _, err := store.AppendMessage(messageWithCalls(toolCalls)); err != nil {
		t.Fatal(err)
	}
	// Only the first call finished before the turn was cancelled.
	if _, err := store.AppendMessage(ToolResultMessage(calls[0], "shell", "real output")); err != nil {
		t.Fatal(err)
	}
	context, err := store.Context()
	if err != nil {
		t.Fatal(err)
	}
	if len(context) != 5 {
		t.Fatalf("context has %d messages, want 5: %#v", len(context), context)
	}
	want := []struct {
		id      typedid.ToolCallID
		content string
	}{
		{calls[0], "real output"},
		{calls[1], InterruptedToolResult},
		{calls[2], InterruptedToolResult},
	}
	for i, expected := range want {
		got := context[i+2].Message
		id, _ := got.ToolResult()
		if got.Role != RoleTool || id != expected.id || got.Text() != expected.content {
			t.Fatalf("result %d = %#v, want id %s content %q", i, got, expected.id, expected.content)
		}
	}
}

// TestContextRepairIgnoresResultsFromEarlierTurns covers call IDs reused across
// turns, which compatible servers do (call_0 on every turn). An earlier turn's
// result must not answer a later turn's identical ID; the later batch is still
// incomplete and must be repaired.
func TestContextRepairIgnoresResultsFromEarlierTurns(t *testing.T) {
	store, err := newStore(t.TempDir(), t.TempDir(), "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	defer store.Close()
	callID := typedid.ExternalToolCallID("call_0")
	assistant := messageWithCalls([]ToolCall{{
		ID: callID, Type: "function", Function: ToolFunction{Name: "shell", Arguments: json.RawMessage(`{}`)},
	}})
	if _, err := store.AppendMessage(assistant); err != nil {
		t.Fatal(err)
	}
	if _, err := store.AppendMessage(ToolResultMessage(callID, "shell", "first turn result")); err != nil {
		t.Fatal(err)
	}
	if _, err := store.AppendMessage(Message{Role: RoleUser, Parts: []Part{{Type: PartText, Text: "again"}}}); err != nil {
		t.Fatal(err)
	}
	// The second turn reuses the ID and was cancelled before any result.
	if _, err := store.AppendMessage(assistant); err != nil {
		t.Fatal(err)
	}
	context, err := store.Context()
	if err != nil {
		t.Fatal(err)
	}
	if len(context) != 6 {
		t.Fatalf("context has %d messages, want 6: %#v", len(context), context)
	}
	tail := context[5].Message
	id, _ := tail.ToolResult()
	if tail.Role != RoleTool || id != callID || tail.Text() != InterruptedToolResult {
		t.Fatalf("later batch = %#v, want a synthetic result for %s", tail, callID)
	}
}

// TestCompactionKeepsSystemPromptStable guards the prompt-cache contract: the
// system message must stay byte-identical across repeated compactions so the
// cached leading prefix survives.
func TestCompactionKeepsSystemPromptStable(t *testing.T) {
	store, err := newStore(t.TempDir(), t.TempDir(), "test", "stable system prompt")
	if err != nil {
		t.Fatal(err)
	}
	defer store.Close()
	for _, content := range []string{"q1", "a1", "q2", "a2"} {
		role := RoleUser
		if strings.HasPrefix(content, "a") {
			role = RoleAssistant
		}
		if _, err := store.AppendMessage(Message{Role: role, Parts: []Part{{Type: PartText, Text: content}}}); err != nil {
			t.Fatal(err)
		}
	}
	kept, err := store.AppendMessage(Message{Role: RoleUser, Parts: []Part{{Type: PartText, Text: "q3"}}})
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
	if first[0].Message.Text() != "stable system prompt" {
		t.Fatalf("system prompt changed after first compaction: %q", first[0].Message.Text())
	}
	if _, err := store.AppendCompaction("second summary", kept, 2000, false, nil); err != nil {
		t.Fatal(err)
	}
	second, err := store.Context()
	if err != nil {
		t.Fatal(err)
	}
	if second[0].Message.Text() != first[0].Message.Text() {
		t.Fatalf("system prompt changed between compactions: %q then %q", first[0].Message.Text(), second[0].Message.Text())
	}
	if !strings.Contains(second[1].Message.Text(), "second summary") {
		t.Fatalf("newest summary not projected: %q", second[1].Message.Text())
	}
}

func TestOpenRepairsMalformedTrailingLine(t *testing.T) {
	store, err := newStore(t.TempDir(), t.TempDir(), "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	path := store.Path()
	if _, err := store.AppendMessage(Message{Role: RoleUser, Parts: []Part{{Type: PartText, Text: "content"}}}); err != nil {
		t.Fatal(err)
	}
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
	if _, err := reopened.AppendMessage(Message{Role: RoleUser, Parts: []Part{{Type: PartText, Text: "after repair"}}}); err != nil {
		t.Fatal(err)
	}
	if err := reopened.Close(); err != nil {
		t.Fatal(err)
	}
	// Only a second open shows whether the torn tail was cut from the file. Had
	// it stayed, the append above would have been glued onto it, and this open
	// would discard the merged line as a torn tail in turn.
	again, err := Open(path)
	if err != nil {
		t.Fatal(err)
	}
	defer again.Close()
	context, err := again.Context()
	if err != nil {
		t.Fatal(err)
	}
	got := make([]string, len(context))
	for i, item := range context {
		got[i] = item.Message.Text()
	}
	if want := []string{"system", "content", "after repair"}; !slices.Equal(got, want) {
		t.Fatalf("messages after reopening = %q, want %q", got, want)
	}
}

func TestOpenRejectsLegacyBareIDs(t *testing.T) {
	path := t.TempDir() + "/legacy.jsonl"
	// The header carries the current version, so the bare ID is its only
	// defect; an old version would be refused before the ID is looked at.
	content := fmt.Sprintf(`{"type":"session","version":%d,"id":"550e8400-e29b-41d4-a716-446655440000","app_version":"dev","timestamp":"2026-01-01T00:00:00Z","cwd":"/tmp"}`, SchemaVersion) + "\n"
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
	message := messageWithCalls([]ToolCall{
		{ID: callID, Function: ToolFunction{Name: "read"}},
		{ID: callID, Function: ToolFunction{Name: "write"}},
	})
	if err := message.Validate(); err == nil {
		t.Fatal("duplicate tool call IDs were accepted")
	}
}

func TestToolOutcomeSurvivesReopen(t *testing.T) {
	dir := t.TempDir()
	store, err := newStore(dir, dir, "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	path := store.Path()
	message := ToolResultMessage(typedid.ExternalToolCallID("call-1"), "shell", "exit code: 3")
	message.IsError = true
	message.Details = json.RawMessage(`{"exit_code":3,"duration":"4ms"}`)
	if _, err := store.AppendMessage(message); err != nil {
		t.Fatal(err)
	}
	if err := store.Close(); err != nil {
		t.Fatal(err)
	}
	reopened, err := Open(path)
	if err != nil {
		t.Fatal(err)
	}
	defer reopened.Close()
	got := reopened.ActivePath()[1].Message
	if got == nil || !got.IsError || string(got.Details) != string(message.Details) {
		t.Fatalf("tool outcome after reopen = %#v", got)
	}
}

func TestOrderedPartsAndProviderMetadataSurviveReopen(t *testing.T) {
	dir := t.TempDir()
	store, err := newStore(dir, dir, "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	path := store.Path()
	parts := []Part{
		{Type: PartText, Text: "first"},
		{Type: PartReasoning, Text: "thought", ProviderOptions: json.RawMessage(`{"signature":"opaque"}`)},
		{Type: PartToolCall, ToolCallID: "call-1", ToolName: "read", ToolInput: json.RawMessage(`{}`)},
		{Type: PartText, Text: "last"},
	}
	if _, err := store.AppendMessage(Message{Role: RoleAssistant, Parts: parts}); err != nil {
		t.Fatal(err)
	}
	if err := store.Close(); err != nil {
		t.Fatal(err)
	}
	reopened, err := Open(path)
	if err != nil {
		t.Fatal(err)
	}
	defer reopened.Close()
	got := reopened.ActivePath()[1].Message
	if got == nil || got.Text() != "firstlast" || len(got.Parts) != len(parts) || got.Parts[1].Type != PartReasoning || string(got.Parts[1].ProviderOptions) != string(parts[1].ProviderOptions) || got.Parts[3].Text != "last" {
		t.Fatalf("parts after reopen = %#v", got)
	}
}

func TestModelChangeIsDurableButExcludedFromContext(t *testing.T) {
	store, err := newStore(t.TempDir(), t.TempDir(), "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	defer store.Close()
	if _, err := store.AppendModelChange(ModelSelection{Name: "work/claude", WireFormat: "anthropic", ConnectionID: "work", ExternalID: typedid.ExternalModelID("claude")}); err != nil {
		t.Fatal(err)
	}
	if _, err := store.AppendMessage(Message{Role: RoleUser, Parts: []Part{{Type: PartText, Text: "hello"}}}); err != nil {
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
	if !strings.Contains(string(b), `"type":"model_change"`) || !strings.Contains(string(b), `"wire_format":"anthropic"`) || !strings.Contains(string(b), `"connection_id":"work"`) || !strings.Contains(string(b), `"external_id":"claude"`) || strings.Contains(string(b), `"provider":"anthropic"`) {
		t.Fatal("model change missing from session log")
	}
}

func TestTurnMarkersRoundTripButStayOutOfContext(t *testing.T) {
	store, err := newStore(t.TempDir(), t.TempDir(), "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	if _, err := store.AppendTurnStart(); err != nil {
		t.Fatal(err)
	}
	if !store.Empty() {
		t.Fatal("a turn start alone should leave the session empty")
	}
	if _, err := store.AppendMessage(TextMessage(RoleUser, "hello")); err != nil {
		t.Fatal(err)
	}
	if _, err := store.AppendTurnEnd(-time.Second); err == nil {
		t.Fatal("negative turn duration was accepted")
	}
	if _, err := store.AppendTurnEnd(2*time.Minute + 1500*time.Millisecond); err != nil {
		t.Fatal(err)
	}
	path := store.Path()
	if err := store.Close(); err != nil {
		t.Fatal(err)
	}
	reopened, err := Open(path)
	if err != nil {
		t.Fatal(err)
	}
	defer reopened.Close()
	entries := reopened.ActivePath()
	if len(entries) != 4 || entries[1].Type != EntryTypeTurnStart || entries[3].Type != EntryTypeTurnEnd {
		t.Fatalf("turn markers did not round-trip: %#v", entries)
	}
	if got := entries[3].TurnDuration(); got != 2*time.Minute+1500*time.Millisecond {
		t.Fatalf("turn duration = %v", got)
	}
	context, err := reopened.Context()
	if err != nil {
		t.Fatal(err)
	}
	if len(context) != 2 {
		t.Fatalf("context contains %d messages, want 2", len(context))
	}
}

func TestImagePartRoundTripsThroughPersistence(t *testing.T) {
	dir := t.TempDir()
	store, err := newStore(dir, dir, "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	path := store.Path()
	image := []byte("hello")
	part, err := store.SaveImage(image, "image/png")
	if err != nil {
		t.Fatal(err)
	}
	again, err := store.SaveImage(image, "image/png")
	if err != nil || again.ImageHash != part.ImageHash {
		t.Fatalf("deduplicated image = %#v, %v", again, err)
	}
	if _, err := store.AppendMessage(Message{Role: RoleUser, Parts: []Part{{Type: PartText, Text: "look"}}}); err != nil {
		t.Fatal(err)
	}
	imageResult := ToolResultMessage("call-1", "read", "loaded image")
	imageResult.Parts = append(imageResult.Parts, part)
	if _, err := store.AppendMessage(imageResult); err != nil {
		t.Fatal(err)
	}
	if err := store.Close(); err != nil {
		t.Fatal(err)
	}
	line, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	if strings.Contains(string(line), "aGVsbG8=") || strings.Contains(string(line), "data:image") {
		t.Fatal("image bytes were written into the JSONL")
	}
	blobs, err := os.ReadDir(path + ".blobs")
	if err != nil || len(blobs) != 1 {
		t.Fatalf("blob files = %v, %v", blobs, err)
	}
	reopened, err := Open(path)
	if err != nil {
		t.Fatal(err)
	}
	defer reopened.Close()
	items, err := reopened.Context()
	if err != nil {
		t.Fatal(err)
	}
	var found bool
	for _, item := range items {
		if item.Message.Role != RoleTool {
			continue
		}
		if len(item.Message.Parts) != 2 || item.Message.Parts[1].Type != PartImage || item.Message.Parts[1].ImageHash != part.ImageHash || item.Message.Parts[1].ImageMIME != "image/png" {
			t.Fatalf("tool parts = %#v", item.Message.Parts)
		}
		loaded, err := reopened.ReadImage(item.Message.Parts[1].ImageHash)
		if err != nil || string(loaded) != string(image) {
			t.Fatalf("blob after reopen = %q, %v", loaded, err)
		}
		found = true
	}
	if !found {
		t.Fatal("tool result was not persisted")
	}
	if err := os.WriteFile(filepath.Join(path+".blobs", part.ImageHash), []byte("changed"), 0o600); err != nil {
		t.Fatal(err)
	}
	if _, err := reopened.ReadImage(part.ImageHash); err == nil {
		t.Fatal("corrupted image blob was accepted")
	}
	if _, err := reopened.ReadImage("../other"); err == nil {
		t.Fatal("invalid image hash was accepted")
	}
}
