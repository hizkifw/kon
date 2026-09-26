package session

import (
	"encoding/json"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/hizkifw/kon/internal/typedid"
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
	store, err := New(root, cwd, "test", "system")
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
	if summaries, err := Discover(root, cwd); err != nil || len(summaries) != 0 {
		t.Fatalf("Discover = (%#v, %v), want no sessions", summaries, err)
	}
}

func TestModelChangeAloneKeepsSessionEmpty(t *testing.T) {
	root, cwd := t.TempDir(), t.TempDir()
	store, err := New(root, cwd, "test", "system")
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
	store, err := New(root, cwd, "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	if _, err := store.AppendMessage(Message{Role: RoleUser, Parts: []Part{{Type: PartText, Text: "hello"}}}); err != nil {
		t.Fatal(err)
	}
	if store.Empty() {
		t.Fatal("session with a message is still marked empty")
	}
	id := store.ID()
	if err := store.Close(); err != nil {
		t.Fatal(err)
	}
	summaries, err := Discover(root, cwd)
	if err != nil {
		t.Fatal(err)
	}
	if len(summaries) != 1 || summaries[0].ID != id {
		t.Fatalf("Discover = %#v, want the kept session", summaries)
	}
}

func TestDiscoverFindsSessionsNewestFirst(t *testing.T) {
	root, cwd := t.TempDir(), t.TempDir()
	first, err := New(root, cwd, "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	if _, err := first.AppendMessage(Message{Role: RoleUser, Parts: []Part{{Type: PartText, Text: "one"}}}); err != nil {
		t.Fatal(err)
	}
	first.Close()
	second, err := New(root, cwd, "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	secondID := second.ID()
	if _, err := second.AppendMessage(Message{Role: RoleUser, Parts: []Part{{Type: PartText, Text: "two"}}}); err != nil {
		t.Fatal(err)
	}
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
	if summaries[0].Title != "two" {
		t.Fatalf("newest session title = %q, want %q", summaries[0].Title, "two")
	}
	if summaries[0].CWD == "" {
		t.Fatal("summary is missing its working directory")
	}
}

// TestDiscoverOrdersByHeaderWhenNamesCollide covers two sessions created in the
// same millisecond. Their file names share a timestamp prefix, so name order
// falls back to the random session ID; only the header's full-precision
// timestamp can order them. The fixture forces name order to be the reverse of
// time order so the test fails without reading the header.
func TestDiscoverOrdersByHeaderWhenNamesCollide(t *testing.T) {
	root, cwd := t.TempDir(), t.TempDir()
	dir, err := directoryFor(root, cwd)
	if err != nil {
		t.Fatal(err)
	}
	if err := os.MkdirAll(dir, 0o700); err != nil {
		t.Fatal(err)
	}
	// The "older" session sorts later by name than the "newer" one, so a
	// name-only ordering would surface it first.
	older, err := typedid.ParseSessionID("ses_zzzzzzzzzzzzzzzzzzzz")
	if err != nil {
		t.Fatal(err)
	}
	newer, err := typedid.ParseSessionID("ses_00000000000000000000")
	if err != nil {
		t.Fatal(err)
	}
	writeSessionWithHeaderTime(t, dir, older, "older", time.Date(2026, 1, 1, 0, 0, 0, 1, time.UTC))
	writeSessionWithHeaderTime(t, dir, newer, "newer", time.Date(2026, 1, 1, 0, 0, 0, 2, time.UTC))

	summaries, err := Discover(root, cwd)
	if err != nil {
		t.Fatal(err)
	}
	if len(summaries) != 2 {
		t.Fatalf("Discover returned %d sessions, want 2", len(summaries))
	}
	if summaries[0].ID != newer || summaries[1].ID != older {
		t.Fatalf("Discover order = [%s %s], want [%s %s]", summaries[0].ID, summaries[1].ID, newer, older)
	}
}

// writeSessionWithHeaderTime writes a minimal valid session file whose name
// carries a fixed millisecond prefix (so names collide) while the header records
// the given full-precision timestamp.
func writeSessionWithHeaderTime(t *testing.T, dir string, sessionID typedid.SessionID, title string, created time.Time) {
	t.Helper()
	entryID, err := typedid.NewEntryID()
	if err != nil {
		t.Fatal(err)
	}
	header := Header{Type: "session", Version: SchemaVersion, ID: sessionID, AppVersion: "test", Timestamp: created, CWD: "/tmp"}
	message := TextMessage(RoleUser, title)
	path := filepath.Join(dir, created.UTC().Format("20060102T150405.000Z")+"_"+sessionID.String()+fileSuffix)
	var b strings.Builder
	writeRecord := func(v any) {
		data, err := json.Marshal(v)
		if err != nil {
			t.Fatal(err)
		}
		b.Write(data)
		b.WriteByte('\n')
	}
	writeRecord(header)
	writeRecord(Entry{Type: EntryTypeMessage, ID: entryID, Timestamp: created, Message: &message})
	if err := os.WriteFile(path, []byte(b.String()), 0o600); err != nil {
		t.Fatal(err)
	}
}

// TestDiscoverReadsOnlySessionHead proves listing stops at the head: a line that
// would only fail validation deep in the tail must not hide the session, because
// Discover never reads that far.
func TestDiscoverReadsOnlySessionHead(t *testing.T) {
	root, cwd := t.TempDir(), t.TempDir()
	store, err := New(root, cwd, "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	if _, err := store.AppendMessage(Message{Role: RoleUser, Parts: []Part{{Type: PartText, Text: "opening question"}}}); err != nil {
		t.Fatal(err)
	}
	// Pad past the head window with valid turns, then append a corrupt record
	// directly to the file.
	for i := 0; i < 50; i++ {
		store.AppendMessage(Message{Role: RoleAssistant, Parts: []Part{{Type: PartText, Text: strings.Repeat("x", 200)}}})
	}
	path := store.Path()
	if err := store.Close(); err != nil {
		t.Fatal(err)
	}
	f, err := os.OpenFile(path, os.O_APPEND|os.O_WRONLY, 0)
	if err != nil {
		t.Fatal(err)
	}
	f.WriteString("{not valid json at all\n")
	f.Close()

	summaries, err := Discover(root, cwd)
	if err != nil {
		t.Fatal(err)
	}
	if len(summaries) != 1 || summaries[0].Title != "opening question" {
		t.Fatalf("Discover = %#v", summaries)
	}
}

func TestTailEntriesReadsTrailingTurnsWithoutOpening(t *testing.T) {
	root, cwd := t.TempDir(), t.TempDir()
	store, err := New(root, cwd, "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	for i := 0; i < 5; i++ {
		store.AppendMessage(Message{Role: RoleUser, Parts: []Part{{Type: PartText, Text: "question"}}})
		store.AppendMessage(Message{Role: RoleAssistant, Parts: []Part{{Type: PartText, Text: "answer"}}})
	}
	path := store.Path()
	store.Close()

	entries, err := TailEntries(path, 2)
	if err != nil {
		t.Fatal(err)
	}
	// Two user turns, each with its answer, and nothing from earlier turns.
	if len(entries) != 4 || entries[0].Message.Text() != "question" || entries[2].Message.Text() != "question" {
		t.Fatalf("TailEntries = %#v", entries)
	}
	if _, err := os.Stat(path); err != nil {
		t.Fatalf("TailEntries removed the session file: %v", err)
	}
}

func TestTailEntriesReturnsWholeSessionWhenShorter(t *testing.T) {
	root, cwd := t.TempDir(), t.TempDir()
	store, err := New(root, cwd, "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	store.AppendMessage(Message{Role: RoleUser, Parts: []Part{{Type: PartText, Text: "only question"}}})
	store.AppendMessage(Message{Role: RoleAssistant, Parts: []Part{{Type: PartText, Text: "only answer"}}})
	path := store.Path()
	store.Close()

	entries, err := TailEntries(path, 5)
	if err != nil {
		t.Fatal(err)
	}
	if len(entries) != 3 || entries[0].Message.Role != RoleSystem || entries[2].Message.Text() != "only answer" {
		t.Fatalf("TailEntries = %#v", entries)
	}
}

func TestTailEntriesFollowsParentChainInWindow(t *testing.T) {
	root, cwd := t.TempDir(), t.TempDir()
	store, err := New(root, cwd, "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	store.AppendMessage(Message{Role: RoleUser, Parts: []Part{{Type: PartText, Text: "first"}}})
	store.AppendMessage(Message{Role: RoleAssistant, Parts: []Part{{Type: PartText, Text: "answer one"}}})
	store.AppendMessage(Message{Role: RoleUser, Parts: []Part{{Type: PartText, Text: "second"}}})
	store.AppendMessage(Message{Role: RoleAssistant, Parts: []Part{{Type: PartText, Text: "answer two"}}})
	path := store.Path()
	store.Close()

	// A window of two turns must return the parent chain, not just the last two
	// records, and must preserve conversation order.
	entries, err := TailEntries(path, 2)
	if err != nil {
		t.Fatal(err)
	}
	got := make([]string, 0, len(entries))
	for _, entry := range entries {
		if entry.Message != nil {
			got = append(got, string(entry.Message.Role)+":"+entry.Message.Text())
		}
	}
	want := []string{"user:first", "assistant:answer one", "user:second", "assistant:answer two"}
	if strings.Join(got, ",") != strings.Join(want, ",") {
		t.Fatalf("TailEntries = %v, want %v", got, want)
	}
}

// TestTailEntriesReadsAcrossBlocks forces the backwards reader to make more than
// one ReadAt call, exercising the mid-record leading line it must drop.
func TestTailEntriesReadsAcrossBlocks(t *testing.T) {
	root, cwd := t.TempDir(), t.TempDir()
	store, err := New(root, cwd, "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	// Pad the file well past one tailBlock so two user turns sit more than a
	// block away from each other.
	large := strings.Repeat("x", tailBlock)
	store.AppendMessage(Message{Role: RoleUser, Parts: []Part{{Type: PartText, Text: "old question"}}})
	store.AppendMessage(Message{Role: RoleAssistant, Parts: []Part{{Type: PartText, Text: large}}})
	store.AppendMessage(Message{Role: RoleUser, Parts: []Part{{Type: PartText, Text: "recent question"}}})
	store.AppendMessage(Message{Role: RoleAssistant, Parts: []Part{{Type: PartText, Text: "recent answer"}}})
	path := store.Path()
	store.Close()

	entries, err := TailEntries(path, 1)
	if err != nil {
		t.Fatal(err)
	}
	if len(entries) != 2 || entries[0].Message.Text() != "recent question" || entries[1].Message.Text() != "recent answer" {
		t.Fatalf("TailEntries across blocks = %#v", entries)
	}
}

func TestSummaryTitleIsFirstUserMessage(t *testing.T) {
	root, cwd := t.TempDir(), t.TempDir()
	store, err := New(root, cwd, "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	store.AppendMessage(Message{Role: RoleUser, Parts: []Part{{Type: PartText, Text: "\n  Fix the flaky test  \nmore detail"}}})
	store.Close()

	summaries, err := Discover(root, cwd)
	if err != nil {
		t.Fatal(err)
	}
	if len(summaries) != 1 || summaries[0].Title != "Fix the flaky test" {
		t.Fatalf("title = %#v", summaries)
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
	if _, err := first.AppendMessage(Message{Role: RoleUser, Parts: []Part{{Type: PartText, Text: "keep me"}}}); err != nil {
		t.Fatal(err)
	}
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
	if _, err := store.AppendMessage(Message{Role: RoleUser, Parts: []Part{{Type: PartText, Text: "content"}}}); err != nil {
		t.Fatal(err)
	}
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

// An empty session is written without fsync, so a power loss can leave it
// truncated anywhere. Resume must still find the last real session.
func TestLatestSkipsTornEmptySessions(t *testing.T) {
	root, cwd := t.TempDir(), t.TempDir()
	kept, err := New(root, cwd, "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	if _, err := kept.AppendMessage(Message{Role: RoleUser, Parts: []Part{{Type: PartText, Text: "content"}}}); err != nil {
		t.Fatal(err)
	}
	kept.Close()

	torn, err := New(root, cwd, "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	path := torn.Path()
	b, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	for _, size := range []int{0, len(b) / 2} {
		if err := os.WriteFile(path, b[:size], 0o600); err != nil {
			t.Fatal(err)
		}
		summary, ok, err := Latest(root, cwd)
		if err != nil || !ok || summary.ID != kept.ID() {
			t.Fatalf("Latest with %d-byte torn session = %v, %v, %v; want %s", size, summary.ID, ok, err, kept.ID())
		}
	}
	torn.Close()
}

func TestActivePathIncludesMessagesInOrder(t *testing.T) {
	store, err := New(t.TempDir(), t.TempDir(), "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	defer store.Close()
	if _, err := store.AppendMessage(Message{Role: RoleUser, Parts: []Part{{Type: PartText, Text: "hello"}}}); err != nil {
		t.Fatal(err)
	}
	path := store.ActivePath()
	if len(path) != 2 || path[0].Message.Role != RoleSystem || path[1].Message.Text() != "hello" {
		t.Fatalf("ActivePath = %#v", path)
	}
}

func TestCompactionProjectsRetainedMessages(t *testing.T) {
	store, err := New(t.TempDir(), t.TempDir(), "test", "system prompt")
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
	if !context[1].Summary || !strings.Contains(context[1].Message.Text(), "old work summary") {
		t.Fatalf("compaction summary not projected as its own message: %#v", context[1])
	}
	if context[2].Message.Text() != "new question" || context[3].Message.Text() != "new answer" {
		t.Fatalf("wrong retained messages: %#v", context)
	}
}

func TestContextRepairsUnansweredToolCalls(t *testing.T) {
	store, err := New(t.TempDir(), t.TempDir(), "test", "system")
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
	store, err := New(t.TempDir(), t.TempDir(), "test", "system")
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
	store, err := New(t.TempDir(), t.TempDir(), "test", "system")
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

// TestCompactionKeepsSummaryPrefixBeforeRetainedTail verifies the newest
// compaction's summary is the first conversation message after the system
// prompt, ahead of the retained tail.
func TestCompactionKeepsSummaryPrefixBeforeRetainedTail(t *testing.T) {
	store, err := New(t.TempDir(), t.TempDir(), "test", "system prompt")
	if err != nil {
		t.Fatal(err)
	}
	defer store.Close()
	if _, err := store.AppendMessage(Message{Role: RoleUser, Parts: []Part{{Type: PartText, Text: "old"}}}); err != nil {
		t.Fatal(err)
	}
	kept, err := store.AppendMessage(Message{Role: RoleUser, Parts: []Part{{Type: PartText, Text: "kept"}}})
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
	if !context[1].Summary || !strings.Contains(context[1].Message.Text(), "summary text") {
		t.Fatalf("summary is not the first projected message: %#v", context[1])
	}
	if context[2].Message.Text() != "kept" {
		t.Fatalf("retained tail = %q, want %q", context[2].Message.Text(), "kept")
	}
}

func TestOpenRepairsMalformedTrailingLine(t *testing.T) {
	store, err := New(t.TempDir(), t.TempDir(), "test", "system")
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
	defer reopened.Close()
	if _, err := reopened.AppendMessage(Message{Role: RoleUser, Parts: []Part{{Type: PartText, Text: "after repair"}}}); err != nil {
		t.Fatal(err)
	}
	context, err := reopened.Context()
	if err != nil {
		t.Fatal(err)
	}
	if got := context[len(context)-1].Message.Text(); got != "after repair" {
		t.Fatalf("last message = %q", got)
	}
}

func TestSessionSerializesTypedPrefixes(t *testing.T) {
	store, err := New(t.TempDir(), t.TempDir(), "test", "system")
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
	store, err := New(dir, dir, "test", "system")
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
	store, err := New(dir, dir, "test", "system")
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
	store, err := New(t.TempDir(), t.TempDir(), "test", "system")
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
	store, err := New(t.TempDir(), t.TempDir(), "test", "system")
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
	store, err := New(dir, dir, "test", "system")
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

func TestSubagentSessionRecordsParentAndIsNotLatest(t *testing.T) {
	root, cwd := t.TempDir(), t.TempDir()
	parent, err := New(root, cwd, "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	if _, err := parent.AppendMessage(TextMessage(RoleUser, "parent task")); err != nil {
		t.Fatal(err)
	}
	if err := parent.Close(); err != nil {
		t.Fatal(err)
	}
	child, err := NewChild(root, cwd, "test", "system", parent.ID())
	if err != nil {
		t.Fatal(err)
	}
	if _, err := child.AppendMessage(TextMessage(RoleUser, "subtask")); err != nil {
		t.Fatal(err)
	}
	if err := child.Close(); err != nil {
		t.Fatal(err)
	}
	found, err := Find(root, cwd, child.ID())
	if err != nil || found.Parent != parent.ID() {
		t.Fatalf("child summary = %#v, %v", found, err)
	}
	latest, ok, err := Latest(root, cwd)
	if err != nil || !ok || latest.ID != parent.ID() {
		t.Fatalf("latest = %s, %v, %v; want the parent, not the newer subagent", latest.ID, ok, err)
	}
}
