package agent

import (
	"context"
	"errors"
	"strings"
	"testing"

	"github.com/hizkifw/kon/internal/config"
	"github.com/hizkifw/kon/internal/contextfiles"
	"github.com/hizkifw/kon/internal/provider"
	"github.com/hizkifw/kon/internal/session"
	"github.com/hizkifw/kon/internal/tokens"
	"github.com/hizkifw/kon/internal/tools"
	"github.com/hizkifw/kon/internal/typedid"
)

// testModel is a placeholder profile; the fake provider never reads its
// connection fields.
var testModel = config.Model{Name: "default", Type: "openai-compatible", BaseURL: "https://api.openai.com/v1"}

type fakeProvider struct {
	completeCalls int
	streamCalls   int
}

func (f *fakeProvider) Stream(_ context.Context, _ []session.Message, _ []provider.Tool, emit func(provider.Event)) (session.Message, error) {
	f.streamCalls++
	emit(provider.Event{Text: "done"})
	return session.Message{Role: session.RoleAssistant, Parts: []session.Part{{Type: session.PartText, Text: "done"}}, Finish: "stop", Usage: &session.Usage{PromptTokens: 100, CompletionTokens: 1, TotalTokens: 101}}, nil
}

func (f *fakeProvider) Complete(_ context.Context, _ []session.Message, _ []provider.Tool, _ tokens.Count) (session.Message, error) {
	f.completeCalls++
	return session.Message{Role: session.RoleAssistant, Parts: []session.Part{{Type: session.PartText, Text: "summary"}}, Usage: &session.Usage{PromptTokens: 50, CompletionTokens: 5, TotalTokens: 55}}, nil
}

func TestRunnerCompactsOlderTurnsBeforeRequest(t *testing.T) {
	store, err := session.New(t.TempDir(), t.TempDir(), "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	defer store.Close()
	for i := 0; i < 3; i++ {
		if _, err := store.AppendMessage(session.Message{Role: session.RoleUser, Parts: []session.Part{{Type: session.PartText, Text: strings.Repeat("question ", 80)}}}); err != nil {
			t.Fatal(err)
		}
		if _, err := store.AppendMessage(session.Message{Role: session.RoleAssistant, Parts: []session.Part{{Type: session.PartText, Text: strings.Repeat("answer ", 80)}}}); err != nil {
			t.Fatal(err)
		}
	}
	fake := &fakeProvider{}
	cfg := config.Default()
	model := testModel
	model.ContextWindowTokens = 500
	cfg.Compaction.ReserveTokens = 100
	cfg.Compaction.KeepRecentTokens = 100
	runner := New(model, cfg.Compaction, fake, store, tools.New(t.TempDir(), false))
	if err := runner.Run(context.Background(), "new work", func(Event) {}); err != nil {
		t.Fatal(err)
	}
	if fake.completeCalls == 0 || fake.streamCalls != 1 {
		t.Fatalf("complete=%d stream=%d", fake.completeCalls, fake.streamCalls)
	}
	contextMessages, err := store.Context()
	if err != nil {
		t.Fatal(err)
	}
	if contextMessages[0].Summary {
		t.Fatal("system message was replaced by a summary")
	}
	if !containsSummary(contextMessages) {
		t.Fatal("compaction summary missing from projected context")
	}
}

func TestNewSeedsUsageFromPersistedAssistantMessages(t *testing.T) {
	dir := t.TempDir()
	store, err := session.New(dir, dir, "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	path := store.Path()
	if _, err := store.AppendMessage(session.Message{Role: session.RoleUser, Parts: []session.Part{{Type: session.PartText, Text: "hi"}}}); err != nil {
		t.Fatal(err)
	}
	if _, err := store.AppendMessage(session.Message{
		Role: session.RoleAssistant, Parts: []session.Part{{Type: session.PartText, Text: "answer"}},
		Usage: &session.Usage{PromptTokens: 900, CompletionTokens: 40, TotalTokens: 940},
	}); err != nil {
		t.Fatal(err)
	}
	store.Close()

	reopened, err := session.Open(path)
	if err != nil {
		t.Fatal(err)
	}
	defer reopened.Close()
	cfg := config.Default()
	runner := New(testModel, cfg.Compaction, &fakeProvider{}, reopened, tools.New(t.TempDir(), false))
	tokens, ok := runner.ContextUsage()
	if !ok || tokens != 940 {
		t.Fatalf("ContextUsage = (%d, %v), want (940, true)", tokens, ok)
	}
}

func TestContextUsageUnknownBeforeFirstReport(t *testing.T) {
	store, err := session.New(t.TempDir(), t.TempDir(), "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	defer store.Close()
	if _, err := store.AppendMessage(session.Message{Role: session.RoleUser, Parts: []session.Part{{Type: session.PartText, Text: "hi"}}}); err != nil {
		t.Fatal(err)
	}
	cfg := config.Default()
	runner := New(testModel, cfg.Compaction, &fakeProvider{}, store, tools.New(t.TempDir(), false))
	if tokens, ok := runner.ContextUsage(); ok {
		t.Fatalf("ContextUsage = (%d, true), want unknown", tokens)
	}
}

// interruptingProvider simulates a stream that emits a partial turn and then
// fails, the way a user cancellation or a dropped connection arrives.
type interruptingProvider struct {
	calls int
}

func (p *interruptingProvider) Stream(_ context.Context, _ []session.Message, _ []provider.Tool, emit func(provider.Event)) (session.Message, error) {
	p.calls++
	emit(provider.Event{Text: "partial thought", Thinking: true})
	emit(provider.Event{Text: "half an answer"})
	return session.Message{
		Role:        session.RoleAssistant,
		Interrupted: true,
		Parts: []session.Part{
			{Type: provider.PartReasoning, Text: "partial thought"},
			{Type: provider.PartText, Text: "half an answer"},
		},
	}, context.Canceled
}

func (p *interruptingProvider) Complete(_ context.Context, _ []session.Message, _ []provider.Tool, _ tokens.Count) (session.Message, error) {
	return session.Message{Role: session.RoleAssistant, Parts: []session.Part{{Type: session.PartText, Text: "summary"}}}, nil
}

func TestRunnerPersistsPartialTurnOnInterruptedStream(t *testing.T) {
	store, err := session.New(t.TempDir(), t.TempDir(), "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	defer store.Close()
	fake := &interruptingProvider{}
	runner := New(config.Model{}, config.Compaction{}, fake, store, tools.New(t.TempDir(), false))
	err = runner.Run(context.Background(), "hello", func(Event) {})
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("Run error = %v, want context.Canceled", err)
	}
	contextMessages, err := store.Context()
	if err != nil {
		t.Fatal(err)
	}
	last := contextMessages[len(contextMessages)-1].Message
	if last.Role != session.RoleAssistant || !last.Interrupted || last.Text() != "half an answer" {
		t.Fatalf("partial turn was not persisted: %#v", last)
	}
	if len(last.Parts) != 2 || last.Parts[0].Type != provider.PartReasoning {
		t.Fatalf("partial reasoning was not persisted: %#v", last.Parts)
	}
	// A follow-up turn must see the partial assistant message in its context so
	// the model can continue from it.
	if !containsInterrupted(contextMessages) {
		t.Fatal("persisted partial is missing from the projected context")
	}
}

// TestRunnerBracketsTurnWithMarkers checks that a turn is recorded as a start
// before its user message and an end after its last entry, and that a turn
// cut short by cancellation still records its end: only a process that dies
// mid-turn may leave a start unmatched.
func TestRunnerBracketsTurnWithMarkers(t *testing.T) {
	for _, c := range []struct {
		name     string
		provider Provider
	}{
		{"completed", &fakeProvider{}},
		{"interrupted", &interruptingProvider{}},
	} {
		t.Run(c.name, func(t *testing.T) {
			store, err := session.New(t.TempDir(), t.TempDir(), "test", "system")
			if err != nil {
				t.Fatal(err)
			}
			defer store.Close()
			runner := New(config.Model{}, config.Compaction{}, c.provider, store, tools.New(t.TempDir(), false))
			_ = runner.Run(context.Background(), "hello", func(Event) {})
			path := store.ActivePath()
			// The root system message leads, then the turn.
			if len(path) < 4 || path[1].Type != session.EntryTypeTurnStart || path[2].Message == nil || path[2].Message.Role != session.RoleUser {
				t.Fatalf("turn does not open with a start then the prompt: %#v", path)
			}
			if last := path[len(path)-1]; last.Type != session.EntryTypeTurnEnd || last.DurationMS < 0 {
				t.Fatalf("turn does not close with an end: %#v", last)
			}
			contextMessages, err := store.Context()
			if err != nil {
				t.Fatal(err)
			}
			if len(contextMessages) != len(path)-2 {
				t.Fatalf("turn markers leaked into model context: %d messages for %d entries", len(contextMessages), len(path))
			}
		})
	}
}

type toolCallProvider struct{}

func (toolCallProvider) Stream(context.Context, []session.Message, []provider.Tool, func(provider.Event)) (session.Message, error) {
	return session.Message{Role: session.RoleAssistant, Parts: []session.Part{
		{Type: session.PartToolCall, ToolCallID: typedid.ExternalToolCallID("first"), ToolName: "unknown", ToolInput: []byte(`{}`)},
		{Type: session.PartToolCall, ToolCallID: typedid.ExternalToolCallID("second"), ToolName: "unknown", ToolInput: []byte(`{}`)},
	}}, nil
}

func (toolCallProvider) Complete(context.Context, []session.Message, []provider.Tool, tokens.Count) (session.Message, error) {
	return session.Message{}, nil
}

func TestRunnerPersistsInterruptedResultsForRemainingToolCalls(t *testing.T) {
	store, err := session.New(t.TempDir(), t.TempDir(), "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	defer store.Close()
	ctx, cancel := context.WithCancel(context.Background())
	cancel()
	runner := New(config.Model{}, config.Compaction{}, toolCallProvider{}, store, tools.New(t.TempDir(), false))
	if err := runner.Run(ctx, "hello", func(Event) {}); !errors.Is(err, context.Canceled) {
		t.Fatalf("Run error = %v, want context.Canceled", err)
	}
	// The durable log must hold the result; projection would synthesize it from
	// an unanswered call either way, so only the entries prove the write.
	var interrupted int
	for _, entry := range store.ActivePath() {
		if entry.Message != nil && entry.Message.Role == session.RoleTool && entry.Message.Text() == session.InterruptedToolResult {
			if !entry.Message.IsError {
				t.Fatal("interrupted tool result was not marked as an error")
			}
			interrupted++
		}
	}
	if interrupted != 1 {
		t.Fatalf("interrupted results = %d, want 1: %#v", interrupted, store.ActivePath())
	}
}

func containsInterrupted(messages []session.ContextMessage) bool {
	for _, message := range messages {
		if message.Message.Interrupted {
			return true
		}
	}
	return false
}

type reasoningProvider struct{}

func (reasoningProvider) Stream(_ context.Context, _ []session.Message, _ []provider.Tool, emit func(provider.Event)) (session.Message, error) {
	emit(provider.Event{Text: "pondering", Thinking: true})
	emit(provider.Event{Text: "answer"})
	return session.Message{Role: session.RoleAssistant, Parts: []session.Part{{Type: session.PartText, Text: "answer"}}, Finish: "stop"}, nil
}

func (reasoningProvider) Complete(_ context.Context, _ []session.Message, _ []provider.Tool, _ tokens.Count) (session.Message, error) {
	return session.Message{Role: session.RoleAssistant, Parts: []session.Part{{Type: session.PartText, Text: "summary"}}}, nil
}

func TestRunnerEmitsThinkingBeforeText(t *testing.T) {
	store, err := session.New(t.TempDir(), t.TempDir(), "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	defer store.Close()
	runner := New(config.Model{}, config.Compaction{}, reasoningProvider{}, store, tools.New(t.TempDir(), false))
	var kinds []EventKind
	if err := runner.Run(context.Background(), "hello", func(event Event) { kinds = append(kinds, event.Kind) }); err != nil {
		t.Fatal(err)
	}
	want := []EventKind{EventThinking, EventText, EventAssistantDone}
	if len(kinds) != len(want) {
		t.Fatalf("kinds = %v", kinds)
	}
	for i, kind := range want {
		if kinds[i] != kind {
			t.Fatalf("kinds = %v, want %v", kinds, want)
		}
	}
}

func TestSelectCutNeverStartsAtToolResult(t *testing.T) {
	items := []session.ContextMessage{
		{EntryID: newTestEntryID(t), Message: session.Message{Role: session.RoleSystem, Parts: []session.Part{{Type: session.PartText, Text: "system"}}}},
		{EntryID: newTestEntryID(t), Message: session.Message{Role: session.RoleUser, Parts: []session.Part{{Type: session.PartText, Text: strings.Repeat("x", 100)}}}},
		{EntryID: newTestEntryID(t), Message: session.Message{Role: session.RoleAssistant, Parts: []session.Part{{Type: session.PartToolCall, ToolCallID: typedid.ExternalToolCallID("1")}}}},
		{EntryID: newTestEntryID(t), Message: session.ToolResultMessage("1", "shell", strings.Repeat("y", 100))},
		{EntryID: newTestEntryID(t), Message: session.Message{Role: session.RoleAssistant, Parts: []session.Part{{Type: session.PartText, Text: "done"}}}},
		{EntryID: newTestEntryID(t), Message: session.Message{Role: session.RoleUser, Parts: []session.Part{{Type: session.PartText, Text: "new"}}}},
	}
	cut := selectCut(items, 10)
	if cut < 0 || items[cut].Message.Role == session.RoleTool {
		t.Fatalf("cut=%d role=%q", cut, items[cut].Message.Role)
	}
}

func TestCompactForcesCompactionBelowThreshold(t *testing.T) {
	store, err := session.New(t.TempDir(), t.TempDir(), "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	defer store.Close()
	for i := 0; i < 3; i++ {
		if _, err := store.AppendMessage(session.Message{Role: session.RoleUser, Parts: []session.Part{{Type: session.PartText, Text: strings.Repeat("question ", 80)}}}); err != nil {
			t.Fatal(err)
		}
		if _, err := store.AppendMessage(session.Message{Role: session.RoleAssistant, Parts: []session.Part{{Type: session.PartText, Text: strings.Repeat("answer ", 80)}}}); err != nil {
			t.Fatal(err)
		}
	}
	fake := &fakeProvider{}
	cfg := config.Default()
	// A window large enough that automatic compaction would not trigger, so a
	// successful compaction can only come from the manual force path.
	model := testModel
	model.ContextWindowTokens = 1_000_000
	cfg.Compaction.ReserveTokens = 16_384
	cfg.Compaction.KeepRecentTokens = 100
	runner := New(model, cfg.Compaction, fake, store, tools.New(t.TempDir(), false))
	var compacted []Event
	if err := runner.Compact(context.Background(), func(event Event) { compacted = append(compacted, event) }); err != nil {
		t.Fatal(err)
	}
	if fake.completeCalls != 1 || fake.streamCalls != 0 {
		t.Fatalf("complete=%d stream=%d", fake.completeCalls, fake.streamCalls)
	}
	if len(compacted) == 0 || compacted[0].Kind != EventCompacted {
		t.Fatalf("events = %#v", compacted)
	}
	contextMessages, err := store.Context()
	if err != nil {
		t.Fatal(err)
	}
	if contextMessages[0].Summary {
		t.Fatal("system message was replaced by a summary")
	}
	if !containsSummary(contextMessages) {
		t.Fatal("compaction summary missing from projected context")
	}
}

// containsSummary reports whether any projected message carries a compaction
// summary, which is now a standalone message rather than system-prompt text.
func containsSummary(messages []session.ContextMessage) bool {
	for _, message := range messages {
		if message.Summary && strings.Contains(message.Message.Text(), "summary") {
			return true
		}
	}
	return false
}

func TestCompactWithoutHistoryReportsNothingToCompact(t *testing.T) {
	store, err := session.New(t.TempDir(), t.TempDir(), "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	defer store.Close()
	fake := &fakeProvider{}
	cfg := config.Default()
	runner := New(testModel, cfg.Compaction, fake, store, tools.New(t.TempDir(), false))
	if err := runner.Compact(context.Background(), func(Event) {}); !errors.Is(err, ErrNothingToCompact) {
		t.Fatalf("Compact error = %v, want ErrNothingToCompact", err)
	}
	if fake.completeCalls != 0 {
		t.Fatalf("complete calls = %d, want 0", fake.completeCalls)
	}
}

func TestCompactFallsBackToIsolatedSummaryWhenLiveContextWouldOverflow(t *testing.T) {
	store, err := session.New(t.TempDir(), t.TempDir(), "test", "system prompt")
	if err != nil {
		t.Fatal(err)
	}
	defer store.Close()
	for i := 0; i < 3; i++ {
		if _, err := store.AppendMessage(session.Message{Role: session.RoleUser, Parts: []session.Part{{Type: session.PartText, Text: strings.Repeat("question ", 80)}}}); err != nil {
			t.Fatal(err)
		}
		if _, err := store.AppendMessage(session.Message{Role: session.RoleAssistant, Parts: []session.Part{{Type: session.PartText, Text: strings.Repeat("answer ", 80)}}}); err != nil {
			t.Fatal(err)
		}
	}
	// The isolated request measures a serialized transcript, not the live
	// context, so its usage must not replace the estimated count.
	provider := &recordingProvider{usage: &session.Usage{PromptTokens: 99_999}}
	cfg := config.Default()
	model := testModel
	// A tiny window with a large reserve forces the isolated fallback once usage
	// plus the reserve no longer fits.
	model.ContextWindowTokens = 200
	cfg.Compaction.ReserveTokens = 150
	cfg.Compaction.KeepRecentTokens = 1
	runner := New(model, cfg.Compaction, provider, store, tools.New(t.TempDir(), false))
	var events []Event
	if err := runner.Compact(context.Background(), func(event Event) { events = append(events, event) }); err != nil {
		t.Fatal(err)
	}
	if len(events) == 0 || events[0].Kind != EventCompacted || !events[0].Estimated || events[0].Tokens == 99_999 {
		t.Fatalf("isolated compaction should report the estimate: %#v", events)
	}
	if len(provider.requests) != 1 {
		t.Fatalf("complete requests = %d, want 1", len(provider.requests))
	}
	request := provider.requests[0]
	if len(request) != 2 || request[0].Role != session.RoleSystem {
		t.Fatalf("fallback request is not the isolated two-message shape: %#v", request)
	}
	if len(provider.tools[0]) != 0 {
		t.Fatal("fallback request should not send the live tool roster")
	}
	if !strings.Contains(request[1].Text(), "[user]") {
		t.Fatalf("fallback request should carry the serialized history: %q", request[1].Text())
	}
}

func TestCompactReportsMeasuredContextFromLiveSummaryRequest(t *testing.T) {
	store, err := session.New(t.TempDir(), t.TempDir(), "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	defer store.Close()
	for i := 0; i < 3; i++ {
		if _, err := store.AppendMessage(session.Message{Role: session.RoleUser, Parts: []session.Part{{Type: session.PartText, Text: strings.Repeat("question ", 80)}}}); err != nil {
			t.Fatal(err)
		}
		if _, err := store.AppendMessage(session.Message{Role: session.RoleAssistant, Parts: []session.Part{{Type: session.PartText, Text: strings.Repeat("answer ", 80)}}}); err != nil {
			t.Fatal(err)
		}
	}
	provider := &recordingProvider{usage: &session.Usage{PromptTokens: 12_345, CompletionTokens: 7}}
	cfg := config.Default()
	model := testModel
	model.ContextWindowTokens = 1_000_000
	cfg.Compaction.KeepRecentTokens = 100
	runner := New(model, cfg.Compaction, provider, store, tools.New(t.TempDir(), false))
	var events []Event
	if err := runner.Compact(context.Background(), func(event Event) { events = append(events, event) }); err != nil {
		t.Fatal(err)
	}
	request := []session.ContextMessage{{Message: session.TextMessage(session.RoleUser, CompactSummaryRequest)}}
	want := 12_345 - estimateContext(request, nil)
	if len(events) == 0 || events[0].Kind != EventCompacted || events[0].Estimated || events[0].Tokens != want {
		t.Fatalf("compacted event = %#v, want measured %d tokens", events, want)
	}
}

func TestCompactUsesPreviousSummaryWithoutReSummarizingIt(t *testing.T) {
	store, err := session.New(t.TempDir(), t.TempDir(), "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	defer store.Close()
	for _, content := range []string{"old question", "old answer"} {
		role := session.RoleUser
		if content == "old answer" {
			role = session.RoleAssistant
		}
		if _, err := store.AppendMessage(session.Message{Role: role, Parts: []session.Part{{Type: session.PartText, Text: content}}}); err != nil {
			t.Fatal(err)
		}
	}
	kept, err := store.AppendMessage(session.Message{Role: session.RoleUser, Parts: []session.Part{{Type: session.PartText, Text: "kept question"}}})
	if err != nil {
		t.Fatal(err)
	}
	if _, err := store.AppendCompaction("existing summary", kept, 100, false, nil); err != nil {
		t.Fatal(err)
	}
	if _, err := store.AppendMessage(session.Message{Role: session.RoleUser, Parts: []session.Part{{Type: session.PartText, Text: strings.Repeat("more ", 200)}}}); err != nil {
		t.Fatal(err)
	}

	provider := &recordingProvider{}
	cfg := config.Default()
	model := testModel
	model.ContextWindowTokens = 1_000_000
	cfg.Compaction.KeepRecentTokens = 1
	runner := New(model, cfg.Compaction, provider, store, tools.New(t.TempDir(), false))
	if err := runner.Compact(context.Background(), func(Event) {}); err != nil {
		t.Fatal(err)
	}
	if len(provider.requests) != 1 {
		t.Fatalf("complete requests = %d, want 1", len(provider.requests))
	}
	request := provider.requests[0]
	// The summary request is the live prefix plus one trailing user message, so
	// the provider can reuse the cache the streaming turn populated.
	if request[0].Role != session.RoleSystem || request[0].Text() != "system" {
		t.Fatalf("system prompt not reused verbatim: %#v", request[0])
	}
	if len(request) != 5 {
		t.Fatalf("request has %d messages, want the live prefix plus the trailing request", len(request))
	}
	last := request[len(request)-1]
	if last.Role != session.RoleUser || !strings.Contains(last.Text(), "Context is running low") {
		t.Fatalf("trailing summary request missing: %#v", last)
	}
	if !strings.Contains(request[1].Text(), "existing summary") {
		t.Fatalf("previous summary missing from projected prefix: %q", request[1].Text())
	}
	if len(provider.tools[0]) == 0 {
		t.Fatal("tool roster not sent, so the cached prefix would not match the live turn")
	}
}

type recordingProvider struct {
	requests [][]session.Message
	tools    [][]provider.Tool
	// usage is what each summary response reports, or nil for none.
	usage *session.Usage
}

func (p *recordingProvider) Stream(context.Context, []session.Message, []provider.Tool, func(provider.Event)) (session.Message, error) {
	return session.Message{Role: session.RoleAssistant, Parts: []session.Part{{Type: session.PartText, Text: "done"}}}, nil
}

func (p *recordingProvider) Complete(_ context.Context, messages []session.Message, toolList []provider.Tool, _ tokens.Count) (session.Message, error) {
	p.requests = append(p.requests, messages)
	p.tools = append(p.tools, toolList)
	return session.Message{Role: session.RoleAssistant, Parts: []session.Part{{Type: session.PartText, Text: "summary"}}, Usage: p.usage}, nil
}

func newTestEntryID(t *testing.T) typedid.EntryID {
	t.Helper()
	id, err := typedid.NewEntryID()
	if err != nil {
		t.Fatal(err)
	}
	return id
}

func TestSystemPromptRendersContextFilesInOrder(t *testing.T) {
	prompt := SystemPrompt("/work/project", "/usr/local/bin/kon", []contextfiles.File{
		{Path: "/work/AGENTS.md", Content: "outer rules\n"},
		{Path: "/work/project/AGENTS.md", Content: "inner rules"},
	}, "")
	if !strings.Contains(prompt, "<project_instructions path=\"/work/AGENTS.md\">\nouter rules\n</project_instructions>") {
		t.Fatalf("outer file not rendered with its path:\n%s", prompt)
	}
	outer := strings.Index(prompt, "outer rules")
	inner := strings.Index(prompt, "inner rules")
	if outer < 0 || inner < 0 || outer > inner {
		t.Fatalf("context files not rendered outermost first:\n%s", prompt)
	}
	if cwd := strings.Index(prompt, "Current working directory: /work/project"); cwd < 0 || cwd < inner {
		t.Fatalf("cwd should follow the context files:\n%s", prompt)
	}
}

func TestSystemPromptOmitsContextSectionWhenEmpty(t *testing.T) {
	prompt := SystemPrompt("/work", "/usr/local/bin/kon", nil, "")
	if strings.Contains(prompt, "project_instructions") || strings.Contains(prompt, "Project-specific instructions") {
		t.Fatalf("empty context files produced a section:\n%s", prompt)
	}
}

func TestSystemPromptPointsToBundledDocs(t *testing.T) {
	executable := "/path with spaces/kon"
	prompt := SystemPrompt("/work", executable, nil, "")
	if !strings.Contains(prompt, "Current kon executable: "+executable) {
		t.Fatalf("executable path missing from prompt:\n%s", prompt)
	}
	if !strings.Contains(prompt, "run `kon docs` using the executable path above") || !strings.Contains(prompt, "read the relevant bundled documentation") {
		t.Fatalf("self-documentation instruction missing from prompt:\n%s", prompt)
	}
}

func TestSystemPromptPlacesInstructionsLast(t *testing.T) {
	prompt := SystemPrompt("/work", "/usr/local/bin/kon", []contextfiles.File{{Path: "/work/AGENTS.md", Content: "project rules"}}, "user rules")
	project := strings.Index(prompt, "project rules")
	instructions := strings.Index(prompt, "Additional user instructions:\nuser rules")
	if project < 0 || instructions < 0 || project > instructions {
		t.Fatalf("configured instructions should come last:\n%s", prompt)
	}
}
