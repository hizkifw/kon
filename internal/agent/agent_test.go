package agent

import (
	"context"
	"errors"
	"slices"
	"strings"
	"testing"

	"github.com/hizkifw/kon/internal/contextfiles"
	"github.com/hizkifw/kon/internal/provider"
	"github.com/hizkifw/kon/internal/session"
	"github.com/hizkifw/kon/internal/tokens"
	"github.com/hizkifw/kon/internal/tools"
	"github.com/hizkifw/kon/internal/typedid"
)

// testLimits mirrors the default compaction budgets for a model whose context
// window is unknown.
var testLimits = Limits{ReserveTokens: 16_384, KeepRecentTokens: 20_000}

type fakeProvider struct {
	completeCalls int
	streamCalls   int
	// streams records each streamed request, so a test can check what the
	// model was actually sent rather than what the store holds afterwards.
	streams [][]session.Message
}

func (f *fakeProvider) Stream(_ context.Context, messages []session.Message, _ []session.ToolDefinition, emit func(provider.Event)) (session.Message, error) {
	f.streamCalls++
	f.streams = append(f.streams, messages)
	emit(provider.Event{Text: "done"})
	return session.Message{Role: session.RoleAssistant, Parts: []session.Part{{Type: session.PartText, Text: "done"}}, Finish: "stop", Usage: &session.Usage{PromptTokens: 100, CompletionTokens: 1, TotalTokens: 101}}, nil
}

func (f *fakeProvider) Complete(_ context.Context, _ []session.Message, _ []session.ToolDefinition, _ tokens.Count, emit func(provider.Event)) (session.Message, error) {
	f.completeCalls++
	emit(provider.Event{Text: "weighing it", Thinking: true})
	emit(provider.Event{Text: "summary"})
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
	limits := testLimits
	limits.ContextWindow = 500
	limits.ReserveTokens = 100
	limits.KeepRecentTokens = 100
	runner := New(limits, fake, store, tools.New(t.TempDir(), false, nil))
	if err := runner.Run(context.Background(), "new work", nil, func(Event) {}); err != nil {
		t.Fatal(err)
	}
	if fake.completeCalls == 0 || fake.streamCalls != 1 {
		t.Fatalf("complete=%d stream=%d", fake.completeCalls, fake.streamCalls)
	}
	// The streamed request is checked rather than the store, because only it
	// proves the compaction ran before the request went out. Its first message
	// must be the persisted system prompt byte for byte, since provider prompt
	// caches key on that leading prefix; the two older turns must be folded
	// into the summary, leaving only the newest turn and the new prompt.
	root := store.ActivePath()[0].Message
	want := []struct {
		role session.Role
		text string
	}{
		{session.RoleSystem, root.Text()},
		{session.RoleUser, session.CompactionSummaryPrefix + "summary" + session.CompactionSummarySuffix},
		{session.RoleUser, strings.Repeat("question ", 80)},
		{session.RoleAssistant, strings.Repeat("answer ", 80)},
		{session.RoleUser, "new work"},
	}
	request := fake.streams[0]
	if len(request) != len(want) {
		t.Fatalf("request has %d messages, want %d: %#v", len(request), len(want), request)
	}
	for i, w := range want {
		if request[i].Role != w.role || request[i].Text() != w.text {
			t.Fatalf("request[%d] = %s %q, want %s %q", i, request[i].Role, request[i].Text(), w.role, w.text)
		}
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
	runner := New(testLimits, &fakeProvider{}, reopened, tools.New(t.TempDir(), false, nil))
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
	runner := New(testLimits, &fakeProvider{}, store, tools.New(t.TempDir(), false, nil))
	if tokens, ok := runner.ContextUsage(); ok {
		t.Fatalf("ContextUsage = (%d, true), want unknown", tokens)
	}
}

// interruptingProvider simulates a stream that emits a partial turn and then
// fails, the way a user cancellation or a dropped connection arrives.
type interruptingProvider struct {
	calls int
}

func (p *interruptingProvider) Stream(_ context.Context, _ []session.Message, _ []session.ToolDefinition, emit func(provider.Event)) (session.Message, error) {
	p.calls++
	emit(provider.Event{Text: "partial thought", Thinking: true})
	emit(provider.Event{Text: "half an answer"})
	return session.Message{
		Role:        session.RoleAssistant,
		Interrupted: true,
		Parts: []session.Part{
			{Type: session.PartReasoning, Text: "partial thought"},
			{Type: session.PartText, Text: "half an answer"},
		},
	}, context.Canceled
}

func (p *interruptingProvider) Complete(_ context.Context, _ []session.Message, _ []session.ToolDefinition, _ tokens.Count, _ func(provider.Event)) (session.Message, error) {
	return session.Message{Role: session.RoleAssistant, Parts: []session.Part{{Type: session.PartText, Text: "summary"}}}, nil
}

func TestRunnerPersistsPartialTurnOnInterruptedStream(t *testing.T) {
	store, err := session.New(t.TempDir(), t.TempDir(), "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	defer store.Close()
	fake := &interruptingProvider{}
	runner := New(Limits{}, fake, store, tools.New(t.TempDir(), false, nil))
	err = runner.Run(context.Background(), "hello", nil, func(Event) {})
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("Run error = %v, want context.Canceled", err)
	}
	// A follow-up turn continues from the partial assistant message only if the
	// projected context carries it, so the projection is checked, not the log.
	contextMessages, err := store.Context()
	if err != nil {
		t.Fatal(err)
	}
	last := contextMessages[len(contextMessages)-1].Message
	if last.Role != session.RoleAssistant || !last.Interrupted || last.Text() != "half an answer" {
		t.Fatalf("partial turn was not persisted: %#v", last)
	}
	if len(last.Parts) != 2 || last.Parts[0].Type != session.PartReasoning {
		t.Fatalf("partial reasoning was not persisted: %#v", last.Parts)
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
			runner := New(Limits{}, c.provider, store, tools.New(t.TempDir(), false, nil))
			_ = runner.Run(context.Background(), "hello", nil, func(Event) {})
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

func (toolCallProvider) Stream(context.Context, []session.Message, []session.ToolDefinition, func(provider.Event)) (session.Message, error) {
	return session.Message{Role: session.RoleAssistant, Parts: []session.Part{
		{Type: session.PartToolCall, ToolCallID: typedid.ExternalToolCallID("first"), ToolName: "unknown", ToolInput: []byte(`{}`)},
		{Type: session.PartToolCall, ToolCallID: typedid.ExternalToolCallID("second"), ToolName: "unknown", ToolInput: []byte(`{}`)},
	}}, nil
}

func (toolCallProvider) Complete(context.Context, []session.Message, []session.ToolDefinition, tokens.Count, func(provider.Event)) (session.Message, error) {
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
	runner := New(Limits{}, toolCallProvider{}, store, tools.New(t.TempDir(), false, nil))
	if err := runner.Run(ctx, "hello", nil, func(Event) {}); !errors.Is(err, context.Canceled) {
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

type reasoningProvider struct{}

func (reasoningProvider) Stream(_ context.Context, _ []session.Message, _ []session.ToolDefinition, emit func(provider.Event)) (session.Message, error) {
	emit(provider.Event{Text: "pondering", Thinking: true})
	emit(provider.Event{Text: "answer"})
	return session.Message{Role: session.RoleAssistant, Parts: []session.Part{{Type: session.PartText, Text: "answer"}}, Finish: "stop"}, nil
}

func (reasoningProvider) Complete(_ context.Context, _ []session.Message, _ []session.ToolDefinition, _ tokens.Count, _ func(provider.Event)) (session.Message, error) {
	return session.Message{Role: session.RoleAssistant, Parts: []session.Part{{Type: session.PartText, Text: "summary"}}}, nil
}

func TestRunnerEmitsThinkingBeforeText(t *testing.T) {
	store, err := session.New(t.TempDir(), t.TempDir(), "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	defer store.Close()
	runner := New(Limits{}, reasoningProvider{}, store, tools.New(t.TempDir(), false, nil))
	var kinds []EventKind
	if err := runner.Run(context.Background(), "hello", nil, func(event Event) { kinds = append(kinds, event.Kind) }); err != nil {
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
	// The kept window fills up at the tool result, so the cut must back off
	// past it. The only earlier turn boundary is the first user message, and a
	// cut there would leave nothing to summarize, so the cut falls back to the
	// assistant call that opens the tool group.
	if cut := selectCut(items, 50); cut != 2 {
		t.Fatalf("cut = %d, want 2, the assistant tool call", cut)
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
	// A window large enough that automatic compaction would not trigger, so a
	// successful compaction can only come from the manual force path.
	limits := testLimits
	limits.ContextWindow = 1_000_000
	limits.ReserveTokens = 16_384
	limits.KeepRecentTokens = 100
	runner := New(limits, fake, store, tools.New(t.TempDir(), false, nil))
	var compacted []Event
	if err := runner.Compact(context.Background(), func(event Event) { compacted = append(compacted, event) }); err != nil {
		t.Fatal(err)
	}
	if fake.completeCalls != 1 || fake.streamCalls != 0 {
		t.Fatalf("complete=%d stream=%d", fake.completeCalls, fake.streamCalls)
	}
	// The summary streams between the start and the end, without its reasoning.
	var kinds []EventKind
	for _, event := range compacted {
		kinds = append(kinds, event.Kind)
	}
	if want := []EventKind{EventCompacting, EventCompactionText, EventCompacted, EventUsage}; !slices.Equal(kinds, want) ||
		compacted[0].Tokens <= 0 || compacted[1].Text != "summary" || compacted[2].Text != "summary" {
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

// compactedEvent finds the event that ends a compaction.
func compactedEvent(events []Event) (Event, bool) {
	for _, event := range events {
		if event.Kind == EventCompacted {
			return event, true
		}
	}
	return Event{}, false
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
	limits := testLimits
	// A tiny window with a large reserve forces the isolated fallback once usage
	// plus the reserve no longer fits.
	limits.ContextWindow = 200
	limits.ReserveTokens = 150
	limits.KeepRecentTokens = 1
	runner := New(limits, provider, store, tools.New(t.TempDir(), false, nil))
	var events []Event
	if err := runner.Compact(context.Background(), func(event Event) { events = append(events, event) }); err != nil {
		t.Fatal(err)
	}
	if compacted, ok := compactedEvent(events); !ok || !compacted.Estimated || compacted.Tokens == 99_999 {
		t.Fatalf("isolated compaction should report the estimate: %#v", events)
	}
	if len(provider.requests) != 1 {
		t.Fatalf("complete requests = %d, want 1", len(provider.requests))
	}
	request := provider.requests[0]
	if len(request) != 2 || request[0].Role != session.RoleSystem || request[0].Text() != "system prompt" {
		t.Fatalf("fallback request is not the system prompt and one message: %#v", request)
	}
	if len(provider.tools[0]) != 0 {
		t.Fatal("fallback request should not send the live tool roster")
	}
	if text := request[1].Text(); !strings.Contains(text, "[user]") || !strings.HasSuffix(text, CompactSummaryRequest) {
		t.Fatalf("fallback request should carry the serialized history, then the instruction: %q", text)
	}
}

// The instruction merges a prior checkpoint it finds in <compacted-summary>
// tags, so the fallback carries the prior summary in them, as the live
// context does.
func TestIsolatedSummaryCarriesThePriorCheckpoint(t *testing.T) {
	store, err := session.New(t.TempDir(), t.TempDir(), "test", "system prompt")
	if err != nil {
		t.Fatal(err)
	}
	defer store.Close()
	kept, err := store.AppendMessage(session.Message{Role: session.RoleUser, Parts: []session.Part{{Type: session.PartText, Text: "kept question"}}})
	if err != nil {
		t.Fatal(err)
	}
	if _, err := store.AppendCompaction("existing summary", kept, 100, false, nil); err != nil {
		t.Fatal(err)
	}
	for i := 0; i < 3; i++ {
		if _, err := store.AppendMessage(session.Message{Role: session.RoleAssistant, Parts: []session.Part{{Type: session.PartText, Text: strings.Repeat("answer ", 80)}}}); err != nil {
			t.Fatal(err)
		}
		if _, err := store.AppendMessage(session.Message{Role: session.RoleUser, Parts: []session.Part{{Type: session.PartText, Text: strings.Repeat("question ", 80)}}}); err != nil {
			t.Fatal(err)
		}
	}
	provider := &recordingProvider{}
	limits := testLimits
	limits.ContextWindow = 200
	limits.ReserveTokens = 150
	limits.KeepRecentTokens = 1
	runner := New(limits, provider, store, tools.New(t.TempDir(), false, nil))
	if err := runner.Compact(context.Background(), func(Event) {}); err != nil {
		t.Fatal(err)
	}
	checkpoint := session.CompactionSummaryPrefix + "existing summary" + session.CompactionSummarySuffix
	if text := provider.requests[0][1].Text(); !strings.Contains(text, checkpoint) {
		t.Fatalf("fallback transcript lost the prior checkpoint: %q", text)
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
	limits := testLimits
	limits.ContextWindow = 1_000_000
	limits.KeepRecentTokens = 100
	runner := New(limits, provider, store, tools.New(t.TempDir(), false, nil))
	var events []Event
	if err := runner.Compact(context.Background(), func(event Event) { events = append(events, event) }); err != nil {
		t.Fatal(err)
	}
	// The report measured the live context plus the trailing summary request.
	// Only that request's estimate, well under a thousand tokens, comes back
	// out, so the measured size sits just below the report and far above the
	// byte estimate of this small fixture.
	if compacted, ok := compactedEvent(events); !ok || compacted.Estimated || compacted.Tokens <= 11_345 || compacted.Tokens >= 12_345 {
		t.Fatalf("compacted event = %#v, want a measured size just under 12345 tokens", events)
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
	limits := testLimits
	limits.ContextWindow = 1_000_000
	limits.KeepRecentTokens = 1
	runner := New(limits, provider, store, tools.New(t.TempDir(), false, nil))
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
	if last.Role != session.RoleUser || last.Text() != CompactSummaryRequest {
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
	tools    [][]session.ToolDefinition
	// usage is what each summary response reports, or nil for none.
	usage *session.Usage
}

func (p *recordingProvider) Stream(context.Context, []session.Message, []session.ToolDefinition, func(provider.Event)) (session.Message, error) {
	return session.Message{Role: session.RoleAssistant, Parts: []session.Part{{Type: session.PartText, Text: "done"}}}, nil
}

func (p *recordingProvider) Complete(_ context.Context, messages []session.Message, toolList []session.ToolDefinition, _ tokens.Count, _ func(provider.Event)) (session.Message, error) {
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
	})
	if !strings.Contains(prompt, "<instructions path=\"/work/AGENTS.md\">\nouter rules\n</instructions>") {
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
	prompt := SystemPrompt("/work", "/usr/local/bin/kon", nil)
	if strings.Contains(prompt, "<instructions") || strings.Contains(prompt, "User and project instructions") {
		t.Fatalf("empty context files produced a section:\n%s", prompt)
	}
}

func TestSystemPromptPointsToBundledDocs(t *testing.T) {
	executable := "/path with spaces/kon"
	prompt := SystemPrompt("/work", executable, nil)
	if !strings.Contains(prompt, "Current kon executable: "+executable) {
		t.Fatalf("executable path missing from prompt:\n%s", prompt)
	}
}

func TestSystemPromptPointsToShellTools(t *testing.T) {
	prompt := SystemPrompt("/work", "/usr/local/bin/kon", nil)
	if !strings.Contains(prompt, "`<kon> tool webfetch <url>`") || !strings.Contains(prompt, "`<kon> tool --help`") {
		t.Fatalf("shell tools missing from prompt:\n%s", prompt)
	}
}
