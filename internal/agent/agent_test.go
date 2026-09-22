package agent

import (
	"context"
	"errors"
	"strings"
	"testing"

	"github.com/hizkifw/kon/internal/config"
	"github.com/hizkifw/kon/internal/provider"
	"github.com/hizkifw/kon/internal/session"
	"github.com/hizkifw/kon/internal/tools"
	"github.com/hizkifw/kon/internal/typedid"
)

type fakeProvider struct {
	completeCalls int
	streamCalls   int
}

func (f *fakeProvider) Stream(_ context.Context, _ []session.Message, _ []provider.Tool, emit func(provider.Event)) (session.Message, error) {
	f.streamCalls++
	emit(provider.Event{Text: "done"})
	return session.Message{Role: session.RoleAssistant, Content: "done", Finish: "stop", Usage: &session.Usage{PromptTokens: 100, CompletionTokens: 1, TotalTokens: 101}}, nil
}

func (f *fakeProvider) Complete(_ context.Context, _ []session.Message, _ []provider.Tool, _ int) (session.Message, error) {
	f.completeCalls++
	return session.Message{Role: session.RoleAssistant, Content: "summary", Usage: &session.Usage{PromptTokens: 50, CompletionTokens: 5, TotalTokens: 55}}, nil
}

func TestRunnerCompactsOlderTurnsBeforeRequest(t *testing.T) {
	store, err := session.New(t.TempDir(), t.TempDir(), "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	defer store.Close()
	for i := 0; i < 3; i++ {
		if _, err := store.AppendMessage(session.Message{Role: session.RoleUser, Content: strings.Repeat("question ", 80)}); err != nil {
			t.Fatal(err)
		}
		if _, err := store.AppendMessage(session.Message{Role: session.RoleAssistant, Content: strings.Repeat("answer ", 80)}); err != nil {
			t.Fatal(err)
		}
	}
	fake := &fakeProvider{}
	cfg := config.Default()
	model := cfg.Models[0]
	model.ContextWindowTokens = 500
	cfg.Compaction.ReserveTokens = 100
	cfg.Compaction.KeepRecentTokens = 100
	runner := New(model, cfg.Compaction, fake, store, tools.New(t.TempDir()))
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
	if _, err := store.AppendMessage(session.Message{Role: session.RoleUser, Content: "hi"}); err != nil {
		t.Fatal(err)
	}
	if _, err := store.AppendMessage(session.Message{
		Role: session.RoleAssistant, Content: "answer",
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
	runner := New(cfg.Models[0], cfg.Compaction, &fakeProvider{}, reopened, tools.New(t.TempDir()))
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
	if _, err := store.AppendMessage(session.Message{Role: session.RoleUser, Content: "hi"}); err != nil {
		t.Fatal(err)
	}
	cfg := config.Default()
	runner := New(cfg.Models[0], cfg.Compaction, &fakeProvider{}, store, tools.New(t.TempDir()))
	if tokens, ok := runner.ContextUsage(); ok {
		t.Fatalf("ContextUsage = (%d, true), want unknown", tokens)
	}
}

type reasoningProvider struct{}

func (reasoningProvider) Stream(_ context.Context, _ []session.Message, _ []provider.Tool, emit func(provider.Event)) (session.Message, error) {
	emit(provider.Event{Text: "pondering", Thinking: true})
	emit(provider.Event{Text: "answer"})
	return session.Message{Role: session.RoleAssistant, Content: "answer", Finish: "stop"}, nil
}

func (reasoningProvider) Complete(_ context.Context, _ []session.Message, _ []provider.Tool, _ int) (session.Message, error) {
	return session.Message{Role: session.RoleAssistant, Content: "summary"}, nil
}

func TestRunnerEmitsThinkingBeforeText(t *testing.T) {
	store, err := session.New(t.TempDir(), t.TempDir(), "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	defer store.Close()
	runner := New(config.Model{}, config.Compaction{}, reasoningProvider{}, store, tools.New(t.TempDir()))
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
		{EntryID: newTestEntryID(t), Message: session.Message{Role: session.RoleSystem, Content: "system"}},
		{EntryID: newTestEntryID(t), Message: session.Message{Role: session.RoleUser, Content: strings.Repeat("x", 100)}},
		{EntryID: newTestEntryID(t), Message: session.Message{Role: session.RoleAssistant, ToolCalls: []session.ToolCall{{ID: typedid.ExternalToolCallID("1")}}}},
		{EntryID: newTestEntryID(t), Message: session.Message{Role: session.RoleTool, Content: strings.Repeat("y", 100)}},
		{EntryID: newTestEntryID(t), Message: session.Message{Role: session.RoleAssistant, Content: "done"}},
		{EntryID: newTestEntryID(t), Message: session.Message{Role: session.RoleUser, Content: "new"}},
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
		if _, err := store.AppendMessage(session.Message{Role: session.RoleUser, Content: strings.Repeat("question ", 80)}); err != nil {
			t.Fatal(err)
		}
		if _, err := store.AppendMessage(session.Message{Role: session.RoleAssistant, Content: strings.Repeat("answer ", 80)}); err != nil {
			t.Fatal(err)
		}
	}
	fake := &fakeProvider{}
	cfg := config.Default()
	// A window large enough that automatic compaction would not trigger, so a
	// successful compaction can only come from the manual force path.
	model := cfg.Models[0]
	model.ContextWindowTokens = 1_000_000
	cfg.Compaction.ReserveTokens = 16_384
	cfg.Compaction.KeepRecentTokens = 100
	runner := New(model, cfg.Compaction, fake, store, tools.New(t.TempDir()))
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
		if message.Summary && strings.Contains(message.Message.Content, "summary") {
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
	runner := New(cfg.Models[0], cfg.Compaction, fake, store, tools.New(t.TempDir()))
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
		if _, err := store.AppendMessage(session.Message{Role: session.RoleUser, Content: strings.Repeat("question ", 80)}); err != nil {
			t.Fatal(err)
		}
		if _, err := store.AppendMessage(session.Message{Role: session.RoleAssistant, Content: strings.Repeat("answer ", 80)}); err != nil {
			t.Fatal(err)
		}
	}
	provider := &recordingProvider{}
	cfg := config.Default()
	model := cfg.Models[0]
	// A tiny window with a large reserve forces the isolated fallback once usage
	// plus the reserve no longer fits.
	model.ContextWindowTokens = 200
	cfg.Compaction.ReserveTokens = 150
	cfg.Compaction.KeepRecentTokens = 1
	runner := New(model, cfg.Compaction, provider, store, tools.New(t.TempDir()))
	if err := runner.Compact(context.Background(), func(Event) {}); err != nil {
		t.Fatal(err)
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
	if !strings.Contains(request[1].Content, "[user]") {
		t.Fatalf("fallback request should carry the serialized history: %q", request[1].Content)
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
		if _, err := store.AppendMessage(session.Message{Role: role, Content: content}); err != nil {
			t.Fatal(err)
		}
	}
	kept, err := store.AppendMessage(session.Message{Role: session.RoleUser, Content: "kept question"})
	if err != nil {
		t.Fatal(err)
	}
	if _, err := store.AppendCompaction("existing summary", kept, 100, false, nil); err != nil {
		t.Fatal(err)
	}
	if _, err := store.AppendMessage(session.Message{Role: session.RoleUser, Content: strings.Repeat("more ", 200)}); err != nil {
		t.Fatal(err)
	}

	provider := &recordingProvider{}
	cfg := config.Default()
	model := cfg.Models[0]
	model.ContextWindowTokens = 1_000_000
	cfg.Compaction.KeepRecentTokens = 1
	runner := New(model, cfg.Compaction, provider, store, tools.New(t.TempDir()))
	if err := runner.Compact(context.Background(), func(Event) {}); err != nil {
		t.Fatal(err)
	}
	if len(provider.requests) != 1 {
		t.Fatalf("complete requests = %d, want 1", len(provider.requests))
	}
	request := provider.requests[0]
	// The summary request is the live prefix plus one trailing user message, so
	// the provider can reuse the cache the streaming turn populated.
	if request[0].Role != session.RoleSystem || request[0].Content != "system" {
		t.Fatalf("system prompt not reused verbatim: %#v", request[0])
	}
	if len(request) != 5 {
		t.Fatalf("request has %d messages, want the live prefix plus the trailing request", len(request))
	}
	last := request[len(request)-1]
	if last.Role != session.RoleUser || !strings.Contains(last.Content, "Context is running low") {
		t.Fatalf("trailing summary request missing: %#v", last)
	}
	if !strings.Contains(request[1].Content, "existing summary") {
		t.Fatalf("previous summary missing from projected prefix: %q", request[1].Content)
	}
	if len(provider.tools[0]) == 0 {
		t.Fatal("tool roster not sent, so the cached prefix would not match the live turn")
	}
}

type recordingProvider struct {
	requests [][]session.Message
	tools    [][]provider.Tool
}

func (p *recordingProvider) Stream(context.Context, []session.Message, []provider.Tool, func(provider.Event)) (session.Message, error) {
	return session.Message{Role: session.RoleAssistant, Content: "done"}, nil
}

func (p *recordingProvider) Complete(_ context.Context, messages []session.Message, toolList []provider.Tool, _ int) (session.Message, error) {
	p.requests = append(p.requests, messages)
	p.tools = append(p.tools, toolList)
	return session.Message{Role: session.RoleAssistant, Content: "summary"}, nil
}

func newTestEntryID(t *testing.T) typedid.EntryID {
	t.Helper()
	id, err := typedid.NewEntryID()
	if err != nil {
		t.Fatal(err)
	}
	return id
}
