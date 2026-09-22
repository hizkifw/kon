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

func (f *fakeProvider) Complete(_ context.Context, _ []session.Message, _ int) (session.Message, error) {
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
	if !strings.Contains(contextMessages[0].Message.Content, "summary") {
		t.Fatal("compaction summary missing from projected context")
	}
}

type reasoningProvider struct{}

func (reasoningProvider) Stream(_ context.Context, _ []session.Message, _ []provider.Tool, emit func(provider.Event)) (session.Message, error) {
	emit(provider.Event{Text: "pondering", Thinking: true})
	emit(provider.Event{Text: "answer"})
	return session.Message{Role: session.RoleAssistant, Content: "answer", Finish: "stop"}, nil
}

func (reasoningProvider) Complete(_ context.Context, _ []session.Message, _ int) (session.Message, error) {
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
	if !strings.Contains(contextMessages[0].Message.Content, "summary") {
		t.Fatal("compaction summary missing from projected context")
	}
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

func newTestEntryID(t *testing.T) typedid.EntryID {
	t.Helper()
	id, err := typedid.NewEntryID()
	if err != nil {
		t.Fatal(err)
	}
	return id
}
