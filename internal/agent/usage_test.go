package agent

import (
	"context"
	"encoding/json"
	"errors"
	"strings"
	"testing"

	"github.com/hizkifw/kon/internal/provider"
	"github.com/hizkifw/kon/internal/session"
	"github.com/hizkifw/kon/internal/tokens"
	"github.com/hizkifw/kon/internal/tools"
)

func newUsageStore(t *testing.T) *session.Store {
	t.Helper()
	store, err := session.New(t.TempDir(), t.TempDir(), "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { store.Close() })
	return store
}

func appendMessage(t *testing.T, store *session.Store, message session.Message) {
	t.Helper()
	if _, err := store.AppendMessage(message); err != nil {
		t.Fatal(err)
	}
}

// toolTurn appends a prompt and an assistant tool call that reported usage,
// leaving the call unanswered.
func toolTurn(t *testing.T, store *session.Store, reported int) {
	t.Helper()
	appendMessage(t, store, session.TextMessage(session.RoleUser, "look"))
	appendMessage(t, store, session.Message{
		Role:  session.RoleAssistant,
		Parts: []session.Part{{Type: session.PartToolCall, ToolCallID: "call-1", ToolName: "read", ToolInput: json.RawMessage(`{"path":"a"}`)}},
		Usage: &session.Usage{PromptTokens: tokens.Count(reported), CompletionTokens: 10},
	})
}

func TestUsageAddsNewerMessagesToTheReportedSize(t *testing.T) {
	store := newUsageStore(t)
	toolTurn(t, store, 50_000)
	appendMessage(t, store, session.ToolResultMessage("call-1", "read", strings.Repeat("x", 4000)))
	runner := New(testLimits, &fakeProvider{}, store, tools.New(t.TempDir(), false))

	items, _ := store.Context()
	used, estimated := runner.usageFor(items)
	// The tool result is estimated on top of the report instead of the whole
	// context falling back to a bytes/4 guess.
	if !estimated || used < 50_010+1000 || used > 50_010+1100 {
		t.Fatalf("usageFor = (%d, %v), want the report plus about 1000 estimated", used, estimated)
	}
	if _, ok := runner.ContextUsage(); ok {
		t.Fatal("ContextUsage claimed a measurement that does not cover the tool result")
	}
}

func TestUsageDoesNotCountASynthesizedResultAsMeasured(t *testing.T) {
	store := newUsageStore(t)
	toolTurn(t, store, 50_000)
	runner := New(testLimits, &fakeProvider{}, store, tools.New(t.TempDir(), false))

	// The projection repairs the unanswered call with a placeholder result,
	// which the provider never measured.
	items, _ := store.Context()
	if last := items[len(items)-1]; last.Message.Role != session.RoleTool || !last.EntryID.IsZero() {
		t.Fatalf("expected a synthesized result, got %#v", last)
	}
	if _, estimated := runner.usageFor(items); !estimated {
		t.Fatal("a synthesized result was treated as measured")
	}
	if _, ok := runner.ContextUsage(); ok {
		t.Fatal("ContextUsage covered a synthesized result")
	}
}

func TestSeedUsageIgnoresReportsFromBeforeACompaction(t *testing.T) {
	store := newUsageStore(t)
	appendMessage(t, store, session.TextMessage(session.RoleUser, "one"))
	appendMessage(t, store, session.TextMessage(session.RoleAssistant, "answer"))
	kept, err := store.AppendMessage(session.TextMessage(session.RoleUser, "two"))
	if err != nil {
		t.Fatal(err)
	}
	// This reply survives the compaction, but its report measured the whole
	// context before the older turns were folded away.
	appendMessage(t, store, session.Message{Role: session.RoleAssistant, Parts: []session.Part{{Type: session.PartText, Text: "answer two"}}, Usage: &session.Usage{PromptTokens: 90_000, CompletionTokens: 10}})
	if _, err := store.AppendCompaction("summary", kept, 90_000, false, nil); err != nil {
		t.Fatal(err)
	}
	runner := New(testLimits, &fakeProvider{}, store, tools.New(t.TempDir(), false))
	items, _ := store.Context()
	if used, estimated := runner.usageFor(items); !estimated || used >= 90_000 {
		t.Fatalf("usageFor = (%d, %v); the pre-compaction report measured a context that is gone", used, estimated)
	}
}

// scriptedProvider answers summaries with a fixed response or error, and
// fails streams with a context overflow until a summary has been written.
type scriptedProvider struct {
	summary      session.Message
	summaryErr   error
	overflowOnce bool
	streams      int
	completes    int
}

func (p *scriptedProvider) Stream(_ context.Context, _ []session.Message, _ []session.ToolDefinition, emit func(provider.Event)) (session.Message, error) {
	p.streams++
	if p.overflowOnce && p.completes == 0 {
		return session.Message{}, &provider.APIError{Status: 400, Code: "context_length_exceeded", Message: "context length exceeded"}
	}
	emit(provider.Event{Text: "done"})
	return session.TextMessage(session.RoleAssistant, "done"), nil
}

func (p *scriptedProvider) Complete(context.Context, []session.Message, []session.ToolDefinition, tokens.Count) (session.Message, error) {
	p.completes++
	return p.summary, p.summaryErr
}

// longSession has enough history for a forced compaction to fold some away.
func longSession(t *testing.T, store *session.Store) {
	t.Helper()
	for range 4 {
		appendMessage(t, store, session.TextMessage(session.RoleUser, strings.Repeat("question ", 200)))
		appendMessage(t, store, session.TextMessage(session.RoleAssistant, strings.Repeat("answer ", 200)))
	}
}

func smallKeep() Limits {
	return Limits{ReserveTokens: 100, KeepRecentTokens: 300}
}

func hasCompaction(store *session.Store) bool {
	for _, entry := range store.ActivePath() {
		if entry.Type == session.EntryTypeCompaction {
			return true
		}
	}
	return false
}

func TestCompactRunsWithoutAContextWindow(t *testing.T) {
	store := newUsageStore(t)
	longSession(t, store)
	fake := &scriptedProvider{summary: session.TextMessage(session.RoleAssistant, "summary")}
	runner := New(smallKeep(), fake, store, tools.New(t.TempDir(), false))
	if err := runner.Compact(context.Background(), func(Event) {}); err != nil {
		t.Fatal(err)
	}
	if !hasCompaction(store) {
		t.Fatal("/compact did nothing for a model without a known context window")
	}
}

func TestCompactOnAShortSessionHasNothingToCompact(t *testing.T) {
	store := newUsageStore(t)
	appendMessage(t, store, session.TextMessage(session.RoleUser, "hi"))
	appendMessage(t, store, session.TextMessage(session.RoleAssistant, "hello"))
	limits := testLimits
	limits.ContextWindow = 100_000
	fake := &scriptedProvider{}
	runner := New(limits, fake, store, tools.New(t.TempDir(), false))
	if err := runner.Compact(context.Background(), func(Event) {}); !errors.Is(err, ErrNothingToCompact) {
		t.Fatalf("Compact = %v, want ErrNothingToCompact", err)
	}
	if fake.completes != 0 {
		t.Fatal("a summary was requested for a session with nothing to fold")
	}
}

func TestTruncatedSummaryIsNotPersisted(t *testing.T) {
	for name, fake := range map[string]*scriptedProvider{
		"cut off":        {summary: session.Message{Role: session.RoleAssistant, Parts: []session.Part{{Type: session.PartText, Text: "Goal: half a sum"}}, Finish: session.FinishLength}},
		"only reasoning": {summaryErr: provider.ErrOutputLimit},
	} {
		store := newUsageStore(t)
		longSession(t, store)
		runner := New(smallKeep(), fake, store, tools.New(t.TempDir(), false))
		err := runner.Compact(context.Background(), func(Event) {})
		if err == nil || !strings.Contains(err.Error(), "token limit") {
			t.Fatalf("%s: Compact = %v, want a token-limit error", name, err)
		}
		if hasCompaction(store) {
			t.Fatalf("%s: a truncated summary was persisted", name)
		}
	}
}

func TestOverflowCompactsWithoutAContextWindow(t *testing.T) {
	store := newUsageStore(t)
	longSession(t, store)
	fake := &scriptedProvider{summary: session.TextMessage(session.RoleAssistant, "summary"), overflowOnce: true}
	runner := New(smallKeep(), fake, store, tools.New(t.TempDir(), false))
	if err := runner.Run(context.Background(), "next", func(Event) {}); err != nil {
		t.Fatal(err)
	}
	if !hasCompaction(store) || fake.streams != 2 {
		t.Fatalf("compacted = %v, streams = %d; want a compaction and one retry", hasCompaction(store), fake.streams)
	}
}
