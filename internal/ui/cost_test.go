package ui

import (
	"context"
	"strings"
	"testing"
	"time"

	tea "charm.land/bubbletea/v2"

	"github.com/hizkifw/kon/internal/agent"
	"github.com/hizkifw/kon/internal/app"
	"github.com/hizkifw/kon/internal/history"
	"github.com/hizkifw/kon/internal/session"
)

// statusLine is the rendered status bar: the line above the prompt input.
func statusLine(m Model) string {
	lines := strings.Split(plain(m.View().Content), "\n")
	return lines[len(lines)-2]
}

// TestStatusLineLeavesTurnProgressToTheTranscript checks that the status bar
// no longer narrates a turn: the transcript's working indicator already does.
func TestStatusLineLeavesTurnProgressToTheTranscript(t *testing.T) {
	m := newTestModel(t)
	if got := statusLine(m); strings.Contains(got, "ready") {
		t.Fatalf("idle status line = %q", got)
	}
	m.input.SetValue("hi")
	updated, _ := m.submit()
	m = updated.(Model)
	for _, event := range []agent.Event{
		{Kind: agent.EventThinking, Text: "hm"},
		{Kind: agent.EventText, Text: "ok"},
		{Kind: agent.EventToolStart, Tool: "shell", Arguments: `{"command":"ls","timeout":5}`},
	} {
		m.applyAgentEvent(event)
		if got := statusLine(m); strings.Contains(got, "…") {
			t.Fatalf("status line narrates the turn after %v: %q", event.Kind, got)
		}
	}
	if m.runCancel != nil {
		m.runCancel()
	}
}

func TestStatusLineTotalsSessionAndSubagentSpend(t *testing.T) {
	m := newTestModel(t)
	if got := statusLine(m); strings.Contains(got, "$") {
		t.Fatalf("an unpriced session shows a cost: %q", got)
	}
	m.applyAgentEvent(agent.Event{Kind: agent.EventUsage, Tokens: 50, Cost: 0.004})
	if got := statusLine(m); !strings.HasSuffix(got, "· <$0.01") {
		t.Fatalf("status line = %q, want a cost under a cent", got)
	}
	m.applyAgentEvent(agent.Event{Kind: agent.EventUsage, Tokens: -1, Cost: 0.2})
	m.runtime.(*fakeRuntime).subagentCost = 1.3
	updated, _ := m.Update(m.loadSpend()())
	m = updated.(Model)
	if got := statusLine(m); !strings.HasSuffix(got, "· $1.50") {
		t.Fatalf("status line = %q, want the session and its subagents together", got)
	}
}

// TestStatusLineEstimatesWhileStreaming checks that each streamed chunk counts
// as a token toward the context and the cost until the provider's report
// replaces the estimate.
func TestStatusLineEstimatesWhileStreaming(t *testing.T) {
	runtime := &fakeRuntime{state: app.State{Phase: app.PhaseReady, Active: app.Model{Name: "m", ContextWindow: 1000, OutputPrice: 10000}}}
	m := New(context.Background(), "/tmp", "/tmp/config.json", runtime, history.New(t.TempDir()+"/history.jsonl"), nil)
	m.width, m.height = 80, 24
	m.resize()
	m.applyAgentEvent(agent.Event{Kind: agent.EventUsage, Tokens: 100, Cost: 1})
	for _, event := range []agent.Event{
		{Kind: agent.EventThinking, Text: "hm"},
		{Kind: agent.EventText, Text: "hello"},
		{Kind: agent.EventText, Text: " there"},
	} {
		m.applyAgentEvent(event)
	}
	if got := statusLine(m); !strings.Contains(got, "ctx ~103/1.0k") || !strings.HasSuffix(got, "· $1.03") {
		t.Fatalf("status line = %q, want three chunks estimated", got)
	}
	// A compaction summary is priced but does not add to the context.
	m.applyAgentEvent(agent.Event{Kind: agent.EventCompactionText, Text: "summary"})
	if got := statusLine(m); !strings.Contains(got, "ctx ~103/1.0k") || !strings.HasSuffix(got, "· $1.04") {
		t.Fatalf("status line = %q, want the summary priced only", got)
	}
	m.applyAgentEvent(agent.Event{Kind: agent.EventUsage, Tokens: 110, Cost: 0.5})
	if got := statusLine(m); !strings.Contains(got, "ctx 110/1.0k") || !strings.HasSuffix(got, "· $1.50") {
		t.Fatalf("status line = %q, want the reported usage in place of the estimate", got)
	}
}

// TestResumedSessionShowsWhatItSpent checks that the total survives a
// restart: it is seeded from the costs the session file recorded.
func TestResumedSessionShowsWhatItSpent(t *testing.T) {
	reply := session.TextMessage(session.RoleAssistant, "hello")
	reply.Usage = &session.Usage{PromptTokens: 10, CompletionTokens: 2, Cost: 2}
	runtime := &fakeRuntime{
		state: app.State{Phase: app.PhaseReady},
		entries: []session.Entry{
			{Type: session.EntryTypeMessage, Message: &reply},
			{Type: session.EntryTypeCompaction, Summary: "s", Usage: &session.Usage{Cost: 0.5}},
		},
		subagentCost: 0.25,
	}
	m := New(context.Background(), "/tmp", "/tmp/config.json", runtime, history.New(t.TempDir()+"/history.jsonl"), nil)
	m.width, m.height = 80, 24
	m.resize()
	if got := statusLine(m); !strings.HasSuffix(got, "· $2.50") {
		t.Fatalf("status line = %q, want the session's own recorded spend", got)
	}
	// Init reads the subagents' sessions once the first frame is out.
	updated, _ := m.Update(m.readSpend(false)(time.Now()))
	m = updated.(Model)
	if got := statusLine(m); !strings.HasSuffix(got, "· $2.75") {
		t.Fatalf("status line = %q, want the recorded spend", got)
	}
}

// TestSpendPollRunsOnlyWhileSubagentsMaySpend checks the ledger is read while
// a run or a background job is live, with one tick in flight, and that the
// poll stops once both have ended.
func TestSpendPollRunsOnlyWhileSubagentsMaySpend(t *testing.T) {
	m := newTestModel(t)
	if cmd := m.pollSpend(); cmd != nil {
		t.Fatal("an idle session without jobs polls the ledger")
	}
	m.runtime.(*fakeRuntime).jobs = 1
	m.jobs = 1
	if cmd := m.pollSpend(); cmd == nil {
		t.Fatal("a running job is not polled")
	}
	if cmd := m.pollSpend(); cmd != nil {
		t.Fatal("a second poll started while one is in flight")
	}
	var updated tea.Model
	updated, cmd := m.Update(m.readSpend(true)(time.Now()))
	if m = updated.(Model); cmd == nil {
		t.Fatal("the poll stopped while the job still runs")
	}
	m.runtime.(*fakeRuntime).jobs = 0
	updated, cmd = m.Update(m.readSpend(true)(time.Now()))
	if m = updated.(Model); cmd != nil || m.spendPolling {
		t.Fatal("the poll kept going after the last job exited")
	}
}

// TestFollowerKeepsReadingSubagentSpend checks that a session followed
// read-only keeps its subagent total current: the writer's runs and jobs are
// in another process, so neither can start the poll here.
func TestFollowerKeepsReadingSubagentSpend(t *testing.T) {
	m, runtime := newFollowingModel(t)
	updated, cmd := m.Update(m.readSpend(false)(time.Now()))
	if m = updated.(Model); cmd == nil {
		t.Fatal("a follower stopped reading subagent spend after the first read")
	}
	runtime.subagentCost = 0.75
	updated, cmd = m.Update(m.readSpend(true)(time.Now()))
	if m = updated.(Model); cmd == nil || m.subagentSpent != 0.75 {
		t.Fatalf("subagent spend = %v, poll continues = %v", m.subagentSpent, cmd != nil)
	}
}

// TestSpendReadForAnEarlierSessionIsDropped checks that a read still in
// flight when the session changes cannot show the old session's spend.
func TestSpendReadForAnEarlierSessionIsDropped(t *testing.T) {
	m := newTestModel(t)
	m.runtime.(*fakeRuntime).subagentCost = 4
	stale := m.readSpend(false)(time.Now())
	updated, _ := m.newSession()
	m = updated.(Model)
	updated, _ = m.Update(stale)
	if m = updated.(Model); m.subagentSpent != 0 {
		t.Fatalf("subagent spend = %v after a read for the previous session", m.subagentSpent)
	}
}

func TestFormatCost(t *testing.T) {
	for usd, want := range map[float64]string{0.0001: "<$0.01", 0.01: "$0.01", 0.456: "$0.46", 12.3456: "$12.35"} {
		if got := formatCost(usd); got != want {
			t.Errorf("formatCost(%v) = %q, want %q", usd, got, want)
		}
	}
}
