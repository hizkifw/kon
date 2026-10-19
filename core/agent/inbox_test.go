package agent

import (
	"context"
	"encoding/json"
	"reflect"
	"testing"

	"kon.kitsu.red/core/provider"
	"kon.kitsu.red/core/session"
	"kon.kitsu.red/core/tokens"
	"kon.kitsu.red/core/tool"
	"kon.kitsu.red/core/typedid"
)

// steeringProvider pushes steering into the inbox while its first response
// streams, as a user typing mid-turn would, and records what each request
// ended with.
type steeringProvider struct {
	inbox  *Inbox
	steers []string
	// calls are pushed as calls the user makes, shown as "!echo".
	calls    []Call
	toolCall bool
	requests [][]session.Message
}

func (p *steeringProvider) Stream(_ context.Context, messages []session.Message, _ []session.ToolDefinition, _ func(provider.Event)) (session.Message, error) {
	p.requests = append(p.requests, messages)
	if len(p.requests) > 1 {
		return session.TextMessage(session.RoleAssistant, "adjusted"), nil
	}
	for _, steer := range p.steers {
		p.inbox.Push(steer)
	}
	for _, call := range p.calls {
		p.inbox.PushCall("!echo", call)
	}
	if p.toolCall {
		return session.Message{Role: session.RoleAssistant, Parts: []session.Part{
			{Type: session.PartToolCall, ToolCallID: typedid.ExternalToolCallID("call"), ToolName: "unknown", ToolInput: []byte(`{}`)},
		}}, nil
	}
	return session.TextMessage(session.RoleAssistant, "finished"), nil
}

func (p *steeringProvider) Complete(context.Context, []session.Message, []session.ToolDefinition, tokens.Count, func(provider.Event)) (session.Message, error) {
	return session.Message{}, nil
}

func runSteered(t *testing.T, p *steeringProvider) []Event {
	t.Helper()
	store, err := newSession(t.TempDir(), t.TempDir(), "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	defer store.Close()
	p.inbox = &Inbox{}
	var events []Event
	runner := newRunner(Limits{}, p, store, echoTool{})
	if err := runner.Run(context.Background(), "task", p.inbox, func(e Event) { events = append(events, e) }); err != nil {
		t.Fatal(err)
	}
	return events
}

func steeredText(events []Event) []string {
	var texts []string
	for _, e := range events {
		if e.Kind == EventSteered {
			texts = append(texts, e.Text)
		}
	}
	return texts
}

func TestSteeringFollowsToolResultsInNextRequest(t *testing.T) {
	p := &steeringProvider{steers: []string{"use v2"}, toolCall: true}
	events := runSteered(t, p)
	if len(p.requests) != 2 {
		t.Fatalf("requests = %d, want 2", len(p.requests))
	}
	next := p.requests[1]
	last, before := next[len(next)-1], next[len(next)-2]
	if last.Role != session.RoleUser || last.Text() != "use v2" || before.Role != session.RoleTool {
		t.Fatalf("second request ends with %s %q after %s, want the steer after the tool result", last.Role, last.Text(), before.Role)
	}
	if got := steeredText(events); len(got) != 1 || got[0] != "use v2" {
		t.Fatalf("steered events = %q", got)
	}
}

func TestSteeringDuringFinalResponseContinuesTheRun(t *testing.T) {
	p := &steeringProvider{steers: []string{"also add tests"}}
	runSteered(t, p)
	if len(p.requests) != 2 {
		t.Fatalf("requests = %d, want the run to continue with the steer", len(p.requests))
	}
	if last := p.requests[1][len(p.requests[1])-1]; last.Role != session.RoleUser || last.Text() != "also add tests" {
		t.Fatalf("second request ends with %s %q", last.Role, last.Text())
	}
}

func TestStackedSteeringArrivesAsOneMessage(t *testing.T) {
	p := &steeringProvider{steers: []string{"first", "second"}, toolCall: true}
	events := runSteered(t, p)
	next := p.requests[1]
	if users := countRole(next, session.RoleUser); users != 2 {
		t.Fatalf("user messages = %d, want the prompt and one joined steer", users)
	}
	if got := steeredText(events); len(got) != 1 || got[0] != "first\n\nsecond" {
		t.Fatalf("steered events = %q", got)
	}
}

func TestInboxRemoveFailsOnceTaken(t *testing.T) {
	inbox := &Inbox{}
	inbox.Push("a")
	inbox.Push("b")
	if text, ok := inbox.Remove(1); !ok || text != "b" {
		t.Fatalf("Remove(1) = %q, %v", text, ok)
	}
	inbox.Take()
	if _, ok := inbox.Remove(0); ok {
		t.Fatal("Remove succeeded after the runner took the message")
	}
	var none *Inbox
	if none.Take() != nil || none.Pending() != nil {
		t.Fatal("a nil inbox is not empty")
	}
}

func countRole(messages []session.Message, role session.Role) int {
	n := 0
	for _, m := range messages {
		if m.Role == role {
			n++
		}
	}
	return n
}

func echoCall() Call {
	return Call{Tool: "echo", Arguments: json.RawMessage(`{"x":1}`), Message: func(r tool.Result) string { return "ran " + r.Content }}
}

// A call the user steers with is made at delivery, shown before its tool
// events, and its result leads the steering it was sent with.
func TestSteeredCallRunsAtDelivery(t *testing.T) {
	p := &steeringProvider{steers: []string{"then explain"}, calls: []Call{echoCall()}}
	events := runSteered(t, p)
	if len(p.requests) != 2 {
		t.Fatalf("requests = %d, want the run to continue with the call", len(p.requests))
	}
	if last := p.requests[1][len(p.requests[1])-1]; last.Role != session.RoleUser || last.Text() != "ran {\"x\":1}\n\nthen explain" {
		t.Fatalf("second request ends with %s %q", last.Role, last.Text())
	}
	var kinds []EventKind
	for _, e := range events {
		if e.Kind == EventSteered || e.Kind == EventToolStart || e.Kind == EventToolDone {
			kinds = append(kinds, e.Kind)
		}
	}
	if !reflect.DeepEqual(kinds, []EventKind{EventSteered, EventToolStart, EventToolDone, EventSteered}) {
		t.Fatalf("events = %v", kinds)
	}
	if got := steeredText(events); !reflect.DeepEqual(got, []string{"!echo", "then explain"}) {
		t.Fatalf("steered events = %q", got)
	}
}

// cancelTool stops the run while it executes, as Esc during a command would.
type cancelTool struct{ cancel context.CancelFunc }

func (cancelTool) Definition() session.ToolDefinition {
	return session.ToolDefinition{Name: "cancel", Parameters: json.RawMessage(`{"type":"object"}`)}
}

func (c cancelTool) Run(context.Context, tool.Env, json.RawMessage) (tool.Result, error) {
	c.cancel()
	return tool.Result{Content: "stopped"}, nil
}

// An interrupted call is dropped, and steering not yet sent goes back to the
// inbox in its order.
func TestInterruptedSteeredCallKeepsTheRest(t *testing.T) {
	store, err := newSession(t.TempDir(), t.TempDir(), "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	defer store.Close()
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	runner := newRunner(Limits{}, &fakeProvider{}, store, cancelTool{cancel}, echoTool{})
	inbox := &Inbox{}
	inbox.Push("a")
	inbox.PushCall("!cancel", Call{Tool: "cancel", Arguments: json.RawMessage(`{}`), Message: func(tool.Result) string { return "sent" }})
	inbox.PushCall("!echo", echoCall())
	inbox.Push("b")
	before := len(store.ActivePath())
	if _, err := runner.deliver(ctx, inbox, func(Event) {}); err != context.Canceled {
		t.Fatalf("err = %v, want canceled", err)
	}
	if got := inbox.Take(); !reflect.DeepEqual(got, []string{"a", "!echo", "b"}) {
		t.Fatalf("inbox = %q", got)
	}
	if after := len(store.ActivePath()); after != before {
		t.Fatalf("session grew from %d to %d entries", before, after)
	}
}
