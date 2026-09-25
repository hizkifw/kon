package agent

import (
	"context"
	"testing"

	"github.com/hizkifw/kon/internal/provider"
	"github.com/hizkifw/kon/internal/session"
	"github.com/hizkifw/kon/internal/tokens"
	"github.com/hizkifw/kon/internal/tools"
	"github.com/hizkifw/kon/internal/typedid"
)

// steeringProvider pushes steering into the inbox while its first response
// streams, as a user typing mid-turn would, and records what each request
// ended with.
type steeringProvider struct {
	inbox    *Inbox
	steers   []string
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
	if p.toolCall {
		return session.Message{Role: session.RoleAssistant, Parts: []session.Part{
			{Type: session.PartToolCall, ToolCallID: typedid.ExternalToolCallID("call"), ToolName: "unknown", ToolInput: []byte(`{}`)},
		}}, nil
	}
	return session.TextMessage(session.RoleAssistant, "finished"), nil
}

func (p *steeringProvider) Complete(context.Context, []session.Message, []session.ToolDefinition, tokens.Count) (session.Message, error) {
	return session.Message{}, nil
}

func runSteered(t *testing.T, p *steeringProvider) []Event {
	t.Helper()
	store, err := session.New(t.TempDir(), t.TempDir(), "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	defer store.Close()
	p.inbox = &Inbox{}
	var events []Event
	runner := New(Limits{}, p, store, tools.New(t.TempDir(), false, nil))
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
