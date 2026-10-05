package acp

import (
	"context"
	"encoding/json"
	"io"
	"sync"
	"testing"
	"time"

	"kon.kitsu.red/core/acp"
	"kon.kitsu.red/core/agent"
	"kon.kitsu.red/core/typedid"
)

// heard is an acp.Handler that keeps the updates it is sent.
type heard struct {
	mu      sync.Mutex
	updates []any
}

func (h *heard) Update(_ string, update any) {
	h.mu.Lock()
	defer h.mu.Unlock()
	h.updates = append(h.updates, update)
}

func (h *heard) TurnStart(string)               {}
func (h *heard) TurnEnd(string, string, string) {}

// TestClientDrivesServer runs the published client against the server, so
// the two halves of the protocol cannot drift apart unnoticed.
func TestClientDrivesServer(t *testing.T) {
	runtime := newFakeRuntime(t)
	runtime.run = func(_ context.Context, _ agent.Prompt, _ *agent.Inbox, emit func(agent.Event)) error {
		args := `{"path":"a.go","old_text":"a","new_text":"b"}`
		emit(agent.Event{Kind: agent.EventToolStart, CallID: "call-1", Tool: "edit", Arguments: args})
		emit(agent.Event{Kind: agent.EventToolDone, CallID: "call-1", Tool: "edit", Arguments: args})
		emit(agent.Event{Kind: agent.EventText, Text: "done"})
		return nil
	}
	server := newServer(runtime)
	start := server.Start
	var instructions string
	server.Start = func(cwd string, id typedid.SessionID, given string) (Runtime, error) {
		instructions = given
		return start(cwd, id, given)
	}
	toServer, fromClient := io.Pipe()
	toClient, fromServer := io.Pipe()
	served := make(chan error, 1)
	go func() {
		served <- server.Serve(context.Background(), toServer, fromServer)
		fromServer.Close()
	}()
	h := &heard{}
	client := acp.New(toClient, fromClient, h)
	t.Cleanup(func() {
		fromClient.Close()
		select {
		case <-served:
		case <-time.After(5 * time.Second):
			t.Error("Serve did not return after the client hung up")
		}
	})
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()

	agentInfo, err := client.Initialize(ctx, "test", "dev")
	if err != nil {
		t.Fatal(err)
	}
	want := acp.AgentExtensions{Steer: true, Jobs: true, AgentTurns: true, Instructions: true}
	if agentInfo.ProtocolVersion != acp.ProtocolVersion || agentInfo.AgentCapabilities.Meta.Kon != want {
		t.Fatalf("initialize = %+v", agentInfo)
	}
	s, err := client.NewSession(ctx, "/work", "be terse")
	if err != nil {
		t.Fatal(err)
	}
	if s.SessionID != runtime.id.String() || instructions != "be terse" {
		t.Fatalf("session = %q with instructions %q", s.SessionID, instructions)
	}
	if len(s.ConfigOptions) != 2 || s.ConfigOptions[0].ID != acp.ConfigModel || s.ConfigOptions[1].ID != acp.ConfigEffort {
		t.Fatalf("config options = %+v", s.ConfigOptions)
	}
	if err := client.Steer(ctx, s.SessionID, "early"); !acp.IsCode(err, acp.CodeInvalidRequest) {
		t.Fatalf("steering an idle session: %v", err)
	}
	stop, err := client.Prompt(ctx, s.SessionID, []acp.ContentBlock{acp.TextBlock("hello")})
	if err != nil || stop != acp.StopEndTurn {
		t.Fatalf("Prompt = %q, %v", stop, err)
	}
	if options, err := client.SetConfig(ctx, s.SessionID, acp.ConfigEffort, acp.EffortDefault); err != nil || len(options) != 2 {
		t.Fatalf("SetConfig = %+v, %v", options, err)
	}

	h.mu.Lock()
	defer h.mu.Unlock()
	if len(h.updates) != 4 {
		t.Fatalf("updates = %+v", h.updates)
	}
	if u, ok := h.updates[0].(acp.CommandsUpdate); !ok || len(u.AvailableCommands) != 1 {
		t.Fatalf("first update = %+v", h.updates[0])
	}
	started, ok := h.updates[1].(acp.ToolCall)
	if !ok || started.SessionUpdate != acp.UpdateToolCall || started.Status != acp.StatusInProgress || !json.Valid(started.RawInput) {
		t.Fatalf("tool call = %+v", h.updates[1])
	}
	if len(started.Content) != 1 || started.Content[0].Type != "diff" || *started.Content[0].NewText != "b" {
		t.Fatalf("tool call content = %+v", started.Content)
	}
	if u, ok := h.updates[2].(acp.ToolCall); !ok || u.SessionUpdate != acp.UpdateToolProgress || u.Status != acp.StatusCompleted {
		t.Fatalf("tool call update = %+v", h.updates[2])
	}
	if u, ok := h.updates[3].(acp.ContentChunk); !ok || u.SessionUpdate != acp.UpdateAgentMessage || u.Content.Text != "done" {
		t.Fatalf("chunk = %+v", h.updates[3])
	}
}
