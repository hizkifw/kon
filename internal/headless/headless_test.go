package headless

import (
	"bytes"
	"context"
	"encoding/json"
	"strings"
	"testing"

	"github.com/hizkifw/kon/internal/agent"
	"github.com/hizkifw/kon/internal/app"
	"github.com/hizkifw/kon/internal/typedid"
)

// scriptedRuntime replays a fixed event stream and then returns err.
type scriptedRuntime struct {
	id     typedid.SessionID
	events []agent.Event
	err    error
}

func (r *scriptedRuntime) Run(_ context.Context, _ string, _ *agent.Inbox, emit func(agent.Event)) error {
	for _, event := range r.events {
		emit(event)
	}
	return r.err
}
func (r *scriptedRuntime) SessionID() typedid.SessionID { return r.id }
func (r *scriptedRuntime) State() app.State {
	return app.State{Active: app.Model{Name: "fast"}, Phase: app.PhaseReady}
}

func newScript(t *testing.T, err error, events ...agent.Event) *scriptedRuntime {
	t.Helper()
	id, idErr := typedid.NewSessionID()
	if idErr != nil {
		t.Fatal(idErr)
	}
	return &scriptedRuntime{id: id, events: events, err: err}
}

// aToolTurn is a turn that narrates, calls a tool, and answers.
var aToolTurn = []agent.Event{
	{Kind: agent.EventThinking, Text: "plan"},
	{Kind: agent.EventText, Text: "Let me "},
	{Kind: agent.EventText, Text: "check."},
	{Kind: agent.EventAssistantDone},
	{Kind: agent.EventToolStart, CallID: "call_1", Tool: "shell", Arguments: `{"command":"ls","timeout":10}`},
	{Kind: agent.EventToolDone, CallID: "call_1", Tool: "shell", Arguments: `{"command":"ls","timeout":10}`, Text: "a.go\n"},
	{Kind: agent.EventUsage, Tokens: 120},
	{Kind: agent.EventText, Text: "There is one file.\n"},
	{Kind: agent.EventAssistantDone},
}

func TestTextStreamsEveryMessageToStdout(t *testing.T) {
	runtime := newScript(t, nil, aToolTurn...)
	var stdout, progress bytes.Buffer
	if err := Run(context.Background(), runtime, "hi", Output{Format: FormatText, Stdout: &stdout, Progress: &progress}); err != nil {
		t.Fatal(err)
	}
	if got, want := stdout.String(), "Let me check.\n\nThere is one file.\n"; got != want {
		t.Fatalf("stdout = %q, want %q", got, want)
	}
	lines := strings.Split(strings.TrimSpace(progress.String()), "\n")
	if len(lines) != 2 || !strings.HasPrefix(lines[0], "✓ shell") || lines[1] != "resume with: kon --resume "+runtime.id.String() {
		t.Fatalf("progress = %q", progress.String())
	}
}

func TestTextWithoutProgressWritesOnlyTheConversation(t *testing.T) {
	var stdout bytes.Buffer
	if err := Run(context.Background(), newScript(t, nil, aToolTurn...), "hi", Output{Format: FormatText, Stdout: &stdout}); err != nil {
		t.Fatal(err)
	}
	if strings.Contains(stdout.String(), "shell") || strings.Contains(stdout.String(), "resume") {
		t.Fatalf("stdout carries progress: %q", stdout.String())
	}
}

func TestTextEndsACancelledMessageLine(t *testing.T) {
	var stdout bytes.Buffer
	runtime := newScript(t, context.Canceled, agent.Event{Kind: agent.EventText, Text: "half an ans"})
	err := Run(context.Background(), runtime, "hi", Output{Format: FormatText, Stdout: &stdout})
	if err != context.Canceled || stdout.String() != "half an ans\n" {
		t.Fatalf("stdout = %q, err = %v", stdout.String(), err)
	}
}

func decodeLines(t *testing.T, b []byte) []map[string]any {
	t.Helper()
	var events []map[string]any
	for _, line := range bytes.Split(bytes.TrimSpace(b), []byte("\n")) {
		var event map[string]any
		if err := json.Unmarshal(line, &event); err != nil {
			t.Fatalf("line %q: %v", line, err)
		}
		events = append(events, event)
	}
	return events
}

func TestJSONWritesOneEventPerLine(t *testing.T) {
	runtime := newScript(t, nil, aToolTurn...)
	var stdout bytes.Buffer
	if err := Run(context.Background(), runtime, "hi", Output{Format: FormatJSON, Stdout: &stdout, CWD: "/work"}); err != nil {
		t.Fatal(err)
	}
	events := decodeLines(t, stdout.Bytes())
	var types []string
	for _, event := range events {
		types = append(types, event["type"].(string))
	}
	if got := strings.Join(types, ","); got != "session,assistant,tool_start,tool_done,usage,assistant,result" {
		t.Fatalf("event types = %s", got)
	}
	if events[0]["session_id"] != runtime.id.String() || events[0]["model"] != "fast" || events[0]["cwd"] != "/work" {
		t.Fatalf("session event = %v", events[0])
	}
	if events[1]["text"] != "Let me check." || events[1]["reasoning"] != "plan" {
		t.Fatalf("assistant event = %v", events[1])
	}
	start, done := events[2], events[3]
	if start["call_id"] != "call_1" || done["call_id"] != "call_1" || done["output"] != "a.go\n" {
		t.Fatalf("tool events = %v, %v", start, done)
	}
	if args, ok := start["arguments"].(map[string]any); !ok || args["command"] != "ls" {
		t.Fatalf("arguments were not passed through as JSON: %v", start["arguments"])
	}
	result := events[len(events)-1]
	if result["text"] != "There is one file.\n" || result["session_id"] != runtime.id.String() || result["error"] != nil {
		t.Fatalf("result = %v", result)
	}
}

func TestJSONReportsAPartialMessageAndTheError(t *testing.T) {
	runtime := newScript(t, context.Canceled, agent.Event{Kind: agent.EventText, Text: "half"})
	var stdout bytes.Buffer
	_ = Run(context.Background(), runtime, "hi", Output{Format: FormatJSON, Stdout: &stdout})
	events := decodeLines(t, stdout.Bytes())
	if len(events) != 3 || events[1]["partial"] != true || events[1]["text"] != "half" {
		t.Fatalf("events = %v", events)
	}
	if result := events[2]; result["error"] != context.Canceled.Error() || result["text"] != "half" {
		t.Fatalf("result = %v", result)
	}
}
