package app

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/http"
	"net/http/httptest"
	"os"
	"reflect"
	"strings"
	"testing"
	"time"

	"github.com/hizkifw/kon/internal/agent"
	"github.com/hizkifw/kon/internal/config"
	"github.com/hizkifw/kon/internal/session"
	"github.com/hizkifw/kon/internal/typedid"
)

func sideChatRuntime(t *testing.T, handler http.HandlerFunc) *Runtime {
	t.Helper()
	server := httptest.NewServer(handler)
	t.Cleanup(server.Close)
	cfg := configured("side-model")
	cfg.Models[0].BaseURL = server.URL
	r, err := New(cfg, config.Paths{Sessions: t.TempDir()}, t.TempDir(), "test")
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = r.Close() })
	return r
}

func sideChatReply(w http.ResponseWriter) {
	w.Header().Set("Content-Type", "text/event-stream")
	fmt.Fprint(w, "data: {\"choices\":[{\"delta\":{\"content\":\"side answer\"},\"finish_reason\":\"stop\"}],\"usage\":{\"prompt_tokens\":50,\"completion_tokens\":3,\"total_tokens\":53}}\n\ndata: [DONE]\n\n")
}

func TestSideChatPreservesSessionAndRepairsIncompleteToolContext(t *testing.T) {
	var request struct {
		Model    string            `json:"model"`
		Messages []map[string]any  `json:"messages"`
		Tools    []json.RawMessage `json:"tools"`
	}
	r := sideChatRuntime(t, func(w http.ResponseWriter, req *http.Request) {
		if err := json.NewDecoder(req.Body).Decode(&request); err != nil {
			t.Error(err)
		}
		sideChatReply(w)
	})
	for _, message := range []session.Message{
		session.TextMessage(session.RoleUser, "main task"),
		{Role: session.RoleAssistant, Parts: []session.Part{{Type: session.PartToolCall,
			ToolCallID: typedid.ToolCallID("opaque-call"), ToolName: "shell", ToolInput: json.RawMessage(`{"command":"echo hello"}`),
		}}},
	} {
		if _, err := r.store.AppendMessage(message); err != nil {
			t.Fatal(err)
		}
	}
	before, err := os.ReadFile(r.store.Path())
	if err != nil {
		t.Fatal(err)
	}
	state, runner := r.State(), r.runner
	usage, known := r.ContextUsage()
	var events []agent.Event
	if err := r.SideChat(t.Context(), "what is happening?", func(event agent.Event) { events = append(events, event) }); err != nil {
		t.Fatal(err)
	}
	after, err := os.ReadFile(r.store.Path())
	if err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(before, after) {
		t.Fatal("side chat changed the durable session")
	}
	if !reflect.DeepEqual(state, r.State()) || runner != r.runner {
		t.Fatal("side chat changed the main runtime state or runner")
	}
	if got, ok := r.ContextUsage(); got != usage || ok != known {
		t.Fatalf("context usage changed from (%d, %v) to (%d, %v)", usage, known, got, ok)
	}
	if request.Model != "side-model" || len(request.Tools) != 0 {
		t.Fatalf("model = %q, tools = %v", request.Model, request.Tools)
	}
	if len(request.Messages) != 5 || request.Messages[0]["content"] != r.store.ActivePath()[0].Message.Text() {
		t.Fatalf("unexpected context: %v", request.Messages)
	}
	if result := request.Messages[3]; result["role"] != "tool" || result["tool_call_id"] != "opaque-call" {
		t.Fatalf("missing projected tool result: %v", result)
	}
	question := request.Messages[4]["content"].(string)
	for _, instruction := range []string{"what is happening?", "Tools are unavailable", "suggest asking in the main conversation", "Do not invent results"} {
		if !strings.Contains(question, instruction) {
			t.Fatalf("side request missing %q: %s", instruction, question)
		}
	}
	if len(events) != 2 || events[0].Kind != agent.EventText || events[0].Text != "side answer" || events[1].Kind != agent.EventUsage || events[1].Tokens != -1 {
		t.Fatalf("events = %#v", events)
	}
}

func TestSideChatRunsAlongsideMainRun(t *testing.T) {
	mainStarted := make(chan struct{})
	r := sideChatRuntime(t, func(w http.ResponseWriter, req *http.Request) {
		var request struct {
			Tools []json.RawMessage `json:"tools"`
		}
		if err := json.NewDecoder(req.Body).Decode(&request); err != nil {
			t.Error(err)
		}
		if len(request.Tools) != 0 {
			close(mainStarted)
			<-req.Context().Done()
			return
		}
		sideChatReply(w)
	})
	ctx, cancel := context.WithCancel(t.Context())
	defer cancel()
	mainDone := make(chan error, 1)
	go func() { mainDone <- r.Run(ctx, "main task", nil, func(agent.Event) {}) }()
	awaitSideChatSignal(t, mainStarted)
	if err := r.SideChat(t.Context(), "side question", nil); err != nil {
		t.Fatal(err)
	}
	if r.State().Phase != PhaseRunning {
		t.Fatal("side chat changed the main run phase")
	}
	select {
	case err := <-mainDone:
		t.Fatalf("main run stopped after side chat: %v", err)
	default:
	}
	cancel()
	if err := <-mainDone; !errors.Is(err, context.Canceled) {
		t.Fatalf("main run error = %v", err)
	}
}

func TestSideChatCancellationAndLifecycle(t *testing.T) {
	for _, closeRuntime := range []bool{false, true} {
		t.Run(fmt.Sprintf("close=%v", closeRuntime), func(t *testing.T) {
			started := make(chan struct{})
			r := sideChatRuntime(t, func(w http.ResponseWriter, req *http.Request) {
				_, _ = io.Copy(io.Discard, req.Body)
				close(started)
				<-req.Context().Done()
			})
			ctx, cancel := context.WithCancel(t.Context())
			defer cancel()
			done := make(chan error, 1)
			go func() { done <- r.SideChat(ctx, "question", nil) }()
			awaitSideChatSignal(t, started)
			if err := r.SideChat(t.Context(), "another question", nil); !errors.Is(err, ErrBusy) {
				t.Fatalf("duplicate side chat: %v", err)
			}
			if err := r.NewSession(); !errors.Is(err, ErrBusy) {
				t.Fatalf("session switch during side chat: %v", err)
			}
			if closeRuntime {
				if err := r.Close(); err != nil {
					t.Fatal(err)
				}
			} else {
				cancel()
			}
			if err := <-done; !errors.Is(err, context.Canceled) {
				t.Fatalf("cancelled side chat error = %v", err)
			}
			if r.sideDone != nil || r.sideCancel != nil {
				t.Fatal("side chat lifecycle did not settle")
			}
			if closeRuntime {
				if err := r.SideChat(t.Context(), "question", nil); !errors.Is(err, ErrClosed) {
					t.Fatalf("closed side chat: %v", err)
				}
			} else if err := r.NewSession(); err != nil {
				t.Fatalf("session switch after cancellation: %v", err)
			}
		})
	}
}

func TestSideChatRefusesUnavailableRuntime(t *testing.T) {
	for phase, want := range map[Phase]error{PhaseClosed: ErrClosed, PhaseFollowing: ErrReadOnly, PhaseNeedsConfiguration: ErrNotReady, PhaseReady: ErrNotReady} {
		t.Run(string(phase), func(t *testing.T) {
			r := &Runtime{phase: phase}
			if err := r.SideChat(t.Context(), "question", nil); !errors.Is(err, want) {
				t.Fatalf("SideChat = %v, want %v", err, want)
			}
		})
	}
}

func TestSideChatProviderFailureLeavesMainUsable(t *testing.T) {
	for _, toolCall := range []bool{false, true} {
		t.Run(fmt.Sprintf("tool-call=%v", toolCall), func(t *testing.T) {
			r := sideChatRuntime(t, func(w http.ResponseWriter, req *http.Request) {
				if !toolCall {
					http.Error(w, "request rejected", http.StatusBadRequest)
					return
				}
				w.Header().Set("Content-Type", "text/event-stream")
				fmt.Fprint(w, "data: {\"choices\":[{\"delta\":{\"tool_calls\":[{\"index\":0,\"id\":\"call\",\"type\":\"function\",\"function\":{\"name\":\"shell\",\"arguments\":\"{}\"}}]},\"finish_reason\":\"tool_calls\"}]}\n\ndata: [DONE]\n\n")
			})
			before := r.store.ActivePath()
			err := r.SideChat(t.Context(), "question", nil)
			if err == nil {
				t.Fatal("side chat accepted a failed or tool-only response")
			}
			if toolCall && !errors.Is(err, ErrSideChatTools) {
				t.Fatalf("tool call did not report its limitation: %v", err)
			}
			if !reflect.DeepEqual(before, r.store.ActivePath()) || !r.State().Ready() {
				t.Fatal("failed side chat changed the session or main state")
			}
			if err := r.NewSession(); err != nil {
				t.Fatalf("session switch after failed side chat: %v", err)
			}
		})
	}
}

func awaitSideChatSignal(t *testing.T, signal <-chan struct{}) {
	t.Helper()
	select {
	case <-signal:
	case <-time.After(5 * time.Second):
		t.Fatal("timed out waiting for provider request")
	}
}
