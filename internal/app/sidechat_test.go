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
	"path/filepath"
	"reflect"
	"strings"
	"testing"
	"time"

	"kon.kitsu.red/core/agent"
	"kon.kitsu.red/core/provider/wire"
	"kon.kitsu.red/core/session"
	"kon.kitsu.red/core/typedid"
	"kon.kitsu.red/internal/config"
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

// isSideChatRequest tells a side question from a main-run request by the
// instructions only the side question carries.
func isSideChatRequest(t *testing.T, req *http.Request) bool {
	t.Helper()
	body, err := io.ReadAll(req.Body)
	if err != nil {
		t.Error(err)
	}
	return bytes.Contains(body, []byte("Tools are unavailable in this side chat"))
}

func sideChatReply(w http.ResponseWriter) {
	w.Header().Set("Content-Type", "text/event-stream")
	fmt.Fprint(w, "data: {\"choices\":[{\"delta\":{\"content\":\"side answer\"},\"finish_reason\":\"stop\"}],\"usage\":{\"prompt_tokens\":50,\"completion_tokens\":3,\"total_tokens\":53}}\n\ndata: [DONE]\n\n")
}

func TestSideChatPreservesSessionAndRepairsIncompleteToolContext(t *testing.T) {
	var request struct {
		Model      string            `json:"model"`
		Messages   []map[string]any  `json:"messages"`
		Tools      []json.RawMessage `json:"tools"`
		ToolChoice *string           `json:"tool_choice"`
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
	// The main conversation's tools and tool_choice go along unchanged, so the
	// side question reads its prompt cache.
	if request.Model != "side-model" || len(request.Tools) == 0 || request.ToolChoice != nil {
		t.Fatalf("model = %q, tools = %v, tool_choice = %v", request.Model, request.Tools, request.ToolChoice)
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
		if !isSideChatRequest(t, req) {
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
			if err := r.NewSession(); err != nil {
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

func TestSideChatReportsThinkingWithoutItsText(t *testing.T) {
	r := sideChatRuntime(t, func(w http.ResponseWriter, _ *http.Request) {
		w.Header().Set("Content-Type", "text/event-stream")
		fmt.Fprint(w, "data: {\"choices\":[{\"delta\":{\"reasoning_content\":\"secret plan\"}}]}\n\n")
		sideChatReply(w)
	})
	var events []agent.Event
	if err := r.SideChat(t.Context(), "question", func(event agent.Event) { events = append(events, event) }); err != nil {
		t.Fatal(err)
	}
	if len(events) < 2 || events[0].Kind != agent.EventThinking || events[0].Text != "" || events[1].Kind != agent.EventText {
		t.Fatalf("events = %+v", events)
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

// A side question keeps the main tools for the prompt cache, so the model may
// call one; it is told tools are unavailable and answers on the next request,
// which extends the first.
func TestSideChatAnswersToolCallAsUnavailable(t *testing.T) {
	var bodies [][]byte
	r := sideChatRuntime(t, func(w http.ResponseWriter, req *http.Request) {
		body, _ := io.ReadAll(req.Body)
		bodies = append(bodies, body)
		if len(bodies) == 1 {
			w.Header().Set("Content-Type", "text/event-stream")
			fmt.Fprint(w, "data: {\"choices\":[{\"delta\":{\"tool_calls\":[{\"index\":0,\"id\":\"call\",\"type\":\"function\",\"function\":{\"name\":\"shell\",\"arguments\":\"{}\"}}]},\"finish_reason\":\"tool_calls\"}],\"usage\":{\"prompt_tokens\":50,\"completion_tokens\":3,\"total_tokens\":53}}\n\ndata: [DONE]\n\n")
			return
		}
		sideChatReply(w)
	})
	var text strings.Builder
	if err := r.SideChat(t.Context(), "side question", func(event agent.Event) { text.WriteString(event.Text) }); err != nil {
		t.Fatal(err)
	}
	if len(bodies) != 2 || text.String() != "side answer" {
		t.Fatalf("requests = %d, answer = %q", len(bodies), text.String())
	}
	var first, second struct {
		Messages []json.RawMessage `json:"messages"`
	}
	if json.Unmarshal(bodies[0], &first) != nil || json.Unmarshal(bodies[1], &second) != nil {
		t.Fatal("undecodable request")
	}
	if len(second.Messages) != len(first.Messages)+2 || !bytes.Contains(second.Messages[len(second.Messages)-1], []byte(sideChatToolUnavailable)) {
		t.Fatalf("retry did not answer the call: %s", bodies[1])
	}
	for i, message := range first.Messages {
		if !bytes.Equal(message, second.Messages[i]) {
			t.Fatalf("retry changed message %d", i)
		}
	}
}

func TestSideChatWireFormats(t *testing.T) {
	for _, format := range []wire.Format{wire.OpenAI, wire.OpenAICompatible, wire.OpenRouter, wire.Ollama, wire.OpenAIResponses, wire.Anthropic} {
		for _, toolHistory := range []bool{false, true} {
			t.Run(fmt.Sprintf("%s/tools=%v", format, toolHistory), func(t *testing.T) {
				var request struct {
					Tools      []json.RawMessage       `json:"tools"`
					ToolChoice json.RawMessage         `json:"tool_choice"`
					Messages   []struct{ Role string } `json:"messages"`
				}
				var body []byte
				server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, req *http.Request) {
					body, _ = io.ReadAll(req.Body)
					if err := json.Unmarshal(body, &request); err != nil {
						t.Error(err)
					}
					w.Header().Set("Content-Type", "text/event-stream")
					switch format {
					case wire.Anthropic:
						fmt.Fprint(w, `data: {"type":"content_block_start","index":0,"content_block":{"type":"text","text":""}}

data: {"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"side answer"}}

data: {"type":"message_delta","delta":{"stop_reason":"end_turn"}}

data: {"type":"message_stop"}

`)
					case wire.OpenAIResponses:
						fmt.Fprint(w, `data: {"type":"response.output_item.added","output_index":0,"item":{"type":"message","role":"assistant","content":[]}}

data: {"type":"response.output_text.delta","output_index":0,"delta":"side answer"}

data: {"type":"response.output_item.done","output_index":0,"item":{"type":"message","role":"assistant","content":[{"type":"output_text","text":"side answer"}]}}

data: {"type":"response.completed","response":{}}

`)
					default:
						sideChatReply(w)
					}
				}))
				defer server.Close()
				cfg := configured("side-model")
				cfg.Models[0].Type, cfg.Models[0].BaseURL = format, server.URL
				r, err := New(cfg, config.Paths{Sessions: t.TempDir()}, t.TempDir(), "test")
				if err != nil {
					t.Fatal(err)
				}
				defer r.Close()
				messages := []session.Message{session.TextMessage(session.RoleUser, "main prompt")}
				if toolHistory {
					messages = append(messages,
						session.Message{Role: session.RoleAssistant, Parts: []session.Part{{Type: session.PartToolCall, ToolCallID: typedid.ToolCallID("call-history"), ToolName: "shell", ToolInput: json.RawMessage(`{"command":"echo hello"}`)}}},
						session.ToolResultMessage(typedid.ToolCallID("call-history"), "shell", "historical tool result"))
				}
				for _, message := range messages {
					if _, err := r.store.AppendMessage(message); err != nil {
						t.Fatal(err)
					}
				}
				var text strings.Builder
				if err := r.SideChat(t.Context(), "side question", func(event agent.Event) { text.WriteString(event.Text) }); err != nil {
					t.Fatal(err)
				}
				if text.String() != "side answer" {
					t.Fatalf("answer = %q", text.String())
				}
				// tool_choice stays as the main turn sends it, since changing it
				// would invalidate the cached conversation.
				if len(request.Tools) != 4 || request.ToolChoice != nil {
					t.Fatalf("tools = %d, tool_choice = %s", len(request.Tools), request.ToolChoice)
				}
				for _, want := range []string{"main prompt", "side question"} {
					if !bytes.Contains(body, []byte(want)) {
						t.Fatalf("request missing %q: %s", want, body)
					}
				}
				if toolHistory && (!bytes.Contains(body, []byte("call-history")) || !bytes.Contains(body, []byte("historical tool result"))) {
					t.Fatalf("tool history lost: %s", body)
				}
				if format == wire.Anthropic {
					for i := 1; i < len(request.Messages); i++ {
						if request.Messages[i].Role == request.Messages[i-1].Role {
							t.Fatalf("adjacent same-role turns: %s", body)
						}
					}
				}
			})
		}
	}
}

func TestSideChatCancelledCleanupDoesNotBlockMutations(t *testing.T) {
	started, release := make(chan struct{}), make(chan struct{})
	r := sideChatRuntime(t, func(w http.ResponseWriter, req *http.Request) { sideChatReply(w) })
	if _, err := r.store.AppendMessage(session.TextMessage(session.RoleUser, "saved session")); err != nil {
		t.Fatal(err)
	}
	saved := r.SessionID()
	r.paths.ConfigFile = filepath.Join(t.TempDir(), "config.json")
	profile := r.config.Models[0]
	profile.Name = "other"
	r.config.Models = append(r.config.Models, profile)
	ctx, cancel := context.WithCancel(t.Context())
	defer cancel()
	done := make(chan error, 1)
	go func() {
		done <- r.SideChat(ctx, "question", func(event agent.Event) {
			if event.Kind == agent.EventText {
				close(started)
				<-release
			}
		})
	}()
	defer func() { close(release); <-done }()
	awaitSideChatSignal(t, started)
	cancel()
	if err := r.NewSession(); err != nil {
		t.Fatalf("new during cancelled cleanup: %v", err)
	}
	if err := r.Resume(saved); err != nil {
		t.Fatalf("resume during cancelled cleanup: %v", err)
	}
	if err := r.SwitchModel("other"); err != nil {
		t.Fatalf("model switch during cancelled cleanup: %v", err)
	}
}

func TestSideChatSnapshotRetainsIncognitoImagesAfterClose(t *testing.T) {
	var body []byte
	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, req *http.Request) {
		body, _ = io.ReadAll(req.Body)
		sideChatReply(w)
	}))
	defer server.Close()
	cfg := configured("side-model")
	cfg.Models[0].BaseURL, cfg.Models[0].Inputs = server.URL, []session.Modality{session.ModalityImage}
	r, err := Start(cfg, config.Paths{Sessions: t.TempDir()}, t.TempDir(), "test", Options{Incognito: true})
	if err != nil {
		t.Fatal(err)
	}
	defer r.Close()
	part, err := r.store.SaveMedia([]byte("test image"), "image/png")
	if err != nil {
		t.Fatal(err)
	}
	if _, err := r.store.AppendMessage(session.Message{Role: session.RoleUser, Parts: []session.Part{{Type: session.PartText, Text: "image question"}, part}}); err != nil {
		t.Fatal(err)
	}
	r.mu.Lock()
	client, messages, definitions, err := r.prepareSideChat("describe the image")
	r.mu.Unlock()
	if err != nil {
		t.Fatal(err)
	}
	if err := r.Close(); err != nil {
		t.Fatal(err)
	}
	if _, err := client.Stream(t.Context(), messages, definitions, nil); err != nil {
		t.Fatal(err)
	}
	if !bytes.Contains(body, []byte("data:image/png;base64,dGVzdCBpbWFnZQ==")) {
		t.Fatalf("snapshot lost image: %s", body)
	}
}

func TestSideChatSnapshotsWhileMainRunWrites(t *testing.T) {
	started, release := make(chan struct{}), make(chan struct{})
	mainRequests := 0
	r := sideChatRuntime(t, func(w http.ResponseWriter, req *http.Request) {
		if !isSideChatRequest(t, req) {
			mainRequests++
			if mainRequests == 1 {
				close(started)
				<-release
			}
		}
		sideChatReply(w)
	})
	done := make(chan error, 1)
	go func() {
		for i := 0; i < 20; i++ {
			if err := r.Run(t.Context(), "main task", nil, func(agent.Event) {}); err != nil {
				done <- err
				return
			}
		}
		done <- nil
	}()
	awaitSideChatSignal(t, started)
	close(release)
	defer func() {
		if err := <-done; err != nil {
			t.Error(err)
		}
	}()
	for i := 0; i < 20; i++ {
		if err := r.SideChat(t.Context(), "side question", nil); err != nil {
			t.Fatal(err)
		}
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
