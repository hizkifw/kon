package provider

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"net/http/httptest"
	"slices"
	"strings"
	"testing"

	"github.com/hizkifw/kon/internal/provider/wire"
	"github.com/hizkifw/kon/internal/session"
	"github.com/hizkifw/kon/internal/typedid"
)

// newMessagesTestModel points a messagesModel at a stub server.
func newMessagesTestModel(t *testing.T, handler http.HandlerFunc) *messagesModel {
	t.Helper()
	server := httptest.NewServer(handler)
	t.Cleanup(server.Close)
	spec, _ := wire.Lookup(wire.Anthropic)
	return &messagesModel{client: server.Client(), baseURL: server.URL, apiKey: "sk-test", model: "claude-test", spec: spec, reasoning: true}
}

// messagesStream is a complete stream: thinking with a signature, text, then
// a tool call whose input arrives in pieces.
var messagesStream = strings.Join([]string{
	sse(`{"type":"message_start","message":{"usage":{"input_tokens":10,"cache_creation_input_tokens":20,"cache_read_input_tokens":300,"output_tokens":1}}}`),
	sse(`{"type":"content_block_start","index":0,"content_block":{"type":"thinking","thinking":"","signature":""}}`),
	sse(`{"type":"content_block_delta","index":0,"delta":{"type":"thinking_delta","thinking":"plan "}}`),
	sse(`{"type":"content_block_delta","index":0,"delta":{"type":"thinking_delta","thinking":"it"}}`),
	sse(`{"type":"content_block_delta","index":0,"delta":{"type":"signature_delta","signature":"sig=="}}`),
	sse(`{"type":"content_block_stop","index":0}`),
	"event: ping\n" + sse(`{"type":"ping"}`),
	sse(`{"type":"content_block_start","index":1,"content_block":{"type":"text","text":""}}`),
	sse(`{"type":"content_block_delta","index":1,"delta":{"type":"text_delta","text":"Hello"}}`),
	sse(`{"type":"content_block_stop","index":1}`),
	sse(`{"type":"content_block_start","index":2,"content_block":{"type":"tool_use","id":"toolu_1","name":"read","input":{}}}`),
	sse(`{"type":"content_block_delta","index":2,"delta":{"type":"input_json_delta","partial_json":"{\"path\":"}}`),
	sse(`{"type":"content_block_delta","index":2,"delta":{"type":"input_json_delta","partial_json":"\"a.go\"}"}}`),
	sse(`{"type":"content_block_stop","index":2}`),
	sse(`{"type":"message_delta","delta":{"stop_reason":"tool_use"},"usage":{"output_tokens":42}}`),
	sse(`{"type":"message_stop"}`),
}, "")

func TestMessagesStreamAssemblesBlocks(t *testing.T) {
	var headers http.Header
	model := newMessagesTestModel(t, func(w http.ResponseWriter, r *http.Request) {
		headers = r.Header
		if r.URL.Path != "/messages" {
			t.Errorf("path = %s", r.URL.Path)
		}
		_, _ = io.WriteString(w, messagesStream)
	})
	var events []Event
	response, err := model.Stream(context.Background(), []session.Message{session.TextMessage(session.RoleUser, "hi")}, nil, func(e Event) { events = append(events, e) })
	if err != nil {
		t.Fatal(err)
	}
	if headers.Get("x-api-key") != "sk-test" || headers.Get("anthropic-version") == "" || headers.Get("Authorization") != "" {
		t.Fatalf("auth headers = %v", headers)
	}
	if len(response.Parts) != 3 {
		t.Fatalf("parts = %#v", response.Parts)
	}
	thinking, text, call := response.Parts[0], response.Parts[1], response.Parts[2]
	if thinking.Type != session.PartReasoning || thinking.Text != "plan it" || decodeThinkingOptions(thinking.ProviderOptions).Signature != "sig==" {
		t.Fatalf("thinking = %#v", thinking)
	}
	if text.Type != session.PartText || text.Text != "Hello" {
		t.Fatalf("text = %#v", text)
	}
	if call.ToolCallID.String() != "toolu_1" || call.ToolName != "read" || string(call.ToolInput) != `{"path":"a.go"}` {
		t.Fatalf("call = %#v", call)
	}
	if response.Finish != "tool_calls" {
		t.Fatalf("finish = %q", response.Finish)
	}
	// input_tokens is only the uncached remainder; the prompt is all three.
	if u := response.Usage; u == nil || u.PromptTokens != 330 || u.CachedTokens != 300 || u.CacheWriteTokens != 20 || u.CompletionTokens != 42 {
		t.Fatalf("usage = %#v", response.Usage)
	}
	if len(events) != 3 || !events[0].Thinking || events[2].Text != "Hello" {
		t.Fatalf("events = %#v", events)
	}
}

func TestMessagesRequestShape(t *testing.T) {
	signed, _ := json.Marshal(thinkingOptions{Signature: "sig=="})
	conversation := []session.Message{
		session.TextMessage(session.RoleSystem, "system prompt"),
		session.TextMessage(session.RoleUser, "task"),
		{Role: session.RoleAssistant, Parts: []session.Part{
			{Type: session.PartReasoning, Text: "", ProviderOptions: signed},
			{Type: session.PartReasoning, Text: "unsigned, from another provider"},
			{Type: session.PartText, Text: "reading both"},
			{Type: session.PartToolCall, ToolCallID: typedid.ExternalToolCallID("a"), ToolName: "read", ToolInput: json.RawMessage(`{"path":"a"}`)},
			{Type: session.PartToolCall, ToolCallID: typedid.ExternalToolCallID("b"), ToolName: "read", ToolInput: json.RawMessage(`{"path":"b"}`)},
		}},
		session.ToolResultMessage(typedid.ExternalToolCallID("a"), "read", "A"),
		session.ToolResultMessage(typedid.ExternalToolCallID("b"), "read", "B"),
		session.TextMessage(session.RoleUser, "steer: use v2"),
		// An interrupted turn holding only reasoning is dropped whole.
		{Role: session.RoleAssistant, Parts: []session.Part{{Type: session.PartReasoning, Text: "half a thought"}}},
		session.TextMessage(session.RoleUser, "next"),
	}
	var body []byte
	model := newMessagesTestModel(t, func(w http.ResponseWriter, r *http.Request) {
		body, _ = io.ReadAll(r.Body)
		_, _ = io.WriteString(w, sse(`{"type":"content_block_start","index":0,"content_block":{"type":"text","text":"ok"}}`)+sse(`{"type":"message_delta","delta":{"stop_reason":"end_turn"}}`)+sse(`{"type":"message_stop"}`))
	})
	model.effort = "xhigh"
	tools := []session.ToolDefinition{{Name: "read", Description: "read a file", Parameters: json.RawMessage(`{"type":"object"}`)}}
	if _, err := model.Stream(context.Background(), conversation, tools, nil); err != nil {
		t.Fatal(err)
	}
	// The signed thinking goes back with its empty text and the unsigned one
	// is left out. Both results, the steer, and the next prompt share one user
	// turn, results first, and the conversation's last block carries the
	// moving breakpoint. A streamed turn asks for the default output budget,
	// whatever it is tuned to.
	assertWireJSON(t, body, fmt.Sprintf(`{
		"model": "claude-test",
		"max_tokens": %d,
		"system": [{"type": "text", "text": "system prompt", "cache_control": {"type": "ephemeral"}}],
		"messages": [
			{"role": "user", "content": [{"type": "text", "text": "task"}]},
			{"role": "assistant", "content": [
				{"type": "thinking", "thinking": "", "signature": "sig=="},
				{"type": "text", "text": "reading both"},
				{"type": "tool_use", "id": "a", "name": "read", "input": {"path": "a"}},
				{"type": "tool_use", "id": "b", "name": "read", "input": {"path": "b"}}
			]},
			{"role": "user", "content": [
				{"type": "tool_result", "tool_use_id": "a", "content": [{"type": "text", "text": "A"}]},
				{"type": "tool_result", "tool_use_id": "b", "content": [{"type": "text", "text": "B"}]},
				{"type": "text", "text": "steer: use v2"},
				{"type": "text", "text": "next", "cache_control": {"type": "ephemeral"}}
			]}
		],
		"tools": [{"name": "read", "description": "read a file", "input_schema": {"type": "object"}}],
		"thinking": {"type": "adaptive", "display": "summarized"},
		"output_config": {"effort": "xhigh"},
		"stream": true
	}`, defaultMessagesMaxTokens))
}

func TestMessagesLearnsFromRejections(t *testing.T) {
	var bodies [][]byte
	requests := 0
	model := newMessagesTestModel(t, func(w http.ResponseWriter, r *http.Request) {
		body, _ := io.ReadAll(r.Body)
		bodies = append(bodies, body)
		requests++
		fail := func(message string) {
			w.WriteHeader(http.StatusBadRequest)
			_, _ = io.WriteString(w, `{"type":"error","error":{"type":"invalid_request_error","message":"`+message+`"}}`)
		}
		switch requests {
		case 1:
			fail("max_tokens: 32000 > 8192, which is the maximum allowed number of output tokens")
		case 2:
			fail("thinking.type: Input tag 'adaptive' does not match the expected tags")
		default:
			_, _ = io.WriteString(w, sse(`{"type":"content_block_start","index":0,"content_block":{"type":"text","text":"ok"}}`)+sse(`{"type":"message_stop"}`))
		}
	})
	if _, err := model.Stream(context.Background(), []session.Message{session.TextMessage(session.RoleUser, "hi")}, nil, nil); err != nil {
		t.Fatal(err)
	}
	if len(bodies) != 3 {
		t.Fatalf("requests = %d", len(bodies))
	}
	// The last request fits the reported output limit and budgets thinking
	// within it, since the model predates adaptive thinking.
	assertWireJSON(t, bodies[2], `{
		"model": "claude-test",
		"max_tokens": 8192,
		"messages": [{"role": "user", "content": [{"type": "text", "text": "hi", "cache_control": {"type": "ephemeral"}}]}],
		"thinking": {"type": "enabled", "budget_tokens": 4096},
		"stream": true
	}`)
	// What was learned sticks: the next request is right the first time.
	bodies = nil
	if _, err := model.Stream(context.Background(), []session.Message{session.TextMessage(session.RoleUser, "again")}, nil, nil); err != nil || len(bodies) != 1 {
		t.Fatalf("second request: %v after %d tries", err, len(bodies))
	}
}

// A model configured without reasoning still thinks by default and returns
// bound blocks, so the drop setting goes out with an explicit adaptive
// thinking config, and with the beta that allows it.
func TestMessagesDropsMismatchedThinking(t *testing.T) {
	var bodies [][]byte
	var betas []string
	model := newMessagesTestModel(t, func(w http.ResponseWriter, r *http.Request) {
		body, _ := io.ReadAll(r.Body)
		bodies = append(bodies, body)
		betas = append(betas, r.Header.Get("anthropic-beta"))
		if len(bodies) == 1 {
			w.WriteHeader(http.StatusBadRequest)
			_, _ = io.WriteString(w, `{"type":"error","error":{"type":"invalid_request_error","message":"messages.1.content.0: Invalid `+"`signature`"+` in `+"`thinking`"+` block. The block is bound to a different conversation."}}`)
			return
		}
		_, _ = io.WriteString(w, sse(`{"type":"content_block_start","index":0,"content_block":{"type":"text","text":"ok"}}`)+sse(`{"type":"message_stop"}`))
	})
	model.reasoning = false
	hi := []session.Message{session.TextMessage(session.RoleUser, "hi")}
	if _, err := model.Stream(context.Background(), hi, nil, nil); err != nil {
		t.Fatal(err)
	}
	if len(bodies) != 2 || betas[0] != "" || betas[1] != thinkingBindingBeta {
		t.Fatalf("requests = %d, betas = %q", len(bodies), betas)
	}
	assertWireJSON(t, bodies[1], fmt.Sprintf(`{
		"model": "claude-test",
		"max_tokens": %d,
		"messages": [{"role": "user", "content": [{"type": "text", "text": "hi", "cache_control": {"type": "ephemeral"}}]}],
		"thinking": {"type": "adaptive", "block_binding": {"prefix_mismatch_behavior": "drop_block"}},
		"stream": true
	}`, defaultMessagesMaxTokens))
	// The server drops blocks for one request at a time, so every later
	// request asks again.
	if _, err := model.Stream(context.Background(), hi, nil, nil); err != nil || len(bodies) != 3 || betas[2] != thinkingBindingBeta || !strings.Contains(string(bodies[2]), "drop_block") {
		t.Fatalf("second request: %v, betas = %q", err, betas)
	}
}

// A profile's own betas are kept, with the binding beta added once.
func TestMessagesJoinsConfiguredBetas(t *testing.T) {
	var betas []string
	model := newMessagesTestModel(t, func(w http.ResponseWriter, r *http.Request) {
		betas = append(betas, r.Header.Get("anthropic-beta"))
		if len(betas) == 1 {
			w.WriteHeader(http.StatusBadRequest)
			_, _ = io.WriteString(w, `{"type":"error","error":{"type":"invalid_request_error","message":"The block is bound to a different conversation."}}`)
			return
		}
		_, _ = io.WriteString(w, sse(`{"type":"content_block_start","index":0,"content_block":{"type":"text","text":"ok"}}`)+sse(`{"type":"message_stop"}`))
	})
	model.headers = map[string]string{"anthropic-beta": "context-1m-2025-08-07"}
	if _, err := model.Stream(context.Background(), []session.Message{session.TextMessage(session.RoleUser, "hi")}, nil, nil); err != nil {
		t.Fatal(err)
	}
	if want := []string{"context-1m-2025-08-07", "context-1m-2025-08-07," + thinkingBindingBeta}; !slices.Equal(betas, want) {
		t.Fatalf("betas = %q, want %q", betas, want)
	}
	if got := withBeta("a, "+thinkingBindingBeta, thinkingBindingBeta); got != "a, "+thinkingBindingBeta {
		t.Fatalf("withBeta repeated the beta: %q", got)
	}
}

// Only a rejection of the thinking type teaches that a model needs budgeted
// thinking; an error that merely mentions adaptive thinking does not.
func TestMessagesAdaptiveThinkingError(t *testing.T) {
	for body, want := range map[string]bool{
		"thinking.type: Input tag 'adaptive' does not match the expected tags":                      true,
		"adaptive thinking is not supported on this model":                                          true,
		"claude-haiku-4-5 does not support adaptive thinking":                                       true,
		"output_config.effort: 'xhigh' requires adaptive thinking":                                  false,
		"thinking.display: 'summarized' is not supported with adaptive":                             false,
		"thinking.block_binding requires thinking.type adaptive":                                    false,
		"thinking.type.enabled is not supported for this model. Use thinking.type.adaptive instead": false,
	} {
		if got := isAdaptiveThinkingError(body); got != want {
			t.Errorf("isAdaptiveThinkingError(%q) = %v, want %v", body, got, want)
		}
	}
}

func TestMessagesErrors(t *testing.T) {
	for name, test := range map[string]struct {
		status int
		body   string
		check  func(error) bool
	}{
		"overflow":     {400, `{"type":"error","error":{"type":"invalid_request_error","message":"prompt is too long: 250000 tokens > 200000 maximum"}}`, IsContextOverflow},
		"stream error": {200, sse(`{"type":"message_start","message":{}}`) + "event: error\n" + sse(`{"type":"error","error":{"type":"overloaded_error","message":"Overloaded"}}`), func(err error) bool { return strings.Contains(err.Error(), "Overloaded") }},
		"cut short":    {200, sse(`{"type":"content_block_start","index":0,"content_block":{"type":"text","text":"par"}}`), func(err error) bool { return strings.Contains(err.Error(), "closed before") }},
	} {
		t.Run(name, func(t *testing.T) {
			model := newMessagesTestModel(t, func(w http.ResponseWriter, r *http.Request) {
				w.WriteHeader(test.status)
				_, _ = io.WriteString(w, test.body)
			})
			response, err := model.Stream(context.Background(), []session.Message{session.TextMessage(session.RoleUser, "hi")}, nil, nil)
			if err == nil || !test.check(err) {
				t.Fatalf("err = %v", err)
			}
			if name == "cut short" && response.Text() != "par" {
				t.Fatalf("partial text = %q", response.Text())
			}
		})
	}
}

// Complete sends the streaming turn's tools and no tool_choice: changing
// tool_choice would invalidate the cached conversation it is meant to reuse.
func TestMessagesCompleteKeepsCachedPrefix(t *testing.T) {
	var body map[string]json.RawMessage
	model := newMessagesTestModel(t, func(w http.ResponseWriter, r *http.Request) {
		_ = json.NewDecoder(r.Body).Decode(&body)
		_, _ = io.WriteString(w, sse(`{"type":"content_block_start","index":0,"content_block":{"type":"text","text":"summary"}}`)+sse(`{"type":"message_delta","delta":{"stop_reason":"end_turn"}}`)+sse(`{"type":"message_stop"}`))
	})
	tools := []session.ToolDefinition{{Name: "read", Parameters: json.RawMessage(`{"type":"object"}`)}}
	response, err := model.Complete(context.Background(), []session.Message{session.TextMessage(session.RoleUser, "summarize")}, tools, 4096, nil)
	if err != nil || response.Text() != "summary" {
		t.Fatalf("Complete = %q, %v", response.Text(), err)
	}
	if _, ok := body["tool_choice"]; ok || string(body["max_tokens"]) != "4096" || body["tools"] == nil {
		t.Fatalf("request = %s", body)
	}
}
