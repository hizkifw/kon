package provider

import (
	"context"
	"encoding/json"
	"io"
	"net/http"
	"net/http/httptest"
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
	if u := response.Usage; u == nil || u.PromptTokens != 330 || u.CachedTokens != 300 || u.CompletionTokens != 42 {
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
	var body messagesRequest
	model := newMessagesTestModel(t, func(w http.ResponseWriter, r *http.Request) {
		if err := json.NewDecoder(r.Body).Decode(&body); err != nil {
			t.Error(err)
		}
		_, _ = io.WriteString(w, sse(`{"type":"content_block_start","index":0,"content_block":{"type":"text","text":"ok"}}`)+sse(`{"type":"message_delta","delta":{"stop_reason":"end_turn"}}`)+sse(`{"type":"message_stop"}`))
	})
	model.effort = "xhigh"
	tools := []session.ToolDefinition{{Name: "read", Description: "read a file", Parameters: json.RawMessage(`{"type":"object"}`)}}
	if _, err := model.Stream(context.Background(), conversation, tools, nil); err != nil {
		t.Fatal(err)
	}
	if len(body.System) != 1 || body.System[0].Text != "system prompt" || body.System[0].CacheControl == nil {
		t.Fatalf("system = %#v", body.System)
	}
	if body.Thinking == nil || body.Thinking.Type != "adaptive" || body.OutputConfig == nil || body.OutputConfig.Effort != "xhigh" || body.MaxTokens != defaultMessagesMaxTokens {
		t.Fatalf("thinking = %#v, output = %#v, max = %d", body.Thinking, body.OutputConfig, body.MaxTokens)
	}
	if len(body.Tools) != 1 || string(body.Tools[0].InputSchema) != `{"type":"object"}` {
		t.Fatalf("tools = %#v", body.Tools)
	}
	roles := []string{}
	for _, m := range body.Messages {
		roles = append(roles, m.Role)
	}
	if strings.Join(roles, ",") != "user,assistant,user" {
		t.Fatalf("roles = %v", roles)
	}
	assistant := body.Messages[1].Content
	if len(assistant) != 4 || assistant[0].Type != "thinking" || assistant[0].Thinking == nil || *assistant[0].Thinking != "" || assistant[0].Signature != "sig==" {
		t.Fatalf("assistant = %#v", assistant)
	}
	// Both results, the steer, and the next prompt share one user turn, results
	// first, and the conversation's last block carries the moving breakpoint.
	turn := body.Messages[2].Content
	kinds := []string{}
	for _, block := range turn {
		kinds = append(kinds, block.Type)
	}
	if strings.Join(kinds, ",") != "tool_result,tool_result,text,text" || turn[0].ToolUseID != "a" || turn[0].Content[0].Text != "A" {
		t.Fatalf("user turn = %#v", turn)
	}
	if turn[3].CacheControl == nil || turn[2].CacheControl != nil {
		t.Fatal("the cache breakpoint is not on the last block alone")
	}
}

func TestMessagesLearnsFromRejections(t *testing.T) {
	var bodies []messagesRequest
	var betas []string
	requests := 0
	model := newMessagesTestModel(t, func(w http.ResponseWriter, r *http.Request) {
		var body messagesRequest
		_ = json.NewDecoder(r.Body).Decode(&body)
		bodies = append(bodies, body)
		requests++
		betas = append(betas, r.Header.Get("anthropic-beta"))
		fail := func(message string) {
			w.WriteHeader(http.StatusBadRequest)
			_, _ = io.WriteString(w, `{"type":"error","error":{"type":"invalid_request_error","message":"`+message+`"}}`)
		}
		switch requests {
		case 1:
			fail("max_tokens: 32000 > 8192, which is the maximum allowed number of output tokens")
		case 2:
			fail("thinking.type: Input tag 'adaptive' does not match the expected tags")
		case 3:
			fail("messages.1.content.0: Invalid `signature` in `thinking` block. The block is bound to a different conversation.")
		default:
			_, _ = io.WriteString(w, sse(`{"type":"content_block_start","index":0,"content_block":{"type":"text","text":"ok"}}`)+sse(`{"type":"message_stop"}`))
		}
	})
	if _, err := model.Stream(context.Background(), []session.Message{session.TextMessage(session.RoleUser, "hi")}, nil, nil); err != nil {
		t.Fatal(err)
	}
	last := bodies[len(bodies)-1]
	if len(bodies) != 4 || last.MaxTokens != 8192 || last.Thinking.Type != "enabled" || last.Thinking.BudgetTokens != 4096 || betas[3] != thinkingBindingBeta {
		t.Fatalf("requests = %d, last = %#v, betas = %q", len(bodies), last, betas)
	}
	// What was learned sticks: the next request is right the first time.
	bodies = nil
	if _, err := model.Stream(context.Background(), []session.Message{session.TextMessage(session.RoleUser, "again")}, nil, nil); err != nil || len(bodies) != 1 {
		t.Fatalf("second request: %v after %d tries", err, len(bodies))
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
		"refusal":      {200, sse(`{"type":"message_delta","delta":{"stop_reason":"refusal"}}`) + sse(`{"type":"message_stop"}`), func(err error) bool { return strings.Contains(err.Error(), "refusal") }},
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

func TestMessagesCompleteForbidsToolCalls(t *testing.T) {
	var body messagesRequest
	model := newMessagesTestModel(t, func(w http.ResponseWriter, r *http.Request) {
		_ = json.NewDecoder(r.Body).Decode(&body)
		_, _ = io.WriteString(w, sse(`{"type":"content_block_start","index":0,"content_block":{"type":"text","text":"summary"}}`)+sse(`{"type":"message_delta","delta":{"stop_reason":"end_turn"}}`)+sse(`{"type":"message_stop"}`))
	})
	tools := []session.ToolDefinition{{Name: "read", Parameters: json.RawMessage(`{"type":"object"}`)}}
	response, err := model.Complete(context.Background(), []session.Message{session.TextMessage(session.RoleUser, "summarize")}, tools, 4096)
	if err != nil || response.Text() != "summary" {
		t.Fatalf("Complete = %q, %v", response.Text(), err)
	}
	if body.ToolChoice == nil || body.ToolChoice.Type != "none" || body.MaxTokens != 4096 || len(body.Tools) != 1 {
		t.Fatalf("request = %#v", body)
	}
}
