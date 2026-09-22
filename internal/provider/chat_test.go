package provider

import (
	"context"
	"encoding/json"
	"errors"
	"io"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"

	"github.com/hizkifw/kon/internal/session"
)

// newTestModel returns a chatModel pointed at a stub server, plus the server.
func newTestModel(t *testing.T, handler http.HandlerFunc) *chatModel {
	t.Helper()
	server := httptest.NewServer(handler)
	t.Cleanup(server.Close)
	return &chatModel{
		client:  server.Client(),
		baseURL: server.URL,
		model:   "test-model",
		headers: map[string]string{"X-Custom": "custom-value"},
	}
}

// sse formats one server-sent event.
func sse(payload string) string {
	return "data: " + payload + "\n\n"
}

func TestChatStreamAssemblesDeltas(t *testing.T) {
	events := strings.Join([]string{
		sse(`{"choices":[{"index":0,"delta":{"role":"assistant"}}]}`),
		sse(`{"choices":[{"index":0,"delta":{"reasoning_content":"let me "}}]}`),
		sse(`{"choices":[{"index":0,"delta":{"reasoning_content":"look"}}]}`),
		sse(`{"choices":[{"index":0,"delta":{"content":"check"}}]}`),
		sse(`{"choices":[{"index":0,"delta":{"content":"ing"}}]}`),
		// Tool call spread across deltas: identity first, argument fragments after.
		sse(`{"choices":[{"index":0,"delta":{"tool_calls":[{"index":0,"id":"call-1","type":"function","function":{"name":"read","arguments":""}}]}}]}`),
		sse(`{"choices":[{"index":0,"delta":{"tool_calls":[{"index":0,"function":{"arguments":"{\"pa"}}]}}]}`),
		sse(`{"choices":[{"index":0,"delta":{"tool_calls":[{"index":0,"function":{"arguments":"th\":\"x\"}"}}]}}]}`),
		sse(`{"choices":[{"index":0,"delta":{},"finish_reason":"tool_calls"}]}`),
		sse(`{"choices":[],"usage":{"prompt_tokens":312,"completion_tokens":89,"total_tokens":401,"prompt_tokens_details":{"cached_tokens":148}}}`),
		"data: [DONE]\n\n",
	}, "")
	var emitted []Event
	model := newTestModel(t, func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "text/event-stream")
		_, _ = io.WriteString(w, events)
	})
	response, err := model.Stream(context.Background(), []session.Message{{Role: session.RoleUser, Content: "hi"}}, nil, func(event Event) {
		emitted = append(emitted, event)
	})
	if err != nil {
		t.Fatal(err)
	}
	if response.Text != "checking" || response.Reasoning != "let me look" || response.Finish != "tool_calls" {
		t.Fatalf("response = %#v", response)
	}
	if len(response.ToolCalls) != 1 || response.ToolCalls[0].ID != "call-1" || response.ToolCalls[0].Name != "read" || string(response.ToolCalls[0].Arguments) != `{"path":"x"}` {
		t.Fatalf("tool calls = %#v", response.ToolCalls)
	}
	if response.Usage == nil || response.Usage.PromptTokens != 312 || response.Usage.CompletionTokens != 89 || response.Usage.TotalTokens != 401 {
		t.Fatalf("usage = %#v", response.Usage)
	}
	// prompt_tokens covers every input token, cached or not; the cached share
	// is recorded separately for context management.
	if response.Usage.CachedTokens != 148 {
		t.Fatalf("cached tokens = %d", response.Usage.CachedTokens)
	}
	if len(emitted) != 4 || emitted[0].Thinking != true || emitted[0].Text != "let me " || emitted[1].Text != "look" || emitted[2].Text != "check" || emitted[3].Thinking {
		t.Fatalf("emitted = %#v", emitted)
	}
}

func TestChatStreamSendsChatCompletionsBody(t *testing.T) {
	var method, path, authorization, accept, custom string
	var body chatRequest
	model := newTestModel(t, func(w http.ResponseWriter, r *http.Request) {
		method, path = r.Method, r.URL.Path
		authorization = r.Header.Get("Authorization")
		accept = r.Header.Get("Accept")
		custom = r.Header.Get("X-Custom")
		raw, _ := io.ReadAll(r.Body)
		_ = json.Unmarshal(raw, &body)
		w.Header().Set("Content-Type", "text/event-stream")
		_, _ = io.WriteString(w, sse(`{"choices":[{"index":0,"delta":{"content":"ok"},"finish_reason":"stop"}]}`)+"data: [DONE]\n\n")
	})
	messages := []session.Message{
		{Role: session.RoleSystem, Content: "be brief"},
		{Role: session.RoleUser, Content: "hi"},
		{Role: session.RoleAssistant, Content: "", ToolCalls: []session.ToolCall{{
			ID: "call-9", Type: "function",
			Function: session.ToolFunction{Name: "edit", Arguments: json.RawMessage(`{"path":"x"}`)},
		}}},
		{Role: session.RoleTool, Content: "done", ToolCallID: "call-9", Name: "edit"},
		// Reasoning parts are display-only for this format and must not leak
		// into the request.
		{Role: session.RoleAssistant, Content: "checking", Parts: []session.Part{{Type: PartReasoning, Text: "secret thoughts"}}},
	}
	tools := []Tool{{Name: "edit", Description: "Edit a file", Parameters: json.RawMessage(`{"type":"object"}`)}}
	model.apiKey = "sk-test"
	response, err := model.Stream(context.Background(), messages, tools, func(Event) {})
	if err != nil {
		t.Fatal(err)
	}
	if response.Text != "ok" || response.Finish != "stop" {
		t.Fatalf("response = %#v", response)
	}
	if method != http.MethodPost || path != "/chat/completions" {
		t.Fatalf("request = %s %s", method, path)
	}
	if authorization != "Bearer sk-test" || accept != "text/event-stream" || custom != "custom-value" {
		t.Fatalf("headers: auth=%q accept=%q custom=%q", authorization, accept, custom)
	}
	if body.Model != "test-model" || !body.Stream || body.StreamOptions == nil || !body.StreamOptions.IncludeUsage || body.MaxTokens != 0 {
		t.Fatalf("request = %#v", body)
	}
	if len(body.Messages) != 5 {
		t.Fatalf("messages = %#v", body.Messages)
	}
	if body.Messages[0].Role != "system" || body.Messages[0].Content.(string) != "be brief" {
		t.Fatalf("system message = %#v", body.Messages[0])
	}
	assistant := body.Messages[2]
	if assistant.Content != nil {
		t.Fatalf("empty assistant content = %#v, want omitted", assistant.Content)
	}
	if len(assistant.ToolCalls) != 1 || assistant.ToolCalls[0].ID != "call-9" || assistant.ToolCalls[0].Type != "function" || assistant.ToolCalls[0].Function.Name != "edit" || assistant.ToolCalls[0].Function.Arguments != `{"path":"x"}` {
		t.Fatalf("assistant tool calls = %#v", assistant.ToolCalls)
	}
	if body.Messages[3].Role != "tool" || body.Messages[3].ToolCallID != "call-9" || body.Messages[3].Content.(string) != "done" {
		t.Fatalf("tool message = %#v", body.Messages[3])
	}
	if body.Messages[4].Content == nil || body.Messages[4].Content.(string) != "checking" {
		t.Fatalf("assistant message = %#v", body.Messages[4])
	}
	if len(body.Tools) != 1 || body.Tools[0].Type != "function" || body.Tools[0].Function.Name != "edit" || string(body.Tools[0].Function.Parameters) != `{"type":"object"}` {
		t.Fatalf("tools = %#v", body.Tools)
	}
}

func TestChatStreamSynthesizesMissingToolCallBits(t *testing.T) {
	// Some compatible servers stream tool calls with no ID and no arguments.
	// finalizeToolCalls must run on the streaming path too.
	model := newTestModel(t, func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "text/event-stream")
		_, _ = io.WriteString(w, sse(`{"choices":[{"index":0,"delta":{"tool_calls":[{"index":0,"type":"function","function":{"name":"shell"}}]}}],"finish_reason":null}`)+"data: [DONE]\n\n")
	})
	response, err := model.Stream(context.Background(), []session.Message{{Role: session.RoleUser, Content: "hi"}}, nil, func(Event) {})
	if err != nil {
		t.Fatal(err)
	}
	if len(response.ToolCalls) != 1 || response.ToolCalls[0].ID != "call_0" || response.ToolCalls[0].Name != "shell" || string(response.ToolCalls[0].Arguments) != `{}` {
		t.Fatalf("tool calls = %#v", response.ToolCalls)
	}
}

func TestChatStreamRejectsMalformedArguments(t *testing.T) {
	model := newTestModel(t, func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "text/event-stream")
		_, _ = io.WriteString(w, sse(`{"choices":[{"index":0,"delta":{"tool_calls":[{"index":0,"id":"call-1","type":"function","function":{"name":"read","arguments":"{oops"}}]}}]}`)+"data: [DONE]\n\n")
	})
	if _, err := model.Stream(context.Background(), []session.Message{{Role: session.RoleUser, Content: "hi"}}, nil, func(Event) {}); err == nil {
		t.Fatal("malformed streamed tool arguments were accepted")
	}
}

func TestChatStreamRetriesWithoutStreamOptions(t *testing.T) {
	streamOptionsRequests := 0
	model := newTestModel(t, func(w http.ResponseWriter, r *http.Request) {
		var body chatRequest
		raw, _ := io.ReadAll(r.Body)
		_ = json.Unmarshal(raw, &body)
		if body.StreamOptions != nil {
			streamOptionsRequests++
			w.WriteHeader(http.StatusBadRequest)
			_, _ = io.WriteString(w, `{"error":{"message":"Unknown field: stream_options","type":"invalid_request_error"}}`)
			return
		}
		w.Header().Set("Content-Type", "text/event-stream")
		_, _ = io.WriteString(w, sse(`{"choices":[{"index":0,"delta":{"content":"ok"},"finish_reason":"stop"}]}`)+"data: [DONE]\n\n")
	})
	response, err := model.Stream(context.Background(), []session.Message{{Role: session.RoleUser, Content: "hi"}}, nil, func(Event) {})
	if err != nil {
		t.Fatal(err)
	}
	if response.Text != "ok" || streamOptionsRequests != 1 {
		t.Fatalf("response = %#v, stream_options requests = %d", response, streamOptionsRequests)
	}
}

func TestChatStreamSurfacesInStreamError(t *testing.T) {
	model := newTestModel(t, func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "text/event-stream")
		_, _ = io.WriteString(w, sse(`{"error":{"message":"quota exceeded","type":"insufficient_quota","code":429}}`))
	})
	_, err := model.Stream(context.Background(), []session.Message{{Role: session.RoleUser, Content: "hi"}}, nil, func(Event) {})
	var apiErr *APIError
	if !errors.As(err, &apiErr) {
		t.Fatalf("err = %v, want *APIError", err)
	}
	if apiErr.Status != 0 || apiErr.Message != "quota exceeded" || apiErr.Code != "429" {
		t.Fatalf("api error = %#v", apiErr)
	}
	if IsContextOverflow(err) {
		t.Fatal("quota error classified as overflow")
	}
}

func TestChatStreamSurfacesHTTPError(t *testing.T) {
	model := newTestModel(t, func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusUnauthorized)
		_, _ = io.WriteString(w, `{"error":{"message":"invalid api key","type":"invalid_request_error","code":"invalid_api_key"}}`)
	})
	_, err := model.Stream(context.Background(), []session.Message{{Role: session.RoleUser, Content: "hi"}}, nil, func(Event) {})
	if err == nil {
		t.Fatal("expected error")
	}
	var apiErr *APIError
	if !errors.As(err, &apiErr) || apiErr.Status != http.StatusUnauthorized || apiErr.Code != "invalid_api_key" {
		t.Fatalf("err = %#v", err)
	}
}

func TestChatCompleteMapsMessageAndUsage(t *testing.T) {
	var body chatRequest
	model := newTestModel(t, func(w http.ResponseWriter, r *http.Request) {
		raw, _ := io.ReadAll(r.Body)
		_ = json.Unmarshal(raw, &body)
		w.Header().Set("Content-Type", "application/json")
		_, _ = io.WriteString(w, `{
			"choices": [{
				"index": 0,
				"message": {
					"role": "assistant",
					"content": "summary",
					"reasoning_content": "pondering",
					"tool_calls": [{"id": "call-1", "type": "function", "function": {"name": "read", "arguments": "{\"path\":\"x\"}"}}]
				},
				"finish_reason": "tool_calls"
			}],
			"usage": {"prompt_tokens": 50, "completion_tokens": 25, "total_tokens": 75}
		}`)
	})
	response, err := model.Complete(context.Background(), []session.Message{{Role: session.RoleUser, Content: "hi"}}, nil, 4096)
	if err != nil {
		t.Fatal(err)
	}
	if body.MaxTokens != 4096 || body.Stream || body.Tools != nil {
		t.Fatalf("request = %#v", body)
	}
	if response.Text != "summary" || response.Reasoning != "pondering" || response.Finish != "tool_calls" {
		t.Fatalf("response = %#v", response)
	}
	if len(response.ToolCalls) != 1 || response.ToolCalls[0].ID != "call-1" || string(response.ToolCalls[0].Arguments) != `{"path":"x"}` {
		t.Fatalf("tool calls = %#v", response.ToolCalls)
	}
	if response.Usage == nil || response.Usage.PromptTokens != 50 || response.Usage.CompletionTokens != 25 || response.Usage.TotalTokens != 75 {
		t.Fatalf("usage = %#v", response.Usage)
	}
}

func TestChatCompleteSendsToolsWithToolChoiceNone(t *testing.T) {
	var body chatRequest
	model := newTestModel(t, func(w http.ResponseWriter, r *http.Request) {
		raw, _ := io.ReadAll(r.Body)
		_ = json.Unmarshal(raw, &body)
		w.Header().Set("Content-Type", "application/json")
		_, _ = io.WriteString(w, `{"choices":[{"index":0,"message":{"role":"assistant","content":"summary"},"finish_reason":"stop"}]}`)
	})
	toolList := []Tool{{Name: "read", Description: "read a file", Parameters: json.RawMessage(`{"type":"object"}`)}}
	if _, err := model.Complete(context.Background(), []session.Message{{Role: session.RoleUser, Content: "hi"}}, toolList, 0); err != nil {
		t.Fatal(err)
	}
	if len(body.Tools) != 1 || body.Tools[0].Function.Name != "read" {
		t.Fatalf("tools = %#v", body.Tools)
	}
	if body.ToolChoice != "none" {
		t.Fatalf("tool_choice = %q, want none", body.ToolChoice)
	}
}

func TestChatCompleteOmitsToolsWhenEmpty(t *testing.T) {
	var body chatRequest
	model := newTestModel(t, func(w http.ResponseWriter, r *http.Request) {
		raw, _ := io.ReadAll(r.Body)
		_ = json.Unmarshal(raw, &body)
		w.Header().Set("Content-Type", "application/json")
		_, _ = io.WriteString(w, `{"choices":[{"index":0,"message":{"role":"assistant","content":"summary"},"finish_reason":"stop"}]}`)
	})
	if _, err := model.Complete(context.Background(), []session.Message{{Role: session.RoleUser, Content: "hi"}}, nil, 0); err != nil {
		t.Fatal(err)
	}
	if body.Tools != nil || body.ToolChoice != "" {
		t.Fatalf("request = %#v", body)
	}
}

func TestChatCompleteDerivesTotalTokens(t *testing.T) {
	model := newTestModel(t, func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		_, _ = io.WriteString(w, `{"choices":[{"index":0,"message":{"role":"assistant","content":"done"},"finish_reason":"stop"}],"usage":{"prompt_tokens":10,"completion_tokens":5}}`)
	})
	response, err := model.Complete(context.Background(), nil, nil, 0)
	if err != nil {
		t.Fatal(err)
	}
	if response.Usage == nil || response.Usage.TotalTokens != 15 || response.Usage.PromptTokens != 10 {
		t.Fatalf("usage = %#v", response.Usage)
	}
}

func TestChatCompleteSynthesizesMissingToolCallBits(t *testing.T) {
	model := newTestModel(t, func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		_, _ = io.WriteString(w, `{"choices":[{"index":0,"message":{"role":"assistant","tool_calls":[{"id":"","type":"function","function":{"name":"shell","arguments":""}}]},"finish_reason":"tool_calls"}]}`)
	})
	response, err := model.Complete(context.Background(), nil, nil, 0)
	if err != nil {
		t.Fatal(err)
	}
	if len(response.ToolCalls) != 1 || response.ToolCalls[0].ID != "call_0" || string(response.ToolCalls[0].Arguments) != `{}` {
		t.Fatalf("tool calls = %#v", response.ToolCalls)
	}
}

func TestChatCompleteRejectsMalformedArguments(t *testing.T) {
	model := newTestModel(t, func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		_, _ = io.WriteString(w, `{"choices":[{"index":0,"message":{"role":"assistant","tool_calls":[{"id":"call-1","type":"function","function":{"name":"read","arguments":"{oops"}}]},"finish_reason":"tool_calls"}]}`)
	})
	if _, err := model.Complete(context.Background(), nil, nil, 0); err == nil {
		t.Fatal("malformed tool arguments were accepted")
	}
}

func TestChatCompleteRetriesWithMaxCompletionTokens(t *testing.T) {
	attempts := 0
	var finalBody chatRequest
	model := newTestModel(t, func(w http.ResponseWriter, r *http.Request) {
		var body chatRequest
		raw, _ := io.ReadAll(r.Body)
		_ = json.Unmarshal(raw, &body)
		attempts++
		if body.MaxTokens > 0 {
			w.WriteHeader(http.StatusBadRequest)
			_, _ = io.WriteString(w, `{"error":{"message":"Unsupported parameter: 'max_tokens' is not supported with this model. Use 'max_completion_tokens' instead.","type":"invalid_request_error"}}`)
			return
		}
		finalBody = body
		w.Header().Set("Content-Type", "application/json")
		_, _ = io.WriteString(w, `{"choices":[{"index":0,"message":{"role":"assistant","content":"summary"},"finish_reason":"stop"}],"usage":{"prompt_tokens":10,"completion_tokens":2,"total_tokens":12}}`)
	})
	response, err := model.Complete(context.Background(), []session.Message{{Role: session.RoleUser, Content: "hi"}}, nil, 4096)
	if err != nil {
		t.Fatal(err)
	}
	if attempts != 2 {
		t.Fatalf("attempts = %d", attempts)
	}
	if finalBody.MaxTokens != 0 || finalBody.MaxCompletionTokens != 4096 {
		t.Fatalf("retry request = %#v", finalBody)
	}
	if response.Text != "summary" {
		t.Fatalf("response = %#v", response)
	}
}

func TestChatStreamToleratesKeepAlivesAndCRLF(t *testing.T) {
	events := ": keep-alive\n\n" + "event: message\r\ndata: {\"choices\":[{\"index\":0,\"delta\":{\"content\":\"hi\"}}]}\r\n\r\n" + "data: [DONE]\r\n\r\n"
	model := newTestModel(t, func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "text/event-stream")
		_, _ = io.WriteString(w, events)
	})
	response, err := model.Stream(context.Background(), nil, nil, func(Event) {})
	if err != nil {
		t.Fatal(err)
	}
	if response.Text != "hi" {
		t.Fatalf("response = %#v", response)
	}
}

func TestChatStreamIgnoresForeignChoicesAndNegativeIndexes(t *testing.T) {
	events := strings.Join([]string{
		// A second choice (n > 1) and a malformed negative index must not
		// disturb the assembled response.
		sse(`{"choices":[{"index":1,"delta":{"content":"other"}}]}`),
		sse(`{"choices":[{"index":0,"delta":{"content":"hi"}},{"index":-1,"delta":{"content":"nope"}}]}`),
		sse(`{"choices":[{"index":0,"delta":{"tool_calls":[{"index":-1,"id":"x","function":{"name":"read"}}]}}]}`),
		"data: [DONE]\n\n",
	}, "")
	model := newTestModel(t, func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "text/event-stream")
		_, _ = io.WriteString(w, events)
	})
	response, err := model.Stream(context.Background(), nil, nil, func(Event) {})
	if err != nil {
		t.Fatal(err)
	}
	if response.Text != "hi" || len(response.ToolCalls) != 0 {
		t.Fatalf("response = %#v", response)
	}
}

func TestChatStreamAssemblesParallelToolCalls(t *testing.T) {
	events := strings.Join([]string{
		sse(`{"choices":[{"index":0,"delta":{"tool_calls":[{"index":0,"id":"call-a","type":"function","function":{"name":"read","arguments":"{\"path\":\"a\"}"}}]}}]}`),
		sse(`{"choices":[{"index":0,"delta":{"tool_calls":[{"index":1,"id":"call-b","type":"function","function":{"name":"shell","arguments":"{\"comm"}}]}}]}`),
		sse(`{"choices":[{"index":0,"delta":{"tool_calls":[{"index":1,"function":{"arguments":"and\":\"ls\"}"}}]}}]}`),
		sse(`{"choices":[{"index":0,"delta":{},"finish_reason":"tool_calls"}]}`),
		"data: [DONE]\n\n",
	}, "")
	model := newTestModel(t, func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "text/event-stream")
		_, _ = io.WriteString(w, events)
	})
	response, err := model.Stream(context.Background(), nil, nil, func(Event) {})
	if err != nil {
		t.Fatal(err)
	}
	if len(response.ToolCalls) != 2 {
		t.Fatalf("tool calls = %#v", response.ToolCalls)
	}
	if response.ToolCalls[0].ID != "call-a" || response.ToolCalls[0].Name != "read" || string(response.ToolCalls[0].Arguments) != `{"path":"a"}` {
		t.Fatalf("first call = %#v", response.ToolCalls[0])
	}
	if response.ToolCalls[1].ID != "call-b" || response.ToolCalls[1].Name != "shell" || string(response.ToolCalls[1].Arguments) != `{"command":"ls"}` {
		t.Fatalf("second call = %#v", response.ToolCalls[1])
	}
}

func TestToChatMessagesSkipsEmptyContent(t *testing.T) {
	messages, err := toChatMessages([]session.Message{
		{Role: session.RoleUser, Content: "hi"},
		{Role: session.RoleAssistant, Content: "", ToolCalls: []session.ToolCall{{ID: "1", Function: session.ToolFunction{Name: "read", Arguments: json.RawMessage(`{}`)}}}},
		{Role: session.RoleTool, Content: "", ToolCallID: "1", Name: "read"},
	})
	if err != nil {
		t.Fatal(err)
	}
	if len(messages) != 3 {
		t.Fatalf("messages = %#v", messages)
	}
	// Tool results must always carry a content field, even when empty.
	if messages[2].Content == nil || *messages[2].Content.(*string) != "" {
		t.Fatalf("tool content = %#v", messages[2].Content)
	}
	if messages[2].ToolCallID != "1" {
		t.Fatalf("tool call ID = %q", messages[2].ToolCallID)
	}
}

func TestToChatMessagesRejectsUnknownRole(t *testing.T) {
	// A role this wire format cannot send must fail the request instead of
	// silently truncating the conversation.
	if _, err := toChatMessages([]session.Message{{Role: "hyper", Content: "hi"}}); err == nil {
		t.Fatal("unknown role was accepted")
	}
}

func TestChatStreamKeepsPartialOutputOnTruncatedStream(t *testing.T) {
	// The server sends reasoning and text, then closes without [DONE] or a
	// finish reason. The decoded response must still carry what arrived so the
	// caller can persist the partial turn.
	events := sse(`{"choices":[{"index":0,"delta":{"reasoning_content":"thinking..."}}]}`) +
		sse(`{"choices":[{"index":0,"delta":{"content":"half an ans"}}]}`)
	model := newTestModel(t, func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "text/event-stream")
		_, _ = io.WriteString(w, events)
	})
	response, err := model.Stream(context.Background(), nil, nil, func(Event) {})
	if err == nil {
		t.Fatal("truncated stream was reported as complete")
	}
	if response.Text != "half an ans" || response.Reasoning != "thinking..." {
		t.Fatalf("partial output lost: %#v", response)
	}
}

func TestChatStreamReportsErrorMidStreamButKeepsDeltas(t *testing.T) {
	events := sse(`{"choices":[{"index":0,"delta":{"content":"started"}}]}`) +
		sse(`{"error":{"message":"upstream exploded"}}`)
	model := newTestModel(t, func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "text/event-stream")
		_, _ = io.WriteString(w, events)
	})
	response, err := model.Stream(context.Background(), nil, nil, func(Event) {})
	if err == nil {
		t.Fatal("mid-stream error was swallowed")
	}
	if response.Text != "started" {
		t.Fatalf("deltas before the error were lost: %#v", response)
	}
}

// chatFinishReasonMapsLegacyFunctionCall verifies legacy finish reasons unify
// on one spelling.
func TestChatFinishReasonMapsLegacyFunctionCall(t *testing.T) {
	if got := chatFinishReason("function_call"); got != "tool_calls" {
		t.Fatalf("finish reason = %q", got)
	}
	if got := chatFinishReason("length"); got != "length" {
		t.Fatalf("finish reason = %q", got)
	}
}
