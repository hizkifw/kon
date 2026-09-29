package provider

import (
	"context"
	"encoding/json"
	"errors"
	"io"
	"net/http"
	"net/http/httptest"
	"reflect"
	"strings"
	"testing"

	"github.com/hizkifw/kon/internal/buildinfo"
	"github.com/hizkifw/kon/internal/provider/wire"
	"github.com/hizkifw/kon/internal/session"
	"github.com/hizkifw/kon/internal/tokens"
	"github.com/hizkifw/kon/internal/typedid"
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
		spec:    compatibleSpec(),
		headers: map[string]string{"X-Custom": "custom-value"},
	}
}

func compatibleSpec() wire.Spec {
	spec, _ := wire.Lookup(wire.OpenAICompatible)
	return spec
}

// TestChatHeadersOverrideUserAgent keeps the configured headers authoritative:
// a gateway that filters on User-Agent must still be reachable.
func TestChatHeadersOverrideUserAgent(t *testing.T) {
	var userAgent string
	model := newTestModel(t, func(w http.ResponseWriter, r *http.Request) {
		userAgent = r.Header.Get("User-Agent")
		_, _ = io.WriteString(w, okStream)
	})
	model.headers = map[string]string{"User-Agent": "custom/1"}
	if _, err := model.Complete(context.Background(), []session.Message{session.TextMessage(session.RoleUser, "hi")}, nil, 0, nil); err != nil {
		t.Fatal(err)
	}
	if userAgent != "custom/1" {
		t.Fatalf("User-Agent = %q, want the configured override", userAgent)
	}
}

// sse formats one server-sent event.
func sse(payload string) string {
	return "data: " + payload + "\n\n"
}

// okStream is a complete streamed reply saying "ok".
var okStream = sse(`{"choices":[{"index":0,"delta":{"content":"ok"},"finish_reason":"stop"}]}`) + "data: [DONE]\n\n"

// assertWireJSON fails unless got encodes the same JSON value as want, ignoring
// key order. Decoding a request into the struct that encoded it would hide a
// renamed field, which round-trips fine but is unknown to the real server, so
// wire tests compare generic JSON instead.
func assertWireJSON(t *testing.T, got []byte, want string) {
	t.Helper()
	canonical := func(raw []byte) string {
		var value any
		if err := json.Unmarshal(raw, &value); err != nil {
			t.Fatalf("decode %s: %v", raw, err)
		}
		// Maps marshal with sorted keys, which makes the form canonical.
		out, err := json.Marshal(value)
		if err != nil {
			t.Fatal(err)
		}
		return string(out)
	}
	if gotJSON, wantJSON := canonical(got), canonical([]byte(want)); gotJSON != wantJSON {
		t.Fatalf("wire JSON:\n got %s\nwant %s", gotJSON, wantJSON)
	}
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
	response, err := model.Stream(context.Background(), []session.Message{session.TextMessage(session.RoleUser, "hi")}, nil, func(event Event) {
		emitted = append(emitted, event)
	})
	if err != nil {
		t.Fatal(err)
	}
	if response.Text() != "checking" || response.Reasoning() != "let me look" || response.Finish != "tool_calls" {
		t.Fatalf("response = %#v", response)
	}
	if len(response.ToolCalls()) != 1 || response.ToolCalls()[0].ID != "call-1" || response.ToolCalls()[0].Function.Name != "read" || string(response.ToolCalls()[0].Function.Arguments) != `{"path":"x"}` {
		t.Fatalf("tool calls = %#v", response.ToolCalls())
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

func TestChatRequestSendsSelectedEffort(t *testing.T) {
	var bodies []string
	model := newTestModel(t, func(w http.ResponseWriter, r *http.Request) {
		raw, _ := io.ReadAll(r.Body)
		bodies = append(bodies, string(raw))
		_, _ = io.WriteString(w, okStream)
	})
	messages := []session.Message{session.TextMessage(session.RoleUser, "hi")}
	for _, effort := range []struct {
		format wire.Format
		effort string
	}{{wire.OpenAICompatible, ""}, {wire.OpenAI, "low"}, {wire.OpenRouter, "max"}} {
		model.spec, _ = wire.Lookup(effort.format)
		model.effort = effort.effort
		if _, err := model.Complete(context.Background(), messages, nil, 0, nil); err != nil {
			t.Fatal(err)
		}
	}
	if strings.Contains(bodies[0], "reasoning") {
		t.Fatalf("default effort leaked into request: %s", bodies[0])
	}
	if !strings.Contains(bodies[1], `"reasoning_effort":"low"`) {
		t.Fatalf("openai request = %s", bodies[1])
	}
	if !strings.Contains(bodies[2], `"reasoning":{"effort":"max"}`) || strings.Contains(bodies[2], "reasoning_effort") {
		t.Fatalf("openrouter request = %s", bodies[2])
	}
}

// captureRequests serves a canned reply and records each request body.
func captureRequests(t *testing.T) (*chatModel, *[]string) {
	var bodies []string
	model := newTestModel(t, func(w http.ResponseWriter, r *http.Request) {
		raw, _ := io.ReadAll(r.Body)
		bodies = append(bodies, string(raw))
		_, _ = io.WriteString(w, okStream)
	})
	return model, &bodies
}

// assistantWire sends one assistant message through toChatMessages and returns
// its wire form as JSON.
func assistantWire(t *testing.T, replay chatReplay, message session.Message) string {
	t.Helper()
	wire, err := toChatMessages([]session.Message{session.TextMessage(session.RoleUser, "hi"), message}, replay, nil)
	if err != nil {
		t.Fatal(err)
	}
	raw, err := json.Marshal(wire[1])
	if err != nil {
		t.Fatal(err)
	}
	return string(raw)
}

func thinkingReply(model, options string) session.Message {
	message := session.Message{Role: session.RoleAssistant, Model: typedid.ExternalModelID(model), Parts: []session.Part{{Type: session.PartReasoning, Text: "think"}, {Type: session.PartText, Text: "hello"}}}
	if options != "" {
		message.ProviderOptions = json.RawMessage(options)
	}
	return message
}

// TestChatStreamRecordsReasoningField checks that a stream records which field
// its reasoning arrived in, for each convention compatible servers use.
func TestChatStreamRecordsReasoningField(t *testing.T) {
	for _, field := range []string{"reasoning_content", "reasoning", "reasoning_text"} {
		t.Run(field, func(t *testing.T) {
			events := sse(`{"choices":[{"index":0,"delta":{"`+field+`":"think"}}]}`) +
				sse(`{"choices":[{"index":0,"delta":{"content":"hello"},"finish_reason":"stop"}]}`) + "data: [DONE]\n\n"
			response, err := decodeChatStream(strings.NewReader(events), nil)
			if err != nil {
				t.Fatal(err)
			}
			if response.Reasoning() != "think" {
				t.Fatalf("reasoning = %q", response.Reasoning())
			}
			if got := decodeChatOptions(response.ProviderOptions).ReasoningField; got != field {
				t.Fatalf("recorded field = %q, want %q", got, field)
			}
		})
	}
}

// TestChatReplayReturnsReasoningInRecordedField checks that reasoning goes back
// in the field it arrived in, and that a message which recorded none falls back
// to the wire type's default.
func TestChatReplayReturnsReasoningInRecordedField(t *testing.T) {
	replay := chatReplay{model: "m", defaultField: "reasoning_content"}
	for field, want := range map[string]string{
		"reasoning_content": `"reasoning_content":"think"`,
		"reasoning":         `"reasoning":"think"`,
		"reasoning_text":    `"reasoning_text":"think"`,
	} {
		got := assistantWire(t, replay, thinkingReply("m", `{"reasoning_field":"`+field+`"}`))
		if !strings.Contains(got, want) || strings.Count(got, "reason") != 1 {
			t.Fatalf("%s: wire = %s", field, got)
		}
	}
	if got := assistantWire(t, replay, thinkingReply("m", "")); !strings.Contains(got, `"reasoning_content":"think"`) {
		t.Fatalf("unrecorded default = %s", got)
	}
	model, bodies := captureRequests(t)
	model.spec, _ = wire.Lookup(wire.OpenRouter)
	if _, err := model.Complete(context.Background(), []session.Message{session.TextMessage(session.RoleUser, "hi"), thinkingReply("test-model", ""), session.TextMessage(session.RoleUser, "again")}, nil, 0, nil); err != nil {
		t.Fatal(err)
	}
	if got := (*bodies)[0]; !strings.Contains(got, `"reasoning":"think"`) || strings.Contains(got, "reasoning_content") {
		t.Fatalf("openrouter default = %s", got)
	}
}

// TestChatReplayDropsOtherModelsReasoning checks that reasoning written by a
// different model is not handed to the requesting one, while its answer is.
func TestChatReplayDropsOtherModelsReasoning(t *testing.T) {
	got := assistantWire(t, chatReplay{model: "b", defaultField: "reasoning_content"}, thinkingReply("a", `{"reasoning_field":"reasoning","reasoning_details":[{"type":"reasoning.encrypted","data":"x"}]}`))
	if strings.Contains(got, "reason") || !strings.Contains(got, `"content":"hello"`) {
		t.Fatalf("wire = %s", got)
	}
}

// TestChatStreamMergesReasoningDetails checks OpenRouter's structured
// reasoning: streamed text pieces join into one entry, encrypted entries stay
// discrete and opaque, unknown entries are dropped, and the whole list goes
// back verbatim in place of the plain reasoning field.
func TestChatStreamMergesReasoningDetails(t *testing.T) {
	events := sse(`{"choices":[{"index":0,"delta":{"reasoning":"let me ","reasoning_details":[{"type":"reasoning.text","text":"let me ","index":0}]}}]}`) +
		sse(`{"choices":[{"index":0,"delta":{"reasoning":"look","reasoning_details":[{"type":"reasoning.text","text":"look","signature":"sig","index":0}]}}]}`) +
		sse(`{"choices":[{"index":0,"delta":{"reasoning_details":[{"type":"reasoning.encrypted","data":"opaque","id":"r1"},{"type":"mystery"}]}}]}`) +
		sse(`{"choices":[{"index":0,"delta":{"content":"hello"},"finish_reason":"stop"}]}`) + "data: [DONE]\n\n"
	response, err := decodeChatStream(strings.NewReader(events), nil)
	if err != nil {
		t.Fatal(err)
	}
	details := decodeChatOptions(response.ProviderOptions).ReasoningDetails
	if len(details) != 2 {
		t.Fatalf("details = %s", details)
	}
	var text map[string]any
	if err := json.Unmarshal(details[0], &text); err != nil {
		t.Fatal(err)
	}
	if text["text"] != "let me look" || text["signature"] != "sig" {
		t.Fatalf("merged text detail = %s", details[0])
	}
	if string(details[1]) != `{"type":"reasoning.encrypted","data":"opaque","id":"r1"}` {
		t.Fatalf("encrypted detail was altered: %s", details[1])
	}
	message := session.Message{Role: session.RoleAssistant, Model: "m", Parts: response.Parts, ProviderOptions: response.ProviderOptions}
	got := assistantWire(t, chatReplay{model: "m", defaultField: "reasoning"}, message)
	if !strings.Contains(got, `"reasoning_details":[`) || strings.Contains(got, `"reasoning":`) {
		t.Fatalf("wire = %s", got)
	}
}

// TestChatReplaySendsEmptyReasoningToDeepSeek checks DeepSeek's rule for a
// reasoning model: every assistant message carries reasoning_content, empty
// when it has none, and no other server is sent the empty field.
func TestChatReplaySendsEmptyReasoningToDeepSeek(t *testing.T) {
	plain := session.Message{Role: session.RoleAssistant, Model: "m", Parts: []session.Part{{Type: session.PartText, Text: "hello"}}}
	if got := assistantWire(t, chatReplay{model: "m", emptyReasoning: true}, plain); !strings.Contains(got, `"reasoning_content":""`) {
		t.Fatalf("deepseek wire = %s", got)
	}
	if got := assistantWire(t, chatReplay{model: "m"}, plain); strings.Contains(got, "reason") {
		t.Fatalf("other server wire = %s", got)
	}
	for _, c := range []struct {
		baseURL   string
		reasoning bool
		want      bool
	}{
		{"https://api.deepseek.com/v1", true, true},
		{"https://api.deepseek.com/v1", false, false},
		{"https://api.fireworks.ai/inference/v1", true, false},
	} {
		model := &chatModel{baseURL: c.baseURL, reasoning: c.reasoning}
		if got := model.replay().emptyReasoning; got != c.want {
			t.Errorf("replay(%s, reasoning=%v).emptyReasoning = %v", c.baseURL, c.reasoning, got)
		}
	}
}

func TestChatStreamSendsChatCompletionsBody(t *testing.T) {
	var method, path, authorization, accept, custom, userAgent string
	var body []byte
	model := newTestModel(t, func(w http.ResponseWriter, r *http.Request) {
		method, path = r.Method, r.URL.Path
		authorization = r.Header.Get("Authorization")
		accept = r.Header.Get("Accept")
		custom = r.Header.Get("X-Custom")
		userAgent = r.Header.Get("User-Agent")
		body, _ = io.ReadAll(r.Body)
		w.Header().Set("Content-Type", "text/event-stream")
		_, _ = io.WriteString(w, sse(`{"choices":[{"index":0,"delta":{"content":"ok"},"finish_reason":"stop"}]}`)+"data: [DONE]\n\n")
	})
	messages := []session.Message{
		session.TextMessage(session.RoleSystem, "be brief"),
		session.TextMessage(session.RoleUser, "hi"),
		{Role: session.RoleAssistant, Parts: []session.Part{{Type: session.PartToolCall, ToolCallID: "call-9", ToolName: "edit", ToolInput: json.RawMessage(`{"path":"x"}`)}}},
		session.ToolResultMessage("call-9", "edit", "done"),
		// Reasoning is sent back with the assistant message it belongs to.
		{Role: session.RoleAssistant, Parts: []session.Part{{Type: session.PartReasoning, Text: "secret thoughts"}, {Type: session.PartText, Text: "checking"}}},
	}
	tools := []session.ToolDefinition{{Name: "edit", Description: "Edit a file", Parameters: json.RawMessage(`{"type":"object"}`)}}
	model.apiKey = "sk-test"
	response, err := model.Stream(context.Background(), messages, tools, func(Event) {})
	if err != nil {
		t.Fatal(err)
	}
	if response.Text() != "ok" || response.Finish != "stop" {
		t.Fatalf("response = %#v", response)
	}
	if method != http.MethodPost || path != "/chat/completions" {
		t.Fatalf("request = %s %s", method, path)
	}
	if authorization != "Bearer sk-test" || accept != "text/event-stream" || custom != "custom-value" {
		t.Fatalf("headers: auth=%q accept=%q custom=%q", authorization, accept, custom)
	}
	if userAgent != buildinfo.UserAgent() {
		t.Fatalf("User-Agent = %q, want %q", userAgent, buildinfo.UserAgent())
	}
	// The tool-calling assistant message has no content or reasoning to send,
	// so both keys are left out, as is max_tokens for a streamed turn.
	assertWireJSON(t, body, `{
		"model": "test-model",
		"messages": [
			{"role": "system", "content": "be brief"},
			{"role": "user", "content": "hi"},
			{"role": "assistant", "tool_calls": [{"id": "call-9", "type": "function", "function": {"name": "edit", "arguments": "{\"path\":\"x\"}"}}]},
			{"role": "tool", "tool_call_id": "call-9", "content": "done"},
			{"role": "assistant", "content": "checking", "reasoning_content": "secret thoughts"}
		],
		"tools": [{"type": "function", "function": {"name": "edit", "description": "Edit a file", "parameters": {"type": "object"}}}],
		"stream": true,
		"stream_options": {"include_usage": true}
	}`)
}

func TestChatStreamSynthesizesMissingToolCallBits(t *testing.T) {
	// Some compatible servers stream tool calls with no ID and no arguments.
	// finalizeToolCalls must run on the streaming path too.
	model := newTestModel(t, func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "text/event-stream")
		_, _ = io.WriteString(w, sse(`{"choices":[{"index":0,"delta":{"tool_calls":[{"index":0,"type":"function","function":{"name":"shell"}}]}}],"finish_reason":null}`)+"data: [DONE]\n\n")
	})
	response, err := model.Stream(context.Background(), []session.Message{session.TextMessage(session.RoleUser, "hi")}, nil, func(Event) {})
	if err != nil {
		t.Fatal(err)
	}
	// The server sent no ID, so one is made up; its spelling is kon's choice,
	// but a result cannot be matched to an empty one.
	if len(response.ToolCalls()) != 1 || response.ToolCalls()[0].ID == "" || response.ToolCalls()[0].Function.Name != "shell" || string(response.ToolCalls()[0].Function.Arguments) != `{}` {
		t.Fatalf("tool calls = %#v", response.ToolCalls())
	}
}

func TestChatStreamRejectsMalformedArguments(t *testing.T) {
	model := newTestModel(t, func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "text/event-stream")
		_, _ = io.WriteString(w, sse(`{"choices":[{"index":0,"delta":{"tool_calls":[{"index":0,"id":"call-1","type":"function","function":{"name":"read","arguments":"{oops"}}]}}]}`)+"data: [DONE]\n\n")
	})
	if _, err := model.Stream(context.Background(), []session.Message{session.TextMessage(session.RoleUser, "hi")}, nil, func(Event) {}); err == nil {
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
	response, err := model.Stream(context.Background(), []session.Message{session.TextMessage(session.RoleUser, "hi")}, nil, func(Event) {})
	if err != nil {
		t.Fatal(err)
	}
	if response.Text() != "ok" || streamOptionsRequests != 1 {
		t.Fatalf("response = %#v, stream_options requests = %d", response, streamOptionsRequests)
	}
}

func TestChatStreamSurfacesInStreamError(t *testing.T) {
	model := newTestModel(t, func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "text/event-stream")
		_, _ = io.WriteString(w, sse(`{"error":{"message":"quota exceeded","type":"insufficient_quota","code":429}}`))
	})
	_, err := model.Stream(context.Background(), []session.Message{session.TextMessage(session.RoleUser, "hi")}, nil, func(Event) {})
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
	_, err := model.Stream(context.Background(), []session.Message{session.TextMessage(session.RoleUser, "hi")}, nil, func(Event) {})
	if err == nil {
		t.Fatal("expected error")
	}
	var apiErr *APIError
	if !errors.As(err, &apiErr) || apiErr.Status != http.StatusUnauthorized || apiErr.Code != "invalid_api_key" {
		t.Fatalf("err = %#v", err)
	}
}

// TestChatUsageReadsEachServersCacheFields covers where each server puts the
// prompt cache's share: hits and writes are separate counts, and a zero in
// the OpenAI field does not hide a server's own count.
func TestChatUsageReadsEachServersCacheFields(t *testing.T) {
	for name, tc := range map[string]struct {
		report        string
		cached, write tokens.Count
	}{
		"openai":     {`{"prompt_tokens":100,"completion_tokens":5,"prompt_tokens_details":{"cached_tokens":60}}`, 60, 0},
		"openrouter": {`{"prompt_tokens":100,"completion_tokens":5,"prompt_tokens_details":{"cached_tokens":60,"cache_write_tokens":30}}`, 60, 30},
		"deepseek":   {`{"prompt_tokens":100,"completion_tokens":5,"prompt_cache_hit_tokens":70,"prompt_cache_miss_tokens":30,"prompt_tokens_details":{"cached_tokens":0}}`, 70, 0},
		"kimi":       {`{"prompt_tokens":100,"completion_tokens":5,"cached_tokens":80}`, 80, 0},
		"uncached":   {`{"prompt_tokens":100,"completion_tokens":5}`, 0, 0},
	} {
		var report chatUsage
		if err := json.Unmarshal([]byte(tc.report), &report); err != nil {
			t.Fatal(err)
		}
		got := report.usage()
		if got.PromptTokens != 100 || got.CachedTokens != tc.cached || got.CacheWriteTokens != tc.write {
			t.Errorf("%s: usage = %+v, want %d cached and %d written of 100", name, got, tc.cached, tc.write)
		}
	}
}

// TestChatStreamReadsUsageFromTheChoice covers Moonshot, which reports usage
// on the final choice rather than on the chunk.
func TestChatStreamReadsUsageFromTheChoice(t *testing.T) {
	events := strings.Join([]string{
		sse(`{"choices":[{"index":0,"delta":{"content":"hi"}}]}`),
		sse(`{"choices":[{"index":0,"delta":{},"finish_reason":"stop","usage":{"prompt_tokens":19,"completion_tokens":13,"total_tokens":32,"cached_tokens":16}}]}`),
		"data: [DONE]\n\n",
	}, "")
	model := newTestModel(t, func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "text/event-stream")
		_, _ = io.WriteString(w, events)
	})
	response, err := model.Stream(context.Background(), []session.Message{session.TextMessage(session.RoleUser, "hi")}, nil, nil)
	if err != nil {
		t.Fatal(err)
	}
	if u := response.Usage; u == nil || u.PromptTokens != 19 || u.CompletionTokens != 13 || u.CachedTokens != 16 {
		t.Fatalf("usage = %#v", response.Usage)
	}
}

func TestChatCompleteStreamsACappedSummary(t *testing.T) {
	var body chatRequest
	model := newTestModel(t, func(w http.ResponseWriter, r *http.Request) {
		raw, _ := io.ReadAll(r.Body)
		_ = json.Unmarshal(raw, &body)
		w.Header().Set("Content-Type", "text/event-stream")
		_, _ = io.WriteString(w, sse(`{"choices":[{"index":0,"delta":{"reasoning_content":"pondering"}}]}`)+
			sse(`{"choices":[{"index":0,"delta":{"content":"sum"}}]}`)+
			sse(`{"choices":[{"index":0,"delta":{"content":"mary"},"finish_reason":"stop"}]}`)+
			sse(`{"choices":[],"usage":{"prompt_tokens":50,"completion_tokens":25,"total_tokens":75}}`)+
			"data: [DONE]\n\n")
	})
	var emitted []Event
	response, err := model.Complete(context.Background(), []session.Message{session.TextMessage(session.RoleUser, "hi")}, nil, 4096, func(e Event) { emitted = append(emitted, e) })
	if err != nil {
		t.Fatal(err)
	}
	if body.MaxTokens != 4096 || !body.Stream || body.StreamOptions == nil || body.Tools != nil {
		t.Fatalf("request = %#v", body)
	}
	if want := []Event{{Text: "pondering", Thinking: true}, {Text: "sum"}, {Text: "mary"}}; !reflect.DeepEqual(emitted, want) {
		t.Fatalf("emitted = %#v", emitted)
	}
	if response.Text() != "summary" || response.Reasoning() != "pondering" || response.Finish != "stop" {
		t.Fatalf("response = %#v", response)
	}
	if response.Usage == nil || response.Usage.PromptTokens != 50 || response.Usage.CompletionTokens != 25 || response.Usage.TotalTokens != 75 {
		t.Fatalf("usage = %#v", response.Usage)
	}
}

func TestChatCompleteSendsToolsWithToolChoiceNone(t *testing.T) {
	var body []byte
	model := newTestModel(t, func(w http.ResponseWriter, r *http.Request) {
		body, _ = io.ReadAll(r.Body)
		_, _ = io.WriteString(w, okStream)
	})
	toolList := []session.ToolDefinition{{Name: "read", Description: "read a file", Parameters: json.RawMessage(`{"type":"object"}`)}}
	if _, err := model.Complete(context.Background(), []session.Message{session.TextMessage(session.RoleUser, "hi")}, toolList, 0, nil); err != nil {
		t.Fatal(err)
	}
	assertWireJSON(t, body, `{
		"model": "test-model",
		"messages": [{"role": "user", "content": "hi"}],
		"tools": [{"type": "function", "function": {"name": "read", "description": "read a file", "parameters": {"type": "object"}}}],
		"tool_choice": "none",
		"stream": true,
		"stream_options": {"include_usage": true}
	}`)
}

func TestChatCompleteOmitsToolsWhenEmpty(t *testing.T) {
	var body chatRequest
	model := newTestModel(t, func(w http.ResponseWriter, r *http.Request) {
		raw, _ := io.ReadAll(r.Body)
		_ = json.Unmarshal(raw, &body)
		_, _ = io.WriteString(w, okStream)
	})
	if _, err := model.Complete(context.Background(), []session.Message{session.TextMessage(session.RoleUser, "hi")}, nil, 0, nil); err != nil {
		t.Fatal(err)
	}
	if body.Tools != nil || body.ToolChoice != "" {
		t.Fatalf("request = %#v", body)
	}
}

func TestChatCompleteRetriesWithMaxCompletionTokens(t *testing.T) {
	attempts := 0
	var finalBody []byte
	model := newTestModel(t, func(w http.ResponseWriter, r *http.Request) {
		var body map[string]any
		raw, _ := io.ReadAll(r.Body)
		_ = json.Unmarshal(raw, &body)
		attempts++
		if _, legacy := body["max_tokens"]; legacy {
			w.WriteHeader(http.StatusBadRequest)
			_, _ = io.WriteString(w, `{"error":{"message":"Unsupported parameter: 'max_tokens' is not supported with this model. Use 'max_completion_tokens' instead.","type":"invalid_request_error"}}`)
			return
		}
		finalBody = raw
		_, _ = io.WriteString(w, okStream)
	})
	response, err := model.Complete(context.Background(), []session.Message{session.TextMessage(session.RoleUser, "hi")}, nil, 4096, nil)
	if err != nil {
		t.Fatal(err)
	}
	if attempts != 2 {
		t.Fatalf("attempts = %d", attempts)
	}
	assertWireJSON(t, finalBody, `{"model": "test-model", "messages": [{"role": "user", "content": "hi"}], "max_completion_tokens": 4096, "stream": true, "stream_options": {"include_usage": true}}`)
	if response.Text() != "ok" {
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
	if response.Text() != "hi" {
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
	if response.Text() != "hi" || len(response.ToolCalls()) != 0 {
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
	if len(response.ToolCalls()) != 2 {
		t.Fatalf("tool calls = %#v", response.ToolCalls())
	}
	if response.ToolCalls()[0].ID != "call-a" || response.ToolCalls()[0].Function.Name != "read" || string(response.ToolCalls()[0].Function.Arguments) != `{"path":"a"}` {
		t.Fatalf("first call = %#v", response.ToolCalls()[0])
	}
	if response.ToolCalls()[1].ID != "call-b" || response.ToolCalls()[1].Function.Name != "shell" || string(response.ToolCalls()[1].Function.Arguments) != `{"command":"ls"}` {
		t.Fatalf("second call = %#v", response.ToolCalls()[1])
	}
}

func TestToChatMessagesKeepsEmptyToolContent(t *testing.T) {
	messages, err := toChatMessages([]session.Message{
		session.TextMessage(session.RoleUser, "hi"),
		{Role: session.RoleAssistant, Parts: []session.Part{{Type: session.PartToolCall, ToolCallID: "1", ToolName: "read", ToolInput: json.RawMessage(`{}`)}}},
		session.ToolResultMessage("1", "read", ""),
	}, chatReplay{}, nil)
	if err != nil {
		t.Fatal(err)
	}
	if len(messages) != 3 {
		t.Fatalf("messages = %#v", messages)
	}
	encoded, err := json.Marshal(messages[2])
	if err != nil {
		t.Fatal(err)
	}
	// Tool results must always carry a content field, even when empty.
	assertWireJSON(t, encoded, `{"role": "tool", "tool_call_id": "1", "content": ""}`)
}

func TestToChatMessagesRejectsUnknownRole(t *testing.T) {
	// A role this wire format cannot send must fail the request instead of
	// silently truncating the conversation.
	if _, err := toChatMessages([]session.Message{session.TextMessage("hyper", "hi")}, chatReplay{}, nil); err == nil {
		t.Fatal("unknown role was accepted")
	}
}

func TestChatStreamPreservesPartOrder(t *testing.T) {
	stream := sse(`{"choices":[{"index":0,"delta":{"content":"first"}}]}`) +
		sse(`{"choices":[{"index":0,"delta":{"reasoning_content":"thought"}}]}`) +
		sse(`{"choices":[{"index":0,"delta":{"tool_calls":[{"index":0,"id":"call-1","function":{"name":"read","arguments":"{\"path\":"}}]}}]}`) +
		sse(`{"choices":[{"index":0,"delta":{"content":"last"}}]}`) +
		sse(`{"choices":[{"index":0,"delta":{"tool_calls":[{"index":0,"function":{"arguments":"\"a\"}"}}]},"finish_reason":"tool_calls"}]}`) +
		"data: [DONE]\n\n"
	response, err := decodeChatStream(strings.NewReader(stream), nil)
	if err != nil {
		t.Fatal(err)
	}
	want := []string{session.PartText, session.PartReasoning, session.PartToolCall, session.PartText}
	if len(response.Parts) != len(want) {
		t.Fatalf("parts = %#v", response.Parts)
	}
	for i, kind := range want {
		if response.Parts[i].Type != kind {
			t.Fatalf("part order = %#v", response.Parts)
		}
	}
	if response.Parts[2].ToolCallID != "call-1" || string(response.Parts[2].ToolInput) != `{"path":"a"}` || response.Text() != "firstlast" {
		t.Fatalf("assembled parts = %#v", response.Parts)
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
	if response.Text() != "half an ans" || response.Reasoning() != "thinking..." {
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
	if response.Text() != "started" {
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
