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

func newResponsesTestModel(t *testing.T, handler http.HandlerFunc) *responsesModel {
	t.Helper()
	server := httptest.NewServer(handler)
	t.Cleanup(server.Close)
	spec, _ := wire.Lookup(wire.OpenAIResponses)
	return &responsesModel{client: server.Client(), baseURL: server.URL, apiKey: "sk-test", model: "gpt-test", spec: spec, reasoning: true, effort: "high"}
}

const (
	reasoningItem = `{"id":"rs_1","type":"reasoning","summary":[{"type":"summary_text","text":"plan it"}],"encrypted_content":"enc=="}`
	messageItem   = `{"id":"msg_1","type":"message","role":"assistant","status":"completed","content":[{"type":"output_text","text":"Hello","annotations":[]}]}`
	callItem      = `{"id":"fc_1","type":"function_call","status":"completed","call_id":"call_1","name":"read","arguments":"{\"path\":\"a.go\"}"}`
)

var responsesStream = strings.Join([]string{
	sse(`{"type":"response.created","response":{}}`),
	sse(`{"type":"response.output_item.added","output_index":0,"item":{"id":"rs_1","type":"reasoning","summary":[]}}`),
	sse(`{"type":"response.reasoning_summary_text.delta","output_index":0,"delta":"plan "}`),
	sse(`{"type":"response.reasoning_summary_text.delta","output_index":0,"delta":"it"}`),
	sse(`{"type":"response.output_item.done","output_index":0,"item":` + reasoningItem + `}`),
	sse(`{"type":"response.output_item.added","output_index":1,"item":{"id":"msg_1","type":"message","content":[]}}`),
	sse(`{"type":"response.output_text.delta","output_index":1,"delta":"Hello"}`),
	sse(`{"type":"response.output_item.done","output_index":1,"item":` + messageItem + `}`),
	sse(`{"type":"response.output_item.added","output_index":2,"item":{"id":"fc_1","type":"function_call","call_id":"call_1","name":"read","arguments":""}}`),
	sse(`{"type":"response.function_call_arguments.delta","output_index":2,"delta":"{\"path\":"}`),
	sse(`{"type":"response.output_item.done","output_index":2,"item":` + callItem + `}`),
	sse(`{"type":"response.completed","response":{"usage":{"input_tokens":330,"input_tokens_details":{"cached_tokens":300},"output_tokens":42,"total_tokens":372}}}`),
}, "")

func TestResponsesStreamAssemblesItems(t *testing.T) {
	var body responsesRequest
	var auth string
	model := newResponsesTestModel(t, func(w http.ResponseWriter, r *http.Request) {
		auth = r.Header.Get("Authorization")
		if r.URL.Path != "/responses" {
			t.Errorf("path = %s", r.URL.Path)
		}
		_ = json.NewDecoder(r.Body).Decode(&body)
		_, _ = io.WriteString(w, responsesStream)
	})
	var events []Event
	response, err := model.Stream(context.Background(), []session.Message{session.TextMessage(session.RoleUser, "hi")}, nil, func(e Event) { events = append(events, e) })
	if err != nil {
		t.Fatal(err)
	}
	if auth != "Bearer sk-test" || body.Store || !body.Stream || body.Reasoning == nil || body.Reasoning.Effort != "high" || body.Reasoning.Summary != "auto" || len(body.Include) != 1 {
		t.Fatalf("auth = %q, request = %#v", auth, body)
	}
	if len(response.Parts) != 3 {
		t.Fatalf("parts = %#v", response.Parts)
	}
	reasoning, text, call := response.Parts[0], response.Parts[1], response.Parts[2]
	if reasoning.Type != session.PartReasoning || reasoning.Text != "plan it" || !strings.Contains(string(decodeItemOptions(reasoning.ProviderOptions).Item), "enc==") {
		t.Fatalf("reasoning = %#v", reasoning)
	}
	if text.Text != "Hello" || call.ToolCallID.String() != "call_1" || string(call.ToolInput) != `{"path":"a.go"}` {
		t.Fatalf("text = %#v, call = %#v", text, call)
	}
	if response.Finish != "tool_calls" || response.Usage == nil || response.Usage.PromptTokens != 330 || response.Usage.CachedTokens != 300 {
		t.Fatalf("finish = %q, usage = %#v", response.Finish, response.Usage)
	}
	if len(events) != 3 || !events[0].Thinking || events[2].Text != "Hello" {
		t.Fatalf("events = %#v", events)
	}
}

// TestResponsesReplaysOwnItemsVerbatim covers the stateless contract: this
// model's items go back exactly as received, another model's reasoning is
// left out, and everything else is rebuilt.
func TestResponsesReplaysOwnItemsVerbatim(t *testing.T) {
	own := func(raw string) json.RawMessage {
		options, _ := json.Marshal(itemOptions{Item: json.RawMessage(raw)})
		return options
	}
	conversation := []session.Message{
		session.TextMessage(session.RoleSystem, "system prompt"),
		session.TextMessage(session.RoleUser, "task"),
		{Role: session.RoleAssistant, Model: typedid.ExternalModelID("gpt-test"), Parts: []session.Part{
			{Type: session.PartReasoning, Text: "plan it", ProviderOptions: own(reasoningItem)},
			{Type: session.PartToolCall, ToolCallID: typedid.ExternalToolCallID("call_1"), ToolName: "read", ToolInput: json.RawMessage(`{"path":"a.go"}`), ProviderOptions: own(callItem)},
		}},
		session.ToolResultMessage(typedid.ExternalToolCallID("call_1"), "read", "package a"),
		{Role: session.RoleAssistant, Model: typedid.ExternalModelID("other-model"), Parts: []session.Part{
			{Type: session.PartReasoning, Text: "someone else's", ProviderOptions: own(reasoningItem)},
			{Type: session.PartText, Text: "done"},
		}},
	}
	var raw map[string]json.RawMessage
	model := newResponsesTestModel(t, func(w http.ResponseWriter, r *http.Request) {
		_ = json.NewDecoder(r.Body).Decode(&raw)
		_, _ = io.WriteString(w, sse(`{"type":"response.completed","response":{}}`))
	})
	if _, err := model.Stream(context.Background(), conversation, nil, nil); err != nil {
		t.Fatal(err)
	}
	var input []json.RawMessage
	if err := json.Unmarshal(raw["input"], &input); err != nil {
		t.Fatal(err)
	}
	want := []string{
		`{"role":"system","content":"system prompt"}`,
		`{"role":"user","content":"task"}`,
		reasoningItem,
		callItem,
		`{"type":"function_call_output","call_id":"call_1","output":"package a"}`,
		`{"role":"assistant","content":"done"}`,
	}
	if len(input) != len(want) {
		t.Fatalf("input = %s", raw["input"])
	}
	for i := range want {
		if string(input[i]) != want[i] {
			t.Fatalf("input[%d] = %s, want %s", i, input[i], want[i])
		}
	}
}

func TestResponsesIncompleteAndFailed(t *testing.T) {
	model := newResponsesTestModel(t, func(w http.ResponseWriter, r *http.Request) {
		_, _ = io.WriteString(w, sse(`{"type":"response.output_item.added","output_index":0,"item":{"type":"message"}}`)+
			sse(`{"type":"response.output_text.delta","output_index":0,"delta":"trunc"}`)+
			sse(`{"type":"response.incomplete","response":{"incomplete_details":{"reason":"max_output_tokens"}}}`))
	})
	response, err := model.Complete(context.Background(), []session.Message{session.TextMessage(session.RoleUser, "hi")}, nil, 16)
	if err != nil || response.Finish != session.FinishLength || response.Text() != "trunc" {
		t.Fatalf("incomplete = %#v, %v", response, err)
	}

	model = newResponsesTestModel(t, func(w http.ResponseWriter, r *http.Request) {
		_, _ = io.WriteString(w, sse(`{"type":"response.failed","response":{"error":{"code":"context_length_exceeded","message":"Your input exceeds the context window of this model."}}}`))
	})
	if _, err := model.Stream(context.Background(), []session.Message{session.TextMessage(session.RoleUser, "hi")}, nil, nil); !IsContextOverflow(err) {
		t.Fatalf("failed response error = %v, want a context overflow", err)
	}
}
