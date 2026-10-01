package provider

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"

	"github.com/hizkifw/kon/core/provider/wire"
	"github.com/hizkifw/kon/core/session"
	"github.com/hizkifw/kon/core/typedid"
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
	var body []byte
	var auth string
	model := newResponsesTestModel(t, func(w http.ResponseWriter, r *http.Request) {
		auth = r.Header.Get("Authorization")
		if r.URL.Path != "/responses" {
			t.Errorf("path = %s", r.URL.Path)
		}
		body, _ = io.ReadAll(r.Body)
		_, _ = io.WriteString(w, responsesStream)
	})
	var events []Event
	response, err := model.Stream(context.Background(), []session.Message{session.TextMessage(session.RoleUser, "hi")}, nil, func(e Event) { events = append(events, e) })
	if err != nil {
		t.Fatal(err)
	}
	if auth != "Bearer sk-test" {
		t.Fatalf("auth = %q", auth)
	}
	// A stateless request must ask for the encrypted reasoning, since the
	// server keeps no copy to continue from on the next turn.
	assertWireJSON(t, body, fmt.Sprintf(`{
		"model": "gpt-test",
		"input": [{"role": "user", "content": "hi"}],
		"reasoning": {"effort": "high", "summary": "auto"},
		"include": ["reasoning.encrypted_content"],
		"prompt_cache_key": %q,
		"store": false,
		"stream": true
	}`, promptCacheKey([]session.Message{session.TextMessage(session.RoleUser, "hi")})))
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
		session.TextMessage(session.RoleUser, "go on"),
		// Interrupted turns: reasoning whose tool call was dropped, and
		// reasoning whose message never finished. The API rejects a reasoning
		// item without the item it led to, so both leave it out.
		{Role: session.RoleAssistant, Model: typedid.ExternalModelID("gpt-test"), Parts: []session.Part{
			{Type: session.PartReasoning, Text: "plan it", ProviderOptions: own(reasoningItem)},
		}},
		session.TextMessage(session.RoleUser, "again"),
		{Role: session.RoleAssistant, Model: typedid.ExternalModelID("gpt-test"), Parts: []session.Part{
			{Type: session.PartReasoning, Text: "plan it", ProviderOptions: own(reasoningItem)},
			{Type: session.PartText, Text: "half an ans"},
		}},
		session.TextMessage(session.RoleUser, "once more"),
		// A finished answer keeps its reasoning.
		{Role: session.RoleAssistant, Model: typedid.ExternalModelID("gpt-test"), Parts: []session.Part{
			{Type: session.PartReasoning, Text: "plan it", ProviderOptions: own(reasoningItem)},
			{Type: session.PartText, Text: "Hello", ProviderOptions: own(messageItem)},
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
		`{"role":"user","content":"go on"}`,
		`{"role":"user","content":"again"}`,
		`{"role":"assistant","content":"half an ans"}`,
		`{"role":"user","content":"once more"}`,
		reasoningItem,
		messageItem,
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
	response, err := model.Complete(context.Background(), []session.Message{session.TextMessage(session.RoleUser, "hi")}, nil, 16, nil)
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

// A refusal stands in for the answer, streamed and kept like text, so the user
// sees why the model declined instead of an empty reply.
func TestResponsesRefusal(t *testing.T) {
	refusal := `{"id":"msg_1","type":"message","role":"assistant","status":"completed","content":[{"type":"refusal","refusal":"I can't help with that."}]}`
	model := newResponsesTestModel(t, func(w http.ResponseWriter, r *http.Request) {
		_, _ = io.WriteString(w, sse(`{"type":"response.output_item.added","output_index":0,"item":{"id":"msg_1","type":"message","content":[]}}`)+
			sse(`{"type":"response.refusal.delta","output_index":0,"delta":"I can't help with that."}`)+
			sse(`{"type":"response.output_item.done","output_index":0,"item":`+refusal+`}`)+
			sse(`{"type":"response.completed","response":{}}`))
	})
	var streamed strings.Builder
	response, err := model.Stream(context.Background(), []session.Message{session.TextMessage(session.RoleUser, "hi")}, nil, func(e Event) { streamed.WriteString(e.Text) })
	if err != nil || response.Text() != "I can't help with that." || response.Finish != "refusal" || streamed.String() != response.Text() {
		t.Fatalf("response = %#v, streamed = %q, err = %v", response, streamed.String(), err)
	}
}

// Summary parts are paragraphs while they stream, as they are once finished.
func TestResponsesSeparatesSummaryParts(t *testing.T) {
	item := `{"id":"rs_1","type":"reasoning","summary":[{"type":"summary_text","text":"First."},{"type":"summary_text","text":"Second."}],"encrypted_content":"enc=="}`
	model := newResponsesTestModel(t, func(w http.ResponseWriter, r *http.Request) {
		_, _ = io.WriteString(w, sse(`{"type":"response.output_item.added","output_index":0,"item":{"id":"rs_1","type":"reasoning","summary":[]}}`)+
			sse(`{"type":"response.reasoning_summary_part.added","output_index":0,"summary_index":0}`)+
			sse(`{"type":"response.reasoning_summary_text.delta","output_index":0,"summary_index":0,"delta":"First."}`)+
			sse(`{"type":"response.reasoning_summary_part.added","output_index":0,"summary_index":1}`)+
			sse(`{"type":"response.reasoning_summary_text.delta","output_index":0,"summary_index":1,"delta":"Second."}`)+
			sse(`{"type":"response.output_item.done","output_index":0,"item":`+item+`}`)+
			sse(`{"type":"response.completed","response":{}}`))
	})
	var thinking strings.Builder
	response, err := model.Stream(context.Background(), []session.Message{session.TextMessage(session.RoleUser, "hi")}, nil, func(e Event) { thinking.WriteString(e.Text) })
	if err != nil || thinking.String() != "First.\n\nSecond." || response.Reasoning() != thinking.String() {
		t.Fatalf("streamed %q, finished %q, err = %v", thinking.String(), response.Reasoning(), err)
	}
}

// A rejected summary or cache key is left out from then on, and the request
// retried without it.
func TestResponsesLearnsFromRejections(t *testing.T) {
	var bodies []map[string]json.RawMessage
	model := newResponsesTestModel(t, func(w http.ResponseWriter, r *http.Request) {
		var body map[string]json.RawMessage
		_ = json.NewDecoder(r.Body).Decode(&body)
		bodies = append(bodies, body)
		switch len(bodies) {
		case 1:
			w.WriteHeader(http.StatusBadRequest)
			_, _ = io.WriteString(w, `{"error":{"message":"Your organization must be verified to generate reasoning summaries.","type":"invalid_request_error","param":"reasoning.summary","code":"unsupported_value"}}`)
		case 2:
			w.WriteHeader(http.StatusBadRequest)
			_, _ = io.WriteString(w, `{"error":{"message":"Unrecognized request argument supplied: prompt_cache_key","type":"invalid_request_error"}}`)
		default:
			_, _ = io.WriteString(w, sse(`{"type":"response.completed","response":{}}`))
		}
	})
	hi := []session.Message{session.TextMessage(session.RoleUser, "hi")}
	if _, err := model.Stream(context.Background(), hi, nil, nil); err != nil {
		t.Fatal(err)
	}
	if _, err := model.Stream(context.Background(), hi, nil, nil); err != nil {
		t.Fatal(err)
	}
	if len(bodies) != 4 {
		t.Fatalf("requests = %d", len(bodies))
	}
	for i, body := range bodies[2:] {
		if string(body["reasoning"]) != `{"effort":"high"}` || body["prompt_cache_key"] != nil {
			t.Fatalf("request %d after learning = %v", i+3, body)
		}
	}
}

// The cache key follows the conversation's opening, so every request of a
// session shares it and different sessions do not.
func TestPromptCacheKey(t *testing.T) {
	opening := []session.Message{session.TextMessage(session.RoleSystem, "system"), session.TextMessage(session.RoleUser, "task one")}
	later := append(append([]session.Message(nil), opening...), session.TextMessage(session.RoleAssistant, "done"), session.TextMessage(session.RoleUser, "more"))
	other := []session.Message{opening[0], session.TextMessage(session.RoleUser, "task two")}
	if key := promptCacheKey(opening); key != promptCacheKey(later) || key == promptCacheKey(other) || len(key) > 64 {
		t.Fatalf("keys: %s, %s, %s", key, promptCacheKey(later), promptCacheKey(other))
	}
}
