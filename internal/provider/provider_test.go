package provider

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/http"
	"reflect"
	"testing"

	"github.com/hizkifw/kon/internal/session"
	"github.com/hizkifw/kon/internal/typedid"
)

// TestAssistantAssemblesDurableMessage checks what the client adds to a
// response, the role and the model that wrote it, and that everything else
// passes through as the backend assembled it.
func TestAssistantAssemblesDurableMessage(t *testing.T) {
	client := &Client{modelID: typedid.ExternalModelID("gpt-4o")}
	response := Response{
		Parts: []session.Part{
			{Type: session.PartText, Text: "checking"},
			{Type: session.PartReasoning, Text: "thought", ProviderOptions: json.RawMessage(`{"signature":"opaque-value"}`)},
			{Type: session.PartToolCall, ToolCallID: "call-1", ToolName: "read", ToolInput: json.RawMessage(`{"path":"x"}`)},
		},
		Finish:          "tool_calls",
		Usage:           &session.Usage{PromptTokens: 10, CompletionTokens: 5, TotalTokens: 15, CachedTokens: 4},
		ProviderOptions: json.RawMessage(`{"reasoning_field":"reasoning"}`),
	}
	message, err := client.assistant(response)
	if err != nil {
		t.Fatal(err)
	}
	if message.Role != session.RoleAssistant || message.Model.String() != "gpt-4o" {
		t.Fatalf("role = %q, model = %q", message.Role, message.Model)
	}
	want := session.Message{Role: message.Role, Model: message.Model, Parts: response.Parts, Finish: response.Finish, Usage: response.Usage, ProviderOptions: response.ProviderOptions}
	if !reflect.DeepEqual(message, want) {
		t.Fatalf("message = %#v, want the response unchanged", message)
	}
	if err := message.Validate(); err != nil {
		t.Fatalf("assembled message is invalid: %v", err)
	}
}

func TestAssistantRejectsEmptyResponse(t *testing.T) {
	client := &Client{}
	if _, err := client.assistant(Response{}); err == nil {
		t.Fatal("empty response was accepted")
	}
	// A completed turn with reasoning but no answer text is still empty from
	// the wire's point of view and must be rejected; only an interrupted turn
	// may persist reasoning alone.
	if _, err := client.assistant(Response{Parts: []session.Part{{Type: session.PartReasoning, Text: "thinking"}}}); err == nil {
		t.Fatal("reasoning-only completed response was accepted")
	}
}

func TestAssistantOrPartialPersistsInterruptedTurn(t *testing.T) {
	client := &Client{modelID: typedid.ExternalModelID("gpt")}
	cause := errors.New("stream interrupted")
	options := json.RawMessage(`{"reasoning_field":"reasoning_text"}`)
	message, err := client.assistantOrPartial(Response{Parts: []session.Part{{Type: session.PartReasoning, Text: "hm"}, {Type: session.PartText, Text: "half"}}, Finish: "stop", Usage: &session.Usage{PromptTokens: 5}, ProviderOptions: options}, cause)
	if !errors.Is(err, cause) {
		t.Fatalf("interruption not surfaced: %v", err)
	}
	if !message.Interrupted || message.Role != session.RoleAssistant || message.Text() != "half" {
		t.Fatalf("partial message = %#v", message)
	}
	// Usage and finish are dropped so the partial is never mistaken for a
	// completed turn.
	if message.Usage != nil || message.Finish != "" {
		t.Fatalf("partial kept completion metadata: %#v", message)
	}
	// The reasoning that did arrive is sent back on the next request, so the
	// field it came in must survive the interruption.
	if string(message.ProviderOptions) != string(options) {
		t.Fatalf("partial dropped reasoning metadata: %s", message.ProviderOptions)
	}
	if len(message.Parts) != 2 || message.Parts[0].Type != session.PartReasoning || message.Parts[1].Type != session.PartText {
		t.Fatalf("partial parts = %#v", message.Parts)
	}
	if err := message.Validate(); err != nil {
		t.Fatalf("partial message is invalid: %v", err)
	}
}

// TestStreamInterruptedTurnDropsToolCalls covers a stream cut off after a
// tool call arrived. The call never runs, so it has no result, and replaying
// it unanswered would get the next request rejected; the text is kept.
func TestStreamInterruptedTurnDropsToolCalls(t *testing.T) {
	events := sse(`{"choices":[{"index":0,"delta":{"content":"let me read"}}]}`) +
		sse(`{"choices":[{"index":0,"delta":{"tool_calls":[{"index":0,"id":"call-1","type":"function","function":{"name":"read","arguments":"{\"path\":\"x\"}"}}]}}]}`)
	model := newTestModel(t, func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "text/event-stream")
		_, _ = io.WriteString(w, events)
	})
	client := &Client{model: model, modelID: typedid.ExternalModelID("test-model")}
	message, err := client.Stream(context.Background(), []session.Message{session.TextMessage(session.RoleUser, "hi")}, nil, func(Event) {})
	if err == nil {
		t.Fatal("truncated stream was reported as complete")
	}
	if !message.Interrupted || message.Text() != "let me read" {
		t.Fatalf("partial message = %#v", message)
	}
	for _, part := range message.Parts {
		if part.Type == session.PartToolCall {
			t.Fatalf("partial kept an unanswered tool call: %#v", message.Parts)
		}
	}
	if err := message.Validate(); err != nil {
		t.Fatalf("partial message is invalid: %v", err)
	}
}

func TestAssistantOrPartialKeepsReasoningOnlyTurn(t *testing.T) {
	client := &Client{modelID: typedid.ExternalModelID("gpt")}
	cause := errors.New("interrupted")
	message, err := client.assistantOrPartial(Response{Parts: []session.Part{{Type: session.PartReasoning, Text: "still thinking"}}}, cause)
	if !errors.Is(err, cause) {
		t.Fatalf("interruption not surfaced: %v", err)
	}
	if message.Role != session.RoleAssistant || !message.Interrupted || len(message.Parts) != 1 || message.Parts[0].Type != session.PartReasoning {
		t.Fatalf("reasoning-only partial = %#v", message)
	}
	if err := message.Validate(); err != nil {
		t.Fatalf("reasoning-only partial is invalid: %v", err)
	}
}

func TestAssistantOrPartialPassesThroughEmptyFailure(t *testing.T) {
	client := &Client{}
	cause := errors.New("connection refused")
	message, err := client.assistantOrPartial(Response{}, cause)
	if !errors.Is(err, cause) {
		t.Fatalf("error not surfaced: %v", err)
	}
	if message.Role != "" {
		t.Fatalf("empty failure produced a message: %#v", message)
	}
}

func TestIsContextOverflowMatchesProviderPhrasings(t *testing.T) {
	cases := []struct {
		name string
		err  error
		want bool
	}{
		{"nil", nil, false},
		{"openai code", &APIError{Status: 400, Code: "context_length_exceeded", Message: "model context"}, true},
		{"openai message", &APIError{Status: 400, Message: "This model's maximum context length is 8192 tokens"}, true},
		{"anthropic phrasing", &APIError{Status: 400, Message: "prompt is too long: 250000 tokens > 200000 maximum"}, true},
		{"request too large", &APIError{Status: http.StatusRequestEntityTooLarge, Body: "request entity too large: too many tokens"}, true},
		{"in-stream rejection", &APIError{Status: 0, Message: "context length exceeded"}, true},
		{"other bad request", &APIError{Status: 400, Code: "invalid_argument", Message: "model does not exist"}, false},
		{"rate limit", &APIError{Status: 429, Message: "maximum context length exceeded"}, false},
		{"plain error", errors.New("maximum context length"), false},
	}
	for _, testCase := range cases {
		if got := IsContextOverflow(testCase.err); got != testCase.want {
			t.Fatalf("%s: IsContextOverflow = %v, want %v", testCase.name, got, testCase.want)
		}
	}
}

func TestIsContextOverflowSurvivesWrapping(t *testing.T) {
	err := fmt.Errorf("run turn: %w", &APIError{Status: 400, Code: "context_length_exceeded"})
	if !IsContextOverflow(err) {
		t.Fatal("wrapped overflow was not detected")
	}
}

func TestRejectedFieldRequiresRejectionSignal(t *testing.T) {
	// A 400 that names the field must also signal an invalid or unknown
	// parameter; otherwise any 400 mentioning the field triggers a retry.
	cases := []struct {
		name  string
		field string
		body  string
		want  bool
	}{
		{"unknown field", "stream_options", `{"error":{"message":"Unknown field: stream_options"}}`, true},
		{"unsupported field", "max_tokens", `{"error":{"message":"Unsupported parameter: 'max_tokens' is not supported with this model. Use 'max_completion_tokens' instead."}}`, true},
		{"invalid parameter", "max_tokens", `{"error":{"message":"Invalid parameter: max_tokens"}}`, true},
		{"unrelated 400 mentioning the field", "stream_options", `{"error":{"message":"stream_options appeared after the outage window"}}`, false},
		{"field absent", "stream_options", `{"error":{"message":"Unknown field: temperature"}}`, false},
	}
	for _, testCase := range cases {
		err := &APIError{Status: http.StatusBadRequest, Body: testCase.body}
		if got := rejectedField(err, testCase.field); got != testCase.want {
			t.Fatalf("%s: rejectedField = %v, want %v", testCase.name, got, testCase.want)
		}
	}
	if rejectedField(&APIError{Status: http.StatusInternalServerError, Body: "Unknown field: stream_options"}, "stream_options") {
		t.Fatal("non-400 triggered field retry")
	}
}

func TestAPIErrorMessage(t *testing.T) {
	structured := &APIError{Status: 400, Message: "bad model"}
	if got := structured.Error(); got != "provider returned status 400: bad model" {
		t.Fatalf("Error() = %q", got)
	}
	raw := &APIError{Status: 500, Body: " upstream exploded \n"}
	if got := raw.Error(); got != "provider returned status 500: upstream exploded" {
		t.Fatalf("Error() = %q", got)
	}
}
