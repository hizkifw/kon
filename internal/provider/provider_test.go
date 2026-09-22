package provider

import (
	"encoding/json"
	"errors"
	"fmt"
	"net/http"
	"testing"

	"github.com/hizkifw/kon/internal/session"
	"github.com/hizkifw/kon/internal/typedid"
)

func TestAssistantAssemblesDurableMessage(t *testing.T) {
	client := &Client{modelID: typedid.ExternalModelID("gpt-4o")}
	response := Response{
		Text:      "checking",
		Reasoning: "let me look",
		ToolCalls: []ToolCall{{ID: "call-1", Name: "read", Arguments: json.RawMessage(`{"path":"x"}`)}},
		Finish:    "tool_calls",
		Usage:     &session.Usage{PromptTokens: 10, CompletionTokens: 5, TotalTokens: 15, CachedTokens: 4},
	}
	message, err := client.assistant(response)
	if err != nil {
		t.Fatal(err)
	}
	if message.Role != session.RoleAssistant || message.Content != "checking" || message.Model.String() != "gpt-4o" || message.Finish != "tool_calls" {
		t.Fatalf("message = %#v", message)
	}
	if message.Usage == nil || message.Usage.PromptTokens != 10 || message.Usage.CachedTokens != 4 {
		t.Fatalf("usage = %#v", message.Usage)
	}
	if len(message.ToolCalls) != 1 || message.ToolCalls[0].ID.String() != "call-1" || message.ToolCalls[0].Type != "function" {
		t.Fatalf("tool calls = %#v", message.ToolCalls)
	}
	// Reasoning is persisted first so a turn replays in generation order.
	want := []string{PartReasoning, PartText, PartToolCall}
	if len(message.Parts) != len(want) {
		t.Fatalf("parts = %#v", message.Parts)
	}
	for i, kind := range want {
		if message.Parts[i].Type != kind {
			t.Fatalf("parts = %#v", message.Parts)
		}
	}
	if message.Parts[0].Text != "let me look" || message.Parts[2].ToolCallID.String() != "call-1" || string(message.Parts[2].ToolInput) != `{"path":"x"}` {
		t.Fatalf("parts = %#v", message.Parts)
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
	if _, err := client.assistant(Response{Reasoning: "thinking"}); err == nil {
		t.Fatal("reasoning-only completed response was accepted")
	}
}

func TestAssistantOrPartialPersistsInterruptedTurn(t *testing.T) {
	client := &Client{modelID: typedid.ExternalModelID("gpt")}
	cause := errors.New("stream interrupted")
	message, err := client.assistantOrPartial(Response{Text: "half", Reasoning: "hm", Finish: "stop", Usage: &session.Usage{PromptTokens: 5}}, cause)
	if !errors.Is(err, cause) {
		t.Fatalf("interruption not surfaced: %v", err)
	}
	if !message.Interrupted || message.Role != session.RoleAssistant || message.Content != "half" {
		t.Fatalf("partial message = %#v", message)
	}
	// Usage and finish are dropped so the partial is never mistaken for a
	// completed turn.
	if message.Usage != nil || message.Finish != "" {
		t.Fatalf("partial kept completion metadata: %#v", message)
	}
	if len(message.Parts) != 2 || message.Parts[0].Type != PartReasoning || message.Parts[1].Type != PartText {
		t.Fatalf("partial parts = %#v", message.Parts)
	}
	if err := message.Validate(); err != nil {
		t.Fatalf("partial message is invalid: %v", err)
	}
}

func TestAssistantOrPartialKeepsReasoningOnlyTurn(t *testing.T) {
	client := &Client{modelID: typedid.ExternalModelID("gpt")}
	cause := errors.New("interrupted")
	message, err := client.assistantOrPartial(Response{Reasoning: "still thinking"}, cause)
	if !errors.Is(err, cause) {
		t.Fatalf("interruption not surfaced: %v", err)
	}
	if message.Role != session.RoleAssistant || !message.Interrupted || len(message.Parts) != 1 || message.Parts[0].Type != PartReasoning {
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
