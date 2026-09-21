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
