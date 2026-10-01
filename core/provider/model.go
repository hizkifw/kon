package provider

import (
	"context"
	"encoding/json"

	"kon.kitsu.red/core/session"
	"kon.kitsu.red/core/tokens"
)

// Model is the seam between kon and a wire protocol. kon owns the durable
// conversation (core/session.Message) and hands it to a Model verbatim;
// the Model maps it onto its API, streams deltas back through emit, and
// reports the response. Implement one Model per protocol and choose it for
// its wire formats in newModel. Implementations must be safe for concurrent
// use and must return *APIError for server-side failures so kon can classify
// them.
type Model interface {
	// Stream runs one streamed generation, forwarding text and reasoning
	// deltas through emit as they arrive, and returns the assembled response.
	Stream(ctx context.Context, messages []session.Message, tools []session.ToolDefinition, emit func(Event)) (Response, error)
	// Complete runs one capped generation, forwarding deltas through emit
	// when it is set. maxTokens caps the completion when positive; zero means
	// the provider default. tools carries the live tool roster so a one-shot
	// request (such as a compaction summary) keeps the same cached prefix as
	// the streaming turn. It sets no tool_choice, which would invalidate that
	// cache on some providers, so the model may still call a tool; the caller
	// must not run it.
	Complete(ctx context.Context, messages []session.Message, tools []session.ToolDefinition, maxTokens tokens.Count, emit func(Event)) (Response, error)
}

// Event is a streaming delta. Thinking marks reasoning the model produced
// before its answer; it is shown as thinking and kept out of the response text.
type Event struct {
	Text     string
	Thinking bool
	// Retry, when set, is the only field: the request failed before
	// anything streamed and will be sent again.
	Retry *Retry
}

// Response is a provider-neutral generation result. Finish carries the
// provider's own finish reason. Usage is the server's own token report, with
// PromptTokens covering every input token, cached or not. ProviderOptions is
// opaque metadata the wire format needs to send the message back as it arrived.
type Response struct {
	Parts           []session.Part
	Finish          session.FinishReason
	Usage           *session.Usage
	ProviderOptions json.RawMessage
}

// Text is the response's text, as Message.Text reads it.
func (r Response) Text() string { return (session.Message{Parts: r.Parts}).Text() }

// ToolCalls is the tool calls the response makes, in part order.
func (r Response) ToolCalls() []session.ToolCall {
	return (session.Message{Parts: r.Parts}).ToolCalls()
}

// Reasoning is the response's reasoning text, in part order.
func (r Response) Reasoning() string {
	return (session.Message{Parts: r.Parts}).Reasoning()
}
