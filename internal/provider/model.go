package provider

import (
	"context"
	"encoding/json"

	"github.com/hizkifw/kon/internal/session"
	"github.com/hizkifw/kon/internal/tokens"
)

// Model is the seam between kon and a provider wire format. kon owns the
// durable conversation (internal/session.Message) and hands it to a Model
// verbatim; the Model maps it onto its API, streams deltas back through emit,
// and reports the response. Implement one Model per wire format and register
// it in newModel. Implementations must be safe for concurrent use and must
// return *APIError for provider-side failures so kon can classify them.
type Model interface {
	// Stream runs one streamed generation, forwarding text and reasoning
	// deltas through emit as they arrive, and returns the assembled response.
	Stream(ctx context.Context, messages []session.Message, tools []Tool, emit func(Event)) (Response, error)
	// Complete runs one non-streamed generation. maxTokens caps the completion
	// when positive; zero means the provider default. tools carries the live
	// tool roster so a one-shot request (such as a compaction summary) keeps the
	// same cached prefix as the streaming turn; a Model that receives tools must
	// forbid tool calls in its response.
	Complete(ctx context.Context, messages []session.Message, tools []Tool, maxTokens tokens.Count) (Response, error)
}

// Tool is a tool definition advertised to the model.
type Tool struct {
	Name, Description string
	Parameters        json.RawMessage
}

// Event is a streaming delta. Thinking marks reasoning the model produced
// before its answer; it is display-only and never part of the response text.
type Event struct {
	Text     string
	Thinking bool
}

// Response is a provider-neutral generation result. Finish carries the
// provider's own finish reason. Usage is the server's own token report, with
// PromptTokens covering every input token, cached or not.
type Response struct {
	Parts  []session.Part
	Finish session.FinishReason
	Usage  *session.Usage
}

func (r Response) Text() string { return (session.Message{Parts: r.Parts}).Text() }

func (r Response) ToolCalls() []session.ToolCall {
	return (session.Message{Parts: r.Parts}).ToolCalls()
}

func (r Response) Reasoning() string {
	var text string
	for _, part := range r.Parts {
		if part.Type == session.PartReasoning {
			text += part.Text
		}
	}
	return text
}
