// Package provider connects to language models. A Client is built from a
// Spec naming a wire format, an endpoint, and a model; it sends the durable
// conversation (core/session) to the model, retries transient failures, and
// turns the answer back into durable messages. The wire formats it speaks,
// and what each implies, are listed in the wire subpackage: OpenAI chat
// completions and its dialects, OpenAI Responses, and Anthropic's Messages API.
//
// A Spec is resolved by the caller: this package never reads configuration
// or decides which service a connection reaches, only how to talk to it.
package provider

import (
	"context"
	"errors"
	"fmt"
	"strings"

	"kon.kitsu.red/core/provider/wire"
	"kon.kitsu.red/core/session"
	"kon.kitsu.red/core/tokens"
	"kon.kitsu.red/core/typedid"
)

// Spec is a model resolved for use: where it is, how to talk to it, and what
// it can do. The runtime builds it from the user's profile, catalog metadata,
// and the chosen reasoning effort, so this package never reads configuration.
type Spec struct {
	// Name is the profile name, used only to identify the model in errors.
	Name    string
	Format  wire.Format
	ModelID string
	BaseURL string
	APIKey  string
	Headers map[string]string
	// UserAgent identifies the program to the server, or "" for Go's default.
	UserAgent string
	// Vision gates whether image parts are sent. Without it, stored images
	// go out as text placeholders, so a conversation that switches to a
	// model without vision can continue.
	Vision bool
	// Reasoning marks a model that produces reasoning, which some servers hold
	// to stricter rules for replayed history.
	Reasoning bool
	// ReasoningEffort is the level to request, or "" for the server default.
	ReasoningEffort string
	// Pricing prices each response the model returns.
	Pricing Pricing
}

// Pricing is what a model charges, in US dollars per million tokens. The zero
// value means the price is unknown, and responses are left unpriced.
type Pricing struct {
	Input, Output, CacheRead, CacheWrite float64
}

// Cost prices one response's usage in US dollars. Input read from or written
// to a prompt cache is billed at the cache rates. A cache rate left at zero
// falls back to the input rate, so an incomplete price list overstates a cost
// rather than silently dropping part of it.
func (p Pricing) Cost(u session.Usage) float64 {
	read, write := p.CacheRead, p.CacheWrite
	if read == 0 {
		read = p.Input
	}
	if write == 0 {
		write = p.Input
	}
	uncached := max(0, u.PromptTokens-u.CachedTokens-u.CacheWriteTokens)
	return (float64(uncached)*p.Input + float64(u.CachedTokens)*read +
		float64(u.CacheWriteTokens)*write + float64(u.CompletionTokens)*p.Output) / 1e6
}

// Client drives one configured model. It takes and returns session messages,
// so nothing above it sees a wire format. *Client satisfies agent.Provider.
type Client struct {
	model   backend
	modelID typedid.ModelID
	pricing Pricing
}

// New builds the client for a resolved model.
func New(spec Spec, readImage func(string) ([]byte, error)) (*Client, error) {
	if strings.TrimSpace(spec.ModelID) == "" {
		return nil, fmt.Errorf("model %q has no model ID", spec.Name)
	}
	model, err := newBackend(spec, readImage)
	if err != nil {
		return nil, err
	}
	return &Client{model: model, modelID: typedid.ExternalModelID(spec.ModelID), pricing: spec.Pricing}, nil
}

// newBackend builds the backend for a spec's wire format, chosen by its
// protocol.
func newBackend(spec Spec, readImage func(string) ([]byte, error)) (backend, error) {
	dialect, ok := wire.Lookup(spec.Format)
	if !ok {
		return nil, fmt.Errorf("unsupported wire format %q", spec.Format)
	}
	switch dialect.Protocol {
	case wire.Messages:
		return newMessagesModel(spec, dialect, readImage)
	case wire.Responses:
		return newResponsesModel(spec, dialect, readImage)
	}
	return newChatModel(spec, dialect, readImage)
}

// Stream generates one assistant message, emitting text and reasoning as it
// arrives. A response the provider ended early, by cutting it off at its
// limit, refusing, or filtering it, comes with a FinishError and whatever had
// already been produced.
func (c *Client) Stream(ctx context.Context, messages []session.Message, tools []session.ToolDefinition, emit func(Event)) (session.Message, error) {
	response, err := c.model.Stream(ctx, messages, tools, emit)
	if err == nil {
		err = finishError(response.Finish)
	}
	if err != nil {
		return c.assistantOrPartial(response, err)
	}
	return c.assistant(response)
}

// assistantOrPartial turns a failed stream into the durable message for whatever
// the provider had already produced before the failure. A cancelled or dropped
// connection usually arrives with accumulated deltas and no error text; those
// deltas are persisted so the partial turn survives a resume and the next
// request continues from it. A response the provider itself ended short
// (FinishError) is kept the same way. When nothing arrived there is no partial
// turn to keep and the original error is returned unchanged.
func (c *Client) assistantOrPartial(response generation, err error) (session.Message, error) {
	if response.Text() == "" && !hasReasoning(response.Parts) {
		return session.Message{}, err
	}
	// An aborted stream has no finish reason and its usage is incomplete; both
	// are omitted so the partial turn is not mistaken for a completed one. A
	// response the provider ended keeps both, since the server did finish it.
	// Tool calls are dropped either way: their arguments may be cut short, the
	// turn never executes them, and a replay without their results would be
	// rejected by the provider.
	var finish *FinishError
	stopped := errors.As(err, &finish)
	if !stopped {
		response.Finish = ""
		response.Usage = nil
	}
	parts := response.Parts[:0]
	for _, part := range response.Parts {
		if part.Type != session.PartToolCall {
			parts = append(parts, part)
		}
	}
	response.Parts = parts
	message, buildErr := c.buildAssistant(response, true)
	if buildErr != nil {
		return session.Message{}, err
	}
	message.Interrupted = !stopped
	// The partial message is returned alongside the original error so callers
	// can persist what arrived and still surface the interruption.
	return message, err
}

// Complete runs one capped generation whose answer is only useful whole, such
// as a compaction summary, so a response the provider ended short is an error
// rather than a partial message.
func (c *Client) Complete(ctx context.Context, messages []session.Message, tools []session.ToolDefinition, maxTokens tokens.Count, emit func(Event)) (session.Message, error) {
	response, err := c.model.Complete(ctx, messages, tools, maxTokens, emit)
	if err == nil {
		err = finishError(response.Finish)
	}
	if err != nil {
		return session.Message{}, err
	}
	return c.assistant(response)
}

// FinishError reports a response the provider ended before its answer was
// complete. Reason is the finish reason as the backend reported it.
type FinishError struct {
	Reason session.FinishReason
}

func (e *FinishError) Error() string {
	switch e.Reason {
	case session.FinishLength:
		return "response reached its token limit"
	case session.FinishRefusal:
		return "model declined to respond (reason: refusal)"
	case session.FinishContentFilter:
		return "response was stopped by the provider's content filter"
	}
	return fmt.Sprintf("provider stopped the response early (reason: %s)", e.Reason)
}

// abandonedReasons are finish reasons for a generation the server gave up on
// partway, which may pass on its own like a dropped connection: OpenRouter
// normalizes upstream failures to "error", and DeepSeek reports "aborted" and
// "insufficient_system_resource".
var abandonedReasons = map[session.FinishReason]bool{
	"error":                        true,
	"aborted":                      true,
	"insufficient_system_resource": true,
}

// finishError reports whether a finish reason leaves the answer incomplete.
// Any reason kon does not know counts as a normal finish: compatible servers
// invent harmless ones such as "eos_token", and whether the turn continues is
// decided by its tool calls, not its finish reason.
func finishError(reason session.FinishReason) error {
	switch {
	case reason == session.FinishLength, reason == session.FinishRefusal,
		reason == session.FinishContentFilter, abandonedReasons[reason]:
		return &FinishError{Reason: reason}
	}
	return nil
}

// IsOutputLimit reports a response that reached its token limit.
func IsOutputLimit(err error) bool {
	var finish *FinishError
	return errors.As(err, &finish) && finish.Reason == session.FinishLength
}

// assistant converts a neutral response into the durable assistant message.
// Content and ToolCalls feed the wire mapping directly; Parts additionally
// preserve the reasoning trace and the original ordering so future formats
// and tooling can replay the turn exactly.
func (c *Client) assistant(response generation) (session.Message, error) {
	return c.buildAssistant(response, false)
}

// buildAssistant assembles the durable assistant message. When partial is true
// an otherwise-empty response is allowed as long as it carries reasoning: an
// interrupted turn can hold reasoning with no answer text yet.
func (c *Client) buildAssistant(response generation, partial bool) (session.Message, error) {
	if response.Text() == "" && len(response.ToolCalls()) == 0 && !(partial && hasReasoning(response.Parts)) {
		return session.Message{}, errors.New("provider returned an empty assistant message")
	}
	if response.Usage != nil {
		response.Usage.Cost = c.pricing.Cost(*response.Usage)
	}
	message := session.Message{
		Role: session.RoleAssistant, Parts: response.Parts,
		Model: c.modelID, Finish: response.Finish, Usage: response.Usage,
		ProviderOptions: response.ProviderOptions,
	}
	return message, nil
}

func hasReasoning(parts []session.Part) bool {
	for _, part := range parts {
		if part.Type == session.PartReasoning && part.Text != "" {
			return true
		}
	}
	return false
}
