// Package provider owns model access. kon's durable conversation format
// (internal/session) is the single source of truth: a backend implements
// Model to map it onto one wire protocol, and Client turns the neutral result
// back into durable messages. The wire formats kon accepts, and what each
// implies, are listed in the wire subpackage. OpenAI chat completions and its
// dialects are implemented in chat.go, Anthropic's Messages API in messages.go.
//
// Which service a connection reaches, and how /login sets one up, belongs to
// internal/login; this package only learns how to talk to it, through Spec.
package provider

import (
	"context"
	"errors"
	"fmt"
	"strings"

	"github.com/hizkifw/kon/internal/provider/wire"
	"github.com/hizkifw/kon/internal/session"
	"github.com/hizkifw/kon/internal/tokens"
	"github.com/hizkifw/kon/internal/typedid"
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
	// Vision gates whether image parts are sent; see imageReader.
	Vision bool
	// Reasoning marks a model that produces reasoning, which some servers hold
	// to stricter rules for replayed history.
	Reasoning bool
	// ReasoningEffort is the level to request, or "" for the server default.
	ReasoningEffort string
}

// Client drives one configured model. It is the kon-facing half of the
// abstraction: everything above this package only ever sees session messages.
type Client struct {
	model   Model
	modelID typedid.ModelID
}

// New builds the client for a resolved model.
func New(spec Spec, readImage func(string) ([]byte, error)) (*Client, error) {
	if strings.TrimSpace(spec.ModelID) == "" {
		return nil, fmt.Errorf("model %q has no model ID", spec.Name)
	}
	model, err := newModel(spec, readImage)
	if err != nil {
		return nil, err
	}
	return &Client{model: model, modelID: typedid.ExternalModelID(spec.ModelID)}, nil
}

// newModel builds the backend for a spec's wire format: the Messages API for
// Anthropic, and chat completions for every other format in the wire table.
func newModel(spec Spec, readImage func(string) ([]byte, error)) (Model, error) {
	dialect, ok := wire.Lookup(spec.Format)
	if !ok {
		return nil, fmt.Errorf("unsupported wire format %q", spec.Format)
	}
	if dialect.Protocol == wire.Messages {
		return newMessagesModel(spec, dialect, readImage)
	}
	return newChatModel(spec, dialect, readImage)
}

func (c *Client) Stream(ctx context.Context, messages []session.Message, tools []session.ToolDefinition, emit func(Event)) (session.Message, error) {
	response, err := c.model.Stream(ctx, messages, tools, emit)
	if err != nil {
		return c.assistantOrPartial(response, err)
	}
	return c.assistant(response)
}

// assistantOrPartial turns a failed stream into the durable message for whatever
// the provider had already produced before the failure. A cancelled or dropped
// connection usually arrives with accumulated deltas and no error text; those
// deltas are persisted so the partial turn survives a resume and the next
// request continues from it. When nothing arrived there is no partial turn to
// keep and the original error is returned unchanged.
func (c *Client) assistantOrPartial(response Response, err error) (session.Message, error) {
	if response.Text() == "" && !hasReasoning(response.Parts) {
		return session.Message{}, err
	}
	// An aborted stream has no finish reason and its usage is incomplete; both
	// are omitted so the partial turn is not mistaken for a completed one.
	// Tool calls are dropped because the aborted turn never executes them and a
	// replay without their results would be rejected by the provider.
	response.Finish = ""
	response.Usage = nil
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
	message.Interrupted = true
	// The partial message is returned alongside the original error so callers
	// can persist what arrived and still surface the interruption.
	return message, err
}

// ErrOutputLimit reports a completion that reached its token limit before any
// answer, as when reasoning spends the whole budget.
var ErrOutputLimit = errors.New("response reached its token limit before any answer")

func (c *Client) Complete(ctx context.Context, messages []session.Message, tools []session.ToolDefinition, maxTokens tokens.Count) (session.Message, error) {
	response, err := c.model.Complete(ctx, messages, tools, maxTokens)
	if err != nil {
		return session.Message{}, err
	}
	if response.Finish == session.FinishLength && response.Text() == "" && len(response.ToolCalls()) == 0 {
		return session.Message{}, ErrOutputLimit
	}
	return c.assistant(response)
}

// assistant converts a neutral response into the durable assistant message.
// Content and ToolCalls feed the wire mapping directly; Parts additionally
// preserve the reasoning trace and the original ordering so future formats
// and tooling can replay the turn exactly.
func (c *Client) assistant(response Response) (session.Message, error) {
	return c.buildAssistant(response, false)
}

// buildAssistant assembles the durable assistant message. When partial is true
// an otherwise-empty response is allowed as long as it carries reasoning: an
// interrupted turn can hold reasoning with no answer text yet.
func (c *Client) buildAssistant(response Response, partial bool) (session.Message, error) {
	if response.Text() == "" && len(response.ToolCalls()) == 0 && !(partial && hasReasoning(response.Parts)) {
		return session.Message{}, errors.New("provider returned an empty assistant message")
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
