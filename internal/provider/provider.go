// Package provider owns model access. kon's durable conversation format
// (internal/session) is the single source of truth: a backend implements
// Model to map it onto one wire protocol, and Client turns the neutral result
// back into durable messages. The wire formats kon accepts, and what each
// implies, are listed in the wire subpackage; all of them are dialects of
// OpenAI chat completions, implemented in chat.go.
//
// The package also maps services onto connections for /login (registry.go)
// and checks a connection on login (discovery.go). Those are about which
// service kon talks to, not how, and are kept apart from the wire table.
package provider

import (
	"context"
	"errors"
	"fmt"

	"github.com/hizkifw/kon/internal/config"
	"github.com/hizkifw/kon/internal/provider/wire"
	"github.com/hizkifw/kon/internal/session"
	"github.com/hizkifw/kon/internal/tokens"
	"github.com/hizkifw/kon/internal/typedid"
)

// Part types persisted on assistant messages for exact replay of a turn. They
// alias the session constants so callers can use either spelling; image parts
// are produced by tools (see tools/read.go) and consumed by wire mappings.
const (
	PartText      = session.PartText
	PartReasoning = session.PartReasoning
	PartToolCall  = session.PartToolCall
	PartImage     = session.PartImage
)

// Client drives one configured model. It is the kon-facing half of the
// abstraction: everything above this package only ever sees session messages.
type Client struct {
	model   Model
	modelID typedid.ModelID
}

func (c *Client) ModelID() typedid.ModelID { return c.modelID }

// New builds the client for a configured model profile.
func New(profile config.Model, readImage func(string) ([]byte, error)) (*Client, error) {
	if err := profile.Ready(); err != nil {
		return nil, err
	}
	model, err := newModel(profile, readImage)
	if err != nil {
		return nil, err
	}
	return &Client{model: model, modelID: typedid.ExternalModelID(profile.ModelID)}, nil
}

// newModel builds the backend for a profile's wire format. Every format in the
// wire table is a chat completions dialect, so one backend serves them all; a
// format with another protocol would choose its backend here.
func newModel(profile config.Model, readImage func(string) ([]byte, error)) (Model, error) {
	spec, ok := wire.Lookup(profile.WireFormat())
	if !ok {
		return nil, fmt.Errorf("unsupported wire format %q", profile.WireFormat())
	}
	return newChatModel(profile, spec, readImage)
}

func (c *Client) Stream(ctx context.Context, messages []session.Message, tools []Tool, emit func(Event)) (session.Message, error) {
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

func (c *Client) Complete(ctx context.Context, messages []session.Message, tools []Tool, maxTokens tokens.Count) (session.Message, error) {
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
