// Package provider owns model access. kon's durable conversation format
// (internal/session) is the single source of truth: a backend implements
// Model to map it onto one provider wire format, and Client turns the neutral
// result back into durable messages. The OpenAI chat completions format is
// implemented in chat.go; add a backend beside it and register it in newModel
// to support another wire format.
package provider

import (
	"context"
	"errors"
	"fmt"

	"github.com/hizkifw/kon/internal/config"
	"github.com/hizkifw/kon/internal/session"
	"github.com/hizkifw/kon/internal/typedid"
)

// Part types persisted on assistant messages for exact replay of a turn.
const (
	PartText      = "text"
	PartReasoning = "reasoning"
	PartToolCall  = "tool_call"
)

// Client drives one configured model. It is the kon-facing half of the
// abstraction: everything above this package only ever sees session messages.
type Client struct {
	model   Model
	modelID typedid.ModelID
}

func (c *Client) ModelID() typedid.ModelID { return c.modelID }

// New builds the client for a configured model profile.
func New(profile config.Model) (*Client, error) {
	if err := profile.Ready(); err != nil {
		return nil, err
	}
	model, err := newModel(profile)
	if err != nil {
		return nil, err
	}
	return &Client{model: model, modelID: typedid.ExternalModelID(profile.ModelID)}, nil
}

// newModel builds the backend for a profile. Add a case when a new wire
// format lands.
func newModel(profile config.Model) (Model, error) {
	switch profile.Provider {
	case "openai", "openai-compatible", "openrouter", "ollama":
		return newChatModel(profile), nil
	default:
		return nil, fmt.Errorf("unsupported provider %q", profile.Provider)
	}
}

func (c *Client) Stream(ctx context.Context, messages []session.Message, tools []Tool, emit func(Event)) (session.Message, error) {
	response, err := c.model.Stream(ctx, messages, tools, emit)
	if err != nil {
		return session.Message{}, err
	}
	return c.assistant(response)
}

func (c *Client) Complete(ctx context.Context, messages []session.Message, tools []Tool, maxTokens int) (session.Message, error) {
	response, err := c.model.Complete(ctx, messages, tools, maxTokens)
	if err != nil {
		return session.Message{}, err
	}
	return c.assistant(response)
}

// assistant converts a neutral response into the durable assistant message.
// Content and ToolCalls feed the wire mapping directly; Parts additionally
// preserve the reasoning trace and the original ordering so future formats
// and tooling can replay the turn exactly.
func (c *Client) assistant(response Response) (session.Message, error) {
	if response.Text == "" && len(response.ToolCalls) == 0 {
		return session.Message{}, errors.New("provider returned an empty assistant message")
	}
	message := session.Message{
		Role:    session.RoleAssistant,
		Content: response.Text,
		Model:   c.modelID,
		Finish:  response.Finish,
		Usage:   response.Usage,
	}
	if response.Reasoning != "" {
		message.Parts = append(message.Parts, session.Part{Type: PartReasoning, Text: response.Reasoning})
	}
	if response.Text != "" {
		message.Parts = append(message.Parts, session.Part{Type: PartText, Text: response.Text})
	}
	for _, call := range response.ToolCalls {
		id := typedid.ExternalToolCallID(call.ID)
		message.ToolCalls = append(message.ToolCalls, session.ToolCall{ID: id, Type: "function", Function: session.ToolFunction{Name: call.Name, Arguments: call.Arguments}})
		message.Parts = append(message.Parts, session.Part{Type: PartToolCall, ToolCallID: id, ToolName: call.Name, ToolInput: call.Arguments})
	}
	return message, nil
}
