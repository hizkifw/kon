// Package provider adapts goai models to kon's durable message format.
package provider

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"

	"github.com/hizkifw/kon/internal/config"
	"github.com/hizkifw/kon/internal/session"
	"github.com/hizkifw/kon/internal/typedid"
	"github.com/zendev-sh/goai"
	goaiprovider "github.com/zendev-sh/goai/provider"
	"github.com/zendev-sh/goai/provider/anthropic"
	"github.com/zendev-sh/goai/provider/compat"
	"github.com/zendev-sh/goai/provider/google"
	"github.com/zendev-sh/goai/provider/ollama"
	"github.com/zendev-sh/goai/provider/openai"
	"github.com/zendev-sh/goai/provider/openrouter"
)

type Tool struct {
	Name, Description string
	Parameters        json.RawMessage
}

// Event is a streaming text delta. Thinking marks reasoning the model produced
// before its answer; it is display-only and never part of the response text.
type Event struct {
	Text     string
	Thinking bool
}

type Client struct {
	model   goaiprovider.LanguageModel
	modelID typedid.ModelID
}

func (c *Client) ModelID() typedid.ModelID { return c.modelID }

func New(profile config.Model) (*Client, error) {
	if err := profile.Ready(); err != nil {
		return nil, err
	}
	key := profile.APIKey
	var model goaiprovider.LanguageModel
	switch profile.Provider {
	case "openai":
		var opts []openai.Option
		if key != "" {
			opts = append(opts, openai.WithAPIKey(key))
		}
		if profile.BaseURL != "" {
			opts = append(opts, openai.WithBaseURL(profile.BaseURL))
		}
		if len(profile.Headers) > 0 {
			opts = append(opts, openai.WithHeaders(profile.Headers))
		}
		model = openai.Chat(profile.ModelID, opts...)
	case "anthropic":
		var opts []anthropic.Option
		if key != "" {
			opts = append(opts, anthropic.WithAPIKey(key))
		}
		if profile.BaseURL != "" {
			opts = append(opts, anthropic.WithBaseURL(profile.BaseURL))
		}
		if len(profile.Headers) > 0 {
			opts = append(opts, anthropic.WithHeaders(profile.Headers))
		}
		model = anthropic.Chat(profile.ModelID, opts...)
	case "google":
		var opts []google.Option
		if key != "" {
			opts = append(opts, google.WithAPIKey(key))
		}
		if profile.BaseURL != "" {
			opts = append(opts, google.WithBaseURL(profile.BaseURL))
		}
		if len(profile.Headers) > 0 {
			opts = append(opts, google.WithHeaders(profile.Headers))
		}
		model = google.Chat(profile.ModelID, opts...)
	case "openrouter":
		var opts []openrouter.Option
		if key != "" {
			opts = append(opts, openrouter.WithAPIKey(key))
		}
		if profile.BaseURL != "" {
			opts = append(opts, openrouter.WithBaseURL(profile.BaseURL))
		}
		if len(profile.Headers) > 0 {
			opts = append(opts, openrouter.WithHeaders(profile.Headers))
		}
		model = openrouter.Chat(profile.ModelID, opts...)
	case "ollama":
		var opts []ollama.Option
		if profile.BaseURL != "" {
			opts = append(opts, ollama.WithBaseURL(profile.BaseURL))
		}
		if len(profile.Headers) > 0 {
			opts = append(opts, ollama.WithHeaders(profile.Headers))
		}
		model = ollama.Chat(profile.ModelID, opts...)
	case "openai-compatible":
		opts := []compat.Option{compat.WithBaseURL(profile.BaseURL), compat.WithProviderID(profile.Name)}
		if key != "" {
			opts = append(opts, compat.WithAPIKey(key))
		}
		if len(profile.Headers) > 0 {
			opts = append(opts, compat.WithHeaders(profile.Headers))
		}
		model = compat.Chat(profile.ModelID, opts...)
	default:
		return nil, fmt.Errorf("unsupported provider %q", profile.Provider)
	}
	return &Client{model: model, modelID: typedid.ExternalModelID(profile.ModelID)}, nil
}

func (c *Client) Stream(ctx context.Context, messages []session.Message, tools []Tool, emit func(Event)) (session.Message, error) {
	stream, err := goai.StreamText(ctx, c.model, goai.WithMessages(toMessages(messages)...), goai.WithTools(toTools(tools)...), goai.WithMaxSteps(1))
	if err != nil {
		return session.Message{}, err
	}
	for chunk := range stream.Stream() {
		if event, ok := chunkEvent(chunk); ok {
			emit(event)
		}
	}
	result := stream.Result()
	if err := stream.Err(); err != nil {
		return session.Message{}, err
	}
	return fromResult(result, c.modelID)
}

func (c *Client) Complete(ctx context.Context, messages []session.Message, maxTokens int) (session.Message, error) {
	result, err := goai.GenerateText(ctx, c.model, goai.WithMessages(toMessages(messages)...), goai.WithMaxOutputTokens(maxTokens), goai.WithMaxSteps(1))
	if err != nil {
		return session.Message{}, err
	}
	return fromResult(result, c.modelID)
}

func IsContextOverflow(err error) bool {
	if err == nil {
		return false
	}
	var overflow *goai.ContextOverflowError
	if errors.As(err, &overflow) {
		return true
	}
	return goai.IsOverflow(err.Error())
}

// chunkEvent maps a goai stream chunk to a kon event and reports whether the
// chunk carries text. Reasoning deltas surface thinking models' intermediate
// reasoning; empty deltas are dropped.
func chunkEvent(chunk goaiprovider.StreamChunk) (Event, bool) {
	switch chunk.Type {
	case goaiprovider.ChunkText:
		return Event{Text: chunk.Text}, chunk.Text != ""
	case goaiprovider.ChunkReasoning:
		return Event{Text: chunk.Text, Thinking: true}, chunk.Text != ""
	default:
		return Event{}, false
	}
}

func toTools(definitions []Tool) []goai.Tool {
	out := make([]goai.Tool, 0, len(definitions))
	for _, tool := range definitions {
		out = append(out, goai.Tool{Name: tool.Name, Description: tool.Description, InputSchema: tool.Parameters})
	}
	return out
}

func toMessages(messages []session.Message) []goaiprovider.Message {
	out := make([]goaiprovider.Message, 0, len(messages))
	for _, message := range messages {
		converted := goaiprovider.Message{Role: goaiprovider.Role(message.Role), ProviderOptions: message.ProviderOptions}
		if len(message.Parts) > 0 {
			for _, part := range message.Parts {
				converted.Content = append(converted.Content, goaiprovider.Part{Type: goaiprovider.PartType(part.Type), Text: part.Text, ToolCallID: part.ToolCallID.String(), ToolName: part.ToolName, ToolInput: part.ToolInput, ToolOutput: part.ToolOutput, ProviderOptions: part.ProviderOptions})
			}
		} else {
			if message.Content != "" {
				converted.Content = append(converted.Content, goaiprovider.Part{Type: goaiprovider.PartText, Text: message.Content})
			}
			for _, call := range message.ToolCalls {
				converted.Content = append(converted.Content, goaiprovider.Part{Type: goaiprovider.PartToolCall, ToolCallID: call.ID.String(), ToolName: call.Function.Name, ToolInput: call.Function.Arguments, ProviderOptions: call.Metadata})
			}
			if message.Role == session.RoleTool {
				converted.Content = []goaiprovider.Part{{Type: goaiprovider.PartToolResult, ToolCallID: message.ToolCallID.String(), ToolName: message.Name, ToolOutput: message.Content}}
			}
		}
		out = append(out, converted)
	}
	return out
}

func fromResult(result *goai.TextResult, modelID typedid.ModelID) (session.Message, error) {
	if result == nil {
		return session.Message{}, errors.New("provider returned no result")
	}
	message := session.Message{Role: session.RoleAssistant, Content: result.Text, Model: modelID, Finish: session.FinishReason(result.FinishReason), Usage: &session.Usage{PromptTokens: result.TotalUsage.InputTokens, CompletionTokens: result.TotalUsage.OutputTokens, TotalTokens: result.TotalUsage.TotalTokens}}
	if len(result.Steps) > 0 {
		message.Content = result.Steps[len(result.Steps)-1].Text
	}
	message.Parts = replayParts(result)
	for _, call := range result.ToolCalls {
		message.ToolCalls = append(message.ToolCalls, session.ToolCall{ID: typedid.ExternalToolCallID(call.ID), Type: "function", Function: session.ToolFunction{Name: call.Name, Arguments: call.Input}, Metadata: call.Metadata})
	}
	if message.Content == "" && len(message.ToolCalls) == 0 {
		return session.Message{}, errors.New("provider returned an empty assistant message")
	}
	return message, nil
}

// replayParts returns the assistant content to persist for exact replay.
// Reasoning ("thinking") parts are never part of Content and most providers
// omit them from StepResult.Content while streaming. goai assembles the
// replayable assistant message in ResponseMessages with reasoning first
// (including provider-native signatures), which providers such as Anthropic
// require when a thinking trace must accompany tool calls on continuation.
// StepResult.Content is the fallback for results without response messages.
func replayParts(result *goai.TextResult) []session.Part {
	for i := len(result.ResponseMessages) - 1; i >= 0; i-- {
		replay := result.ResponseMessages[i]
		if replay.Role != goaiprovider.RoleAssistant || len(replay.Content) == 0 {
			continue
		}
		return sessionParts(replay.Content)
	}
	if len(result.Steps) == 0 {
		return nil
	}
	return sessionParts(result.Steps[len(result.Steps)-1].Content)
}

func sessionParts(parts []goaiprovider.Part) []session.Part {
	if len(parts) == 0 {
		return nil
	}
	out := make([]session.Part, 0, len(parts))
	for _, part := range parts {
		out = append(out, session.Part{Type: string(part.Type), Text: part.Text, ToolCallID: typedid.ExternalToolCallID(part.ToolCallID), ToolName: part.ToolName, ToolInput: part.ToolInput, ToolOutput: part.ToolOutput, ProviderOptions: part.ProviderOptions})
	}
	return out
}
