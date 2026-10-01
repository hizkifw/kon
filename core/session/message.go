package session

import (
	"encoding/json"
	"errors"
	"fmt"
	"strings"

	"kon.kitsu.red/core/tokens"
	"kon.kitsu.red/core/typedid"
)

// Usage is what one provider response consumed, as the provider reported it.
type Usage struct {
	PromptTokens     tokens.Count `json:"prompt_tokens"`
	CompletionTokens tokens.Count `json:"completion_tokens"`
	TotalTokens      tokens.Count `json:"total_tokens"`
	// CachedTokens is the provider-reported share of PromptTokens that hit a
	// prompt cache. PromptTokens always covers every input token.
	CachedTokens tokens.Count `json:"cached_tokens,omitempty"`
	// CacheWriteTokens is the share of PromptTokens written to a prompt
	// cache, which some providers bill above the input rate.
	CacheWriteTokens tokens.Count `json:"cache_write_tokens,omitempty"`
	// Cost is what kon priced the response at, in US dollars, from the
	// model's prices when it ran. It is absent when the model had no price.
	Cost float64 `json:"cost,omitempty"`
}

// TotalUsage adds up the usage recorded on entries: assistant replies and the
// summary calls of compactions. It totals a session, its own or a subagent's,
// from what the session file already holds.
func TotalUsage(entries []Entry) Usage {
	var total Usage
	for _, entry := range entries {
		usage := entry.Usage
		if entry.Message != nil {
			usage = entry.Message.Usage
		}
		if usage != nil {
			total.Add(*usage)
		}
	}
	return total
}

// Add adds other to u.
func (u *Usage) Add(other Usage) {
	u.PromptTokens += other.PromptTokens
	u.CompletionTokens += other.CompletionTokens
	u.TotalTokens += other.TotalTokens
	u.CachedTokens += other.CachedTokens
	u.CacheWriteTokens += other.CacheWriteTokens
	u.Cost += other.Cost
}

// Role is who a message is from.
type Role string

const (
	RoleSystem    Role = "system"
	RoleUser      Role = "user"
	RoleAssistant Role = "assistant"
	RoleTool      Role = "tool"
)

// FinishReason is why a provider ended a response.
type FinishReason string

// Finish reasons kon acts on. Backends map their own spellings onto these;
// any other reason is kept as the provider sent it.
const (
	// FinishLength is a response cut off at its token limit.
	FinishLength FinishReason = "length"
	// FinishRefusal is a response the model declined to write.
	FinishRefusal FinishReason = "refusal"
	// FinishContentFilter is a response withheld by the provider's content
	// filter, possibly after part of it streamed.
	FinishContentFilter FinishReason = "content_filter"
)

// ToolFunction names the tool a call runs and carries the arguments the
// model wrote, exactly as it wrote them.
type ToolFunction struct {
	Name      string          `json:"name"`
	Arguments json.RawMessage `json:"arguments"`
}

// ToolCall is one tool call an assistant message makes, a view over its
// tool-call part. Metadata is the backend's opaque data for the call.
type ToolCall struct {
	ID       typedid.ToolCallID `json:"id"`
	Type     string             `json:"type"`
	Function ToolFunction       `json:"function"`
	Metadata json.RawMessage    `json:"metadata,omitempty"`
}

// ToolDefinition is a tool as advertised to the model. It lives here, beside
// the calls it invites, so the tool registry and the provider backends share
// it without depending on each other.
type ToolDefinition struct {
	Name, Description string
	Parameters        json.RawMessage
}

// Part preserves ordered, provider-neutral content needed for exact replay.
// ProviderOptions is opaque data owned by the backend in core/provider
// that wrote it; kon stores it without interpreting it.
type Part struct {
	Type            string             `json:"type"`
	Text            string             `json:"text,omitempty"`
	ToolCallID      typedid.ToolCallID `json:"tool_call_id,omitempty"`
	ToolName        string             `json:"tool_name,omitempty"`
	ToolInput       json.RawMessage    `json:"tool_input,omitempty"`
	ToolOutput      string             `json:"tool_output,omitempty"`
	ImageHash       string             `json:"image_hash,omitempty"`
	ImageMIME       string             `json:"image_mime,omitempty"`
	ProviderOptions json.RawMessage    `json:"provider_options,omitempty"`
}

// Content part types. An image part references bytes beside the session file.
const (
	PartReasoning  = "reasoning"
	PartText       = "text"
	PartToolCall   = "tool_call"
	PartImage      = "image"
	PartToolResult = "tool_result"
)

// Message is provider-neutral. Parts are the only source of message content
// and retain the order and opaque metadata supplied by the provider.
type Message struct {
	Role            Role            `json:"role"`
	Parts           []Part          `json:"parts"`
	IsError         bool            `json:"is_error,omitempty"`
	Details         json.RawMessage `json:"details,omitempty"`
	Model           typedid.ModelID `json:"model,omitempty"`
	Finish          FinishReason    `json:"finish_reason,omitempty"`
	Usage           *Usage          `json:"usage,omitempty"`
	ProviderOptions json.RawMessage `json:"provider_options,omitempty"`
	// Interrupted marks an assistant message persisted from a stream that ended
	// early (user cancellation or a dropped connection) rather than a provider
	// finish reason. The partial text and reasoning are kept so the turn can be
	// replayed and continued; a completed turn leaves this false.
	Interrupted bool `json:"interrupted,omitempty"`
}

// TextMessage is a message holding only text.
func TextMessage(role Role, text string) Message {
	return Message{Role: role, Parts: []Part{{Type: PartText, Text: text}}}
}

// ToolResultMessage answers the tool call id, made to the tool name, with
// output.
func ToolResultMessage(id typedid.ToolCallID, name, output string) Message {
	return Message{Role: RoleTool, Parts: []Part{{Type: PartToolResult, ToolCallID: id, ToolName: name, ToolOutput: output}}}
}

// Text is the plain-text view used for prompts and transcript replay.
func (m Message) Text() string {
	var out strings.Builder
	for _, part := range m.Parts {
		if part.Type == PartText {
			out.WriteString(part.Text)
		} else if part.Type == PartToolResult {
			out.WriteString(part.ToolOutput)
		}
	}
	return out.String()
}

// Reasoning is the message's reasoning text, in part order.
func (m Message) Reasoning() string {
	var out strings.Builder
	for _, part := range m.Parts {
		if part.Type == PartReasoning {
			out.WriteString(part.Text)
		}
	}
	return out.String()
}

// ToolCalls is the tool calls the message makes, in part order.
func (m Message) ToolCalls() []ToolCall {
	var calls []ToolCall
	for _, part := range m.Parts {
		if part.Type == PartToolCall {
			calls = append(calls, ToolCall{ID: part.ToolCallID, Type: "function", Function: ToolFunction{Name: part.ToolName, Arguments: part.ToolInput}, Metadata: part.ProviderOptions})
		}
	}
	return calls
}

// ToolResult is the call ID and tool name a tool message answers, or zero
// values for any other message.
func (m Message) ToolResult() (typedid.ToolCallID, string) {
	for _, part := range m.Parts {
		if part.Type == PartToolResult {
			return part.ToolCallID, part.ToolName
		}
	}
	return "", ""
}

// Validate reports whether the message can be stored: content its role
// requires, unique tool call IDs, and well-formed image parts.
func (m Message) Validate() error {
	switch m.Role {
	case RoleSystem, RoleUser:
		if m.Text() == "" {
			return fmt.Errorf("%s message content must not be empty", m.Role)
		}
	case RoleAssistant:
		hasContent := false
		for _, part := range m.Parts {
			if ((part.Type == PartText || part.Type == PartReasoning) && part.Text != "") || part.Type == PartToolCall {
				hasContent = true
			}
		}
		if !hasContent {
			return errors.New("assistant message must contain text, reasoning, or tool calls")
		}
		seen := make(map[typedid.ToolCallID]bool)
		for _, call := range m.ToolCalls() {
			if call.ID.String() == "" || call.Function.Name == "" {
				return errors.New("assistant tool call requires an external ID and function name")
			}
			if seen[call.ID] {
				return fmt.Errorf("duplicate assistant tool call ID %q", call.ID)
			}
			seen[call.ID] = true
		}
	case RoleTool:
		results := 0
		for _, part := range m.Parts {
			if part.Type == PartToolResult {
				results++
			}
		}
		id, name := m.ToolResult()
		if results != 1 || id.String() == "" || name == "" {
			return errors.New("tool result requires an external tool call ID and name")
		}
	default:
		return fmt.Errorf("unknown message role %q", m.Role)
	}
	for _, part := range m.Parts {
		if part.Type == PartImage && (part.Text != "" || !validImageHash(part.ImageHash) || part.ImageMIME == "") {
			return errors.New("image part requires a blob hash and MIME type")
		}
	}
	return nil
}
