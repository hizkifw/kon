package provider

import (
	"bufio"
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/http"
	"strconv"
	"strings"
	"time"

	"github.com/hizkifw/kon/internal/config"
	"github.com/hizkifw/kon/internal/session"
)

// chatModel implements Model for the OpenAI chat completions wire format.
// openai, openai-compatible, and openrouter speak it natively, and ollama
// serves it under /v1. kon builds and parses every message itself: request
// bodies, SSE events, tool-call deltas, and usage reports are all owned here.
type chatModel struct {
	client  *http.Client
	baseURL string
	apiKey  string
	headers map[string]string
	model   string
}

const (
	defaultOpenAIBaseURL     = "https://api.openai.com/v1"
	defaultOpenRouterBaseURL = "https://openrouter.ai/api/v1"
	defaultOllamaBaseURL     = "http://localhost:11434/v1"
)

// maxEventSize bounds one SSE line. Providers occasionally emit very long
// lines for large tool arguments; 1 MiB is far above any real chunk.
const maxEventSize = 1 << 20

// maxBodySize bounds a non-streamed response body.
const maxBodySize = 64 << 20

// completeTimeout bounds one non-streamed request, including the response
// body read. Streaming stays unbounded so long generations are never cut
// off; a hung non-streamed server would otherwise stall until the user
// cancels.
const completeTimeout = 10 * time.Minute

func newChatModel(profile config.Model) *chatModel {
	baseURL := profile.BaseURL
	switch profile.Provider {
	case "openai":
		if baseURL == "" {
			baseURL = defaultOpenAIBaseURL
		}
	case "openrouter":
		if baseURL == "" {
			baseURL = defaultOpenRouterBaseURL
		}
	case "ollama":
		// Ollama serves the chat completions format under /v1; its native API
		// is a different wire format entirely.
		if baseURL == "" {
			baseURL = defaultOllamaBaseURL
		}
		if !strings.HasSuffix(strings.TrimRight(baseURL, "/"), "/v1") {
			baseURL = strings.TrimRight(baseURL, "/") + "/v1"
		}
	}
	return &chatModel{
		client:  &http.Client{},
		baseURL: baseURL,
		apiKey:  profile.APIKey,
		headers: profile.Headers,
		model:   profile.ModelID,
	}
}

// Wire types. Field sets follow the chat completions schema; omitempty keeps
// requests minimal so strict compatible servers accept them.

type chatRequest struct {
	Model         string             `json:"model"`
	Messages      []chatMessage      `json:"messages"`
	Tools         []chatTool         `json:"tools,omitempty"`
	ToolChoice    string             `json:"tool_choice,omitempty"`
	Stream        bool               `json:"stream,omitempty"`
	StreamOptions *chatStreamOptions `json:"stream_options,omitempty"`
	MaxTokens     int                `json:"max_tokens,omitempty"`
	// MaxCompletionTokens is the replacement for MaxTokens on newer OpenAI
	// reasoning models, which reject the legacy field.
	MaxCompletionTokens int `json:"max_completion_tokens,omitempty"`
}

type chatStreamOptions struct {
	IncludeUsage bool `json:"include_usage"`
}

type chatMessage struct {
	Role       string         `json:"role"`
	Content    *string        `json:"content,omitempty"`
	ToolCalls  []chatToolCall `json:"tool_calls,omitempty"`
	ToolCallID string         `json:"tool_call_id,omitempty"`
}

type chatToolCall struct {
	// Index orders streamed tool-call deltas; requests never set it, so
	// omitempty keeps it out of the wire form there.
	Index    int          `json:"index,omitempty"`
	ID       string       `json:"id,omitempty"`
	Type     string       `json:"type,omitempty"`
	Function chatFunction `json:"function"`
}

type chatFunction struct {
	Name      string `json:"name,omitempty"`
	Arguments string `json:"arguments,omitempty"`
}

type chatTool struct {
	Type     string           `json:"type"`
	Function chatToolFunction `json:"function"`
}

type chatToolFunction struct {
	Name        string          `json:"name"`
	Description string          `json:"description,omitempty"`
	Parameters  json.RawMessage `json:"parameters"`
}

// toChatMessages maps the durable conversation onto chat completions messages.
// Persisted reasoning parts are deliberately not replayed: the format has no
// standard field for reasoning and several compatible servers reject it, so
// reasoning is display-only for this wire format.
func toChatMessages(messages []session.Message) ([]chatMessage, error) {
	out := make([]chatMessage, 0, len(messages))
	for i, message := range messages {
		switch message.Role {
		case session.RoleSystem, session.RoleUser:
			out = append(out, chatMessage{Role: string(message.Role), Content: &message.Content})
		case session.RoleAssistant:
			// A partial turn interrupted before any answer text carries only
			// reasoning. This wire format has no reasoning field, so there is
			// nothing to send; skipping it avoids an empty assistant message
			// that several servers reject. The reasoning stays in the durable
			// log and the transcript.
			if message.Content == "" && len(message.ToolCalls) == 0 {
				continue
			}
			wire := chatMessage{Role: string(message.Role)}
			if message.Content != "" {
				wire.Content = &message.Content
			}
			for _, call := range message.ToolCalls {
				wire.ToolCalls = append(wire.ToolCalls, chatToolCall{
					ID:       call.ID.String(),
					Type:     "function",
					Function: chatFunction{Name: call.Function.Name, Arguments: string(call.Function.Arguments)},
				})
			}
			out = append(out, wire)
		case session.RoleTool:
			// Tool messages always carry content, even when a tool returned
			// nothing: the field is required for the role.
			out = append(out, chatMessage{Role: string(message.Role), Content: &message.Content, ToolCallID: message.ToolCallID.String()})
		default:
			return nil, fmt.Errorf("message %d has role %q which this wire format cannot send", i, message.Role)
		}
	}
	return out, nil
}

func toChatTools(tools []Tool) []chatTool {
	if len(tools) == 0 {
		return nil
	}
	out := make([]chatTool, 0, len(tools))
	for _, tool := range tools {
		out = append(out, chatTool{Type: "function", Function: chatToolFunction{Name: tool.Name, Description: tool.Description, Parameters: tool.Parameters}})
	}
	return out
}

// Stream runs one streamed generation and forwards text and reasoning deltas
// through emit as they arrive.
func (m *chatModel) Stream(ctx context.Context, messages []session.Message, tools []Tool, emit func(Event)) (Response, error) {
	wireMessages, err := toChatMessages(messages)
	if err != nil {
		return Response{}, err
	}
	payload := chatRequest{
		Model:         m.model,
		Messages:      wireMessages,
		Tools:         toChatTools(tools),
		Stream:        true,
		StreamOptions: &chatStreamOptions{IncludeUsage: true},
	}
	response, err := m.stream(ctx, payload, emit)
	if !isStreamOptionsError(err) {
		return response, err
	}
	// Some compatible servers predate stream_options. Usage then arrives only
	// if the server includes it on its own; estimation covers the rest.
	payload.StreamOptions = nil
	return m.stream(ctx, payload, emit)
}

func (m *chatModel) stream(ctx context.Context, payload chatRequest, emit func(Event)) (Response, error) {
	body, err := json.Marshal(payload)
	if err != nil {
		return Response{}, fmt.Errorf("encode chat request: %w", err)
	}
	response, err := m.send(ctx, body, "text/event-stream")
	if err != nil {
		return Response{}, err
	}
	defer response.Body.Close()
	if response.StatusCode != http.StatusOK {
		raw, _ := io.ReadAll(io.LimitReader(response.Body, maxEventSize))
		return Response{}, parseAPIError(response.StatusCode, raw)
	}
	result, err := decodeChatStream(response.Body, emit)
	if err != nil {
		// A cancelled or dropped connection can arrive with deltas already
		// assembled. Tool calls are deliberately dropped here: the aborted turn
		// never executes them, and replaying an assistant tool call without its
		// results would be rejected by the provider. The client keeps only the
		// text and reasoning.
		return result, err
	}
	if err := finalizeToolCalls(&result); err != nil {
		return Response{}, err
	}
	return result, nil
}

// Complete runs one non-streamed generation. tools, when non-empty, is sent
// with tool_choice "none": the request matches the streaming turn's tool roster
// so it can reuse the provider's cached prefix, while the summary itself can
// never become a tool call.
func (m *chatModel) Complete(ctx context.Context, messages []session.Message, tools []Tool, maxTokens int) (Response, error) {
	wireMessages, err := toChatMessages(messages)
	if err != nil {
		return Response{}, err
	}
	payload := chatRequest{Model: m.model, Messages: wireMessages, MaxTokens: maxTokens}
	if len(tools) > 0 {
		payload.Tools = toChatTools(tools)
		payload.ToolChoice = "none"
	}
	response, err := m.complete(ctx, payload)
	if isMaxTokensError(err) {
		// Newer OpenAI reasoning models reject the legacy max_tokens field.
		payload.MaxTokens = 0
		payload.MaxCompletionTokens = maxTokens
		return m.complete(ctx, payload)
	}
	return response, err
}

func (m *chatModel) complete(ctx context.Context, payload chatRequest) (Response, error) {
	body, err := json.Marshal(payload)
	if err != nil {
		return Response{}, fmt.Errorf("encode chat request: %w", err)
	}
	if _, hasDeadline := ctx.Deadline(); !hasDeadline {
		// Guard against a hung server; callers with their own deadline win.
		var cancel context.CancelFunc
		ctx, cancel = context.WithTimeout(ctx, completeTimeout)
		defer cancel()
	}
	response, err := m.send(ctx, body, "application/json")
	if err != nil {
		return Response{}, err
	}
	defer response.Body.Close()
	raw, err := io.ReadAll(io.LimitReader(response.Body, maxBodySize))
	if err != nil {
		return Response{}, fmt.Errorf("read chat response: %w", err)
	}
	if response.StatusCode != http.StatusOK {
		return Response{}, parseAPIError(response.StatusCode, raw)
	}
	var decoded chatCompletion
	if err := json.Unmarshal(raw, &decoded); err != nil {
		return Response{}, fmt.Errorf("decode chat response: %w", err)
	}
	if decoded.Error != nil {
		return Response{}, decoded.Error.apiError(raw)
	}
	var result Response
	for _, choice := range decoded.Choices {
		if choice.Index != 0 {
			continue
		}
		result.Text = choice.Message.Content
		result.Reasoning = choice.Message.reasoning()
		result.Finish = chatFinishReason(choice.Finish)
		for _, call := range choice.Message.ToolCalls {
			result.ToolCalls = append(result.ToolCalls, ToolCall{ID: call.ID, Name: call.Function.Name, Arguments: json.RawMessage(call.Function.Arguments)})
		}
		break
	}
	result.Usage = decoded.Usage.usage()
	if err := finalizeToolCalls(&result); err != nil {
		return Response{}, err
	}
	return result, nil
}

func (m *chatModel) send(ctx context.Context, body []byte, accept string) (*http.Response, error) {
	url := strings.TrimRight(m.baseURL, "/") + "/chat/completions"
	request, err := http.NewRequestWithContext(ctx, http.MethodPost, url, bytes.NewReader(body))
	if err != nil {
		return nil, fmt.Errorf("build chat request: %w", err)
	}
	request.Header.Set("Content-Type", "application/json")
	request.Header.Set("Accept", accept)
	if m.apiKey != "" {
		request.Header.Set("Authorization", "Bearer "+m.apiKey)
	}
	for key, value := range m.headers {
		request.Header.Set(key, value)
	}
	response, err := m.client.Do(request)
	if err != nil {
		return nil, fmt.Errorf("chat completions request: %w", err)
	}
	return response, nil
}

// isStreamOptionsError reports whether err is a 400 rejecting the
// stream_options field, which older compatible servers do not know.
func isStreamOptionsError(err error) bool {
	return rejectedField(err, "stream_options")
}

// isMaxTokensError reports whether err is a 400 rejecting the legacy
// max_tokens field, which newer OpenAI reasoning models replace with
// max_completion_tokens.
func isMaxTokensError(err error) bool {
	return rejectedField(err, "max_tokens")
}

// rejectedField reports whether err is a 400 whose body names field as the
// rejected parameter. Matching the body keeps the heuristic
// provider-agnostic without hard-coding each server's phrasing. The check
// also requires a code or type that names an invalid/unknown parameter so
// unrelated 400s that merely mention the field do not trigger a retry.
func rejectedField(err error, field string) bool {
	var apiErr *APIError
	if !errors.As(err, &apiErr) || apiErr.Status != http.StatusBadRequest {
		return false
	}
	body := strings.ToLower(apiErr.Body)
	if !strings.Contains(body, strings.ToLower(field)) {
		return false
	}
	// Error shapes that identify a rejected request parameter. A body naming
	// the field without one of these signals is treated as unrelated.
	for _, shape := range []string{"unknown", "unexpected", "unrecognized", "not supported", "unsupported", "invalid"} {
		if strings.Contains(body, shape) {
			return true
		}
	}
	return false
}

// Stream wire types.

type chatChunk struct {
	Choices []chatChoice `json:"choices"`
	Usage   *chatUsage   `json:"usage"`
	Error   *chatError   `json:"error"`
}

type chatChoice struct {
	Index        int       `json:"index"`
	Delta        chatDelta `json:"delta"`
	FinishReason *string   `json:"finish_reason"`
}

type chatDelta struct {
	Role             string          `json:"role"`
	Content          string          `json:"content"`
	ReasoningContent string          `json:"reasoning_content"`
	Reasoning        json.RawMessage `json:"reasoning"`
	ToolCalls        []chatToolCall  `json:"tool_calls"`
}

// reasoning tolerates the two streaming conventions for reasoning text:
// reasoning_content (DeepSeek and friends) and reasoning (OpenRouter). A
// non-string reasoning value decodes into Reasoning raw and yields nothing
// rather than failing the chunk.
func (d chatDelta) reasoning() string {
	if d.ReasoningContent != "" {
		return d.ReasoningContent
	}
	if len(d.Reasoning) == 0 {
		return ""
	}
	var text string
	if err := json.Unmarshal(d.Reasoning, &text); err != nil {
		return ""
	}
	return text
}

type chatError struct {
	Message string `json:"message"`
	Type    string `json:"type"`
	Code    any    `json:"code"`
}

func (e *chatError) apiError(body []byte) *APIError {
	apiErr := &APIError{Message: e.Message, Type: e.Type, Body: string(body)}
	switch code := e.Code.(type) {
	case string:
		apiErr.Code = code
	case float64:
		apiErr.Code = strconv.FormatFloat(code, 'f', -1, 64)
	}
	return apiErr
}

type chatUsage struct {
	PromptTokens        int `json:"prompt_tokens"`
	CompletionTokens    int `json:"completion_tokens"`
	TotalTokens         int `json:"total_tokens"`
	PromptTokensDetails *struct {
		CachedTokens int `json:"cached_tokens"`
	} `json:"prompt_tokens_details"`
}

// usage converts the server's report. chat completions prompt_tokens covers
// every input token including the cached share, so kon stores it as-is and
// records the cached portion separately for context management.
func (u *chatUsage) usage() *session.Usage {
	if u == nil || (u.PromptTokens == 0 && u.CompletionTokens == 0 && u.TotalTokens == 0) {
		return nil
	}
	total := u.TotalTokens
	if total == 0 {
		total = u.PromptTokens + u.CompletionTokens
	}
	usage := &session.Usage{PromptTokens: u.PromptTokens, CompletionTokens: u.CompletionTokens, TotalTokens: total}
	if u.PromptTokensDetails != nil {
		usage.CachedTokens = u.PromptTokensDetails.CachedTokens
	}
	return usage
}

// chatCompletion is the non-streamed response shape.
type chatCompletion struct {
	Choices []chatCompletionChoice `json:"choices"`
	Usage   *chatUsage             `json:"usage"`
	Error   *chatError             `json:"error"`
}

type chatCompletionChoice struct {
	Index   int           `json:"index"`
	Message chatReplyBody `json:"message"`
	Finish  string        `json:"finish_reason"`
}

type chatReplyBody struct {
	Content          string          `json:"content"`
	ReasoningContent string          `json:"reasoning_content"`
	Reasoning        json.RawMessage `json:"reasoning"`
	ToolCalls        []chatToolCall  `json:"tool_calls"`
}

// reasoning handles the non-streamed reasoning conventions (reasoning_content
// or reasoning as a string).
func (m chatReplyBody) reasoning() string {
	if m.ReasoningContent != "" {
		return m.ReasoningContent
	}
	if len(m.Reasoning) == 0 {
		return ""
	}
	var text string
	if err := json.Unmarshal(m.Reasoning, &text); err != nil {
		return ""
	}
	return text
}

// decodeChatStream reads an SSE event stream, assembling assistant text,
// reasoning, and tool calls from deltas. Events are buffered per the SSE spec:
// consecutive data lines join with newlines until a blank line.
func decodeChatStream(r io.Reader, emit func(Event)) (Response, error) {
	var response Response
	scanner := bufio.NewScanner(r)
	scanner.Buffer(make([]byte, 0, 64*1024), maxEventSize)
	var data []string
	done := false
	apply := func(event string) error {
		if event == "[DONE]" {
			return errStreamDone
		}
		return applyChatChunk(&response, event, emit)
	}
	for scanner.Scan() {
		line := scanner.Text()
		switch {
		case strings.HasPrefix(line, "data:"):
			if value := strings.TrimSpace(line[len("data:"):]); value != "" {
				data = append(data, value)
			}
		case line == "":
			if len(data) == 0 {
				continue
			}
			payload := strings.Join(data, "\n")
			data = nil
			if err := apply(payload); err != nil {
				if errors.Is(err, errStreamDone) {
					done = true
					return response, nil
				}
				// Keep deltas assembled before a mid-stream provider error so
				// the partial turn is not discarded.
				return response, err
			}
		default:
			// Comments (": keep-alive") and event:/id:/retry: fields carry
			// nothing kon needs.
		}
	}
	if err := scanner.Err(); err != nil {
		// A dropped connection surfaces here; keep whatever was assembled so
		// the partial turn is preserved rather than lost.
		if errors.Is(err, bufio.ErrTooLong) {
			return response, fmt.Errorf("read chat stream: event exceeded %d bytes: %w", maxEventSize, err)
		}
		return response, fmt.Errorf("read chat stream: %w", err)
	}
	// Flush a trailing event whose blank-line separator never arrived.
	if len(data) > 0 {
		if err := apply(strings.Join(data, "\n")); err != nil {
			if errors.Is(err, errStreamDone) {
				done = true
			} else {
				return response, err
			}
		}
	}
	// A stream that ends without the [DONE] sentinel or a finish reason was cut
	// short even though the transport closed cleanly. Report it as an error so
	// the caller keeps the partial turn instead of mistaking it for complete.
	if !done && response.Finish == "" {
		return response, errors.New("read chat stream: connection closed before the stream finished")
	}
	return response, nil
}

// errStreamDone marks the [DONE] sentinel and unwinds decodeChatStream.
var errStreamDone = errors.New("stream done")

// applyChatChunk folds one SSE data payload into the response.
func applyChatChunk(response *Response, payload string, emit func(Event)) error {
	var chunk chatChunk
	if err := json.Unmarshal([]byte(payload), &chunk); err != nil {
		return fmt.Errorf("decode chat stream chunk: %w", err)
	}
	if chunk.Error != nil {
		return chunk.Error.apiError([]byte(payload))
	}
	for _, choice := range chunk.Choices {
		if choice.Index != 0 {
			continue
		}
		if text := choice.Delta.Content; text != "" {
			response.Text += text
			if emit != nil {
				emit(Event{Text: text})
			}
		}
		if reasoning := choice.Delta.reasoning(); reasoning != "" {
			response.Reasoning += reasoning
			if emit != nil {
				emit(Event{Text: reasoning, Thinking: true})
			}
		}
		for _, call := range choice.Delta.ToolCalls {
			applyChatToolCallDelta(response, call)
		}
		if choice.FinishReason != nil && *choice.FinishReason != "" {
			response.Finish = chatFinishReason(*choice.FinishReason)
		}
	}
	if usage := chunk.Usage.usage(); usage != nil {
		response.Usage = usage
	}
	return nil
}

// applyChatToolCallDelta folds one streamed tool-call delta into the response.
// Deltas arrive in index order: the first for an index carries the ID and
// function name, later ones append argument fragments.
func applyChatToolCallDelta(response *Response, delta chatToolCall) {
	if delta.Index < 0 {
		return
	}
	for len(response.ToolCalls) <= delta.Index {
		response.ToolCalls = append(response.ToolCalls, ToolCall{})
	}
	call := &response.ToolCalls[delta.Index]
	if delta.ID != "" {
		call.ID = delta.ID
	}
	if delta.Function.Name != "" {
		call.Name += delta.Function.Name
	}
	call.Arguments = append(call.Arguments, delta.Function.Arguments...)
}

// finalizeToolCalls normalizes assembled tool calls: empty argument objects
// become {}, malformed arguments fail loudly, and missing IDs — which some
// compatible servers omit — get stable synthetic ones.
func finalizeToolCalls(response *Response) error {
	for i := range response.ToolCalls {
		call := &response.ToolCalls[i]
		if call.ID == "" {
			call.ID = fmt.Sprintf("call_%d", i)
		}
		args := strings.TrimSpace(string(call.Arguments))
		switch {
		case args == "":
			call.Arguments = json.RawMessage(`{}`)
		case json.Valid([]byte(args)):
			call.Arguments = json.RawMessage(args)
		default:
			return fmt.Errorf("provider returned malformed arguments for tool %q", call.Name)
		}
	}
	return nil
}

// chatFinishReason keeps the provider's own finish reason, mapping legacy
// function_call to tool_calls so callers see one spelling.
func chatFinishReason(value string) session.FinishReason {
	if value == "function_call" {
		return "tool_calls"
	}
	return session.FinishReason(value)
}
