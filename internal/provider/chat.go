package provider

import (
	"bufio"
	"bytes"
	"context"
	"encoding/base64"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/http"
	"strconv"
	"strings"
	"time"

	"github.com/hizkifw/kon/internal/buildinfo"
	"github.com/hizkifw/kon/internal/provider/wire"
	"github.com/hizkifw/kon/internal/session"
	"github.com/hizkifw/kon/internal/tokens"
	"github.com/hizkifw/kon/internal/typedid"
)

// chatModel implements Model for OpenAI chat completions, the protocol behind
// every format in the wire table. The format's spec supplies the dialect
// details. kon builds and parses every message itself: request bodies, SSE
// events, tool-call deltas, and usage reports are all owned here.
type chatModel struct {
	client  *http.Client
	baseURL string
	apiKey  string
	headers map[string]string
	model   string
	spec    wire.Spec
	effort  string
	// reasoning marks a model that produces reasoning; see chatReplay.
	reasoning bool
	readImage func(string) ([]byte, error)
}

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

// imageReader returns nil for a model without vision, so its requests carry
// placeholders instead of image parts. A session keeps images read by an
// earlier model, and a /model switch must not send them to one that rejects
// them.
func imageReader(model Spec, readImage func(string) ([]byte, error)) func(string) ([]byte, error) {
	if !model.Vision {
		return nil
	}
	return readImage
}

func newChatModel(model Spec, spec wire.Spec, readImage func(string) ([]byte, error)) (*chatModel, error) {
	baseURL, err := spec.BaseURL(model.BaseURL)
	if err != nil {
		return nil, fmt.Errorf("model %q: %w", model.Name, err)
	}
	return &chatModel{
		client:    &http.Client{},
		baseURL:   baseURL,
		apiKey:    model.APIKey,
		headers:   model.Headers,
		model:     model.ModelID,
		spec:      spec,
		effort:    model.ReasoningEffort,
		reasoning: model.Reasoning,
		readImage: imageReader(model, readImage),
	}, nil
}

// Request bodies. Field sets follow the chat completions schema; omitempty
// keeps requests minimal so strict compatible servers accept them.

type chatRequest struct {
	Model         string             `json:"model"`
	Messages      []chatMessage      `json:"messages"`
	Tools         []chatTool         `json:"tools,omitempty"`
	ToolChoice    string             `json:"tool_choice,omitempty"`
	Stream        bool               `json:"stream,omitempty"`
	StreamOptions *chatStreamOptions `json:"stream_options,omitempty"`
	MaxTokens     tokens.Count       `json:"max_tokens,omitempty"`
	// MaxCompletionTokens is the replacement for MaxTokens on newer OpenAI
	// reasoning models, which reject the legacy field.
	MaxCompletionTokens tokens.Count `json:"max_completion_tokens,omitempty"`
	ReasoningEffort     string       `json:"reasoning_effort,omitempty"`
	// Reasoning is the nested form of the same control, for formats whose
	// spec sets NestedEffort (OpenRouter's normalized API).
	Reasoning *chatReasoning `json:"reasoning,omitempty"`
}

type chatReasoning struct {
	Effort string `json:"effort"`
}

// request starts a payload with the fields every call shares. The effort is
// sent only when one is selected, so servers without the parameter never see it.
func (m *chatModel) request(messages []chatMessage) chatRequest {
	payload := chatRequest{Model: m.model, Messages: messages}
	switch {
	case m.effort == "":
	case m.spec.NestedEffort:
		payload.Reasoning = &chatReasoning{Effort: m.effort}
	default:
		payload.ReasoningEffort = m.effort
	}
	return payload
}

type chatStreamOptions struct {
	IncludeUsage bool `json:"include_usage"`
}

type chatMessage struct {
	Role    string `json:"role"`
	Content any    `json:"content,omitempty"`
	// An assistant's reasoning goes back in the field it arrived in; servers
	// disagree on the name. ReasoningContent is a pointer so DeepSeek can be
	// sent the empty string it requires on messages without reasoning.
	ReasoningContent *string `json:"reasoning_content,omitempty"`
	Reasoning        string  `json:"reasoning,omitempty"`
	ReasoningText    string  `json:"reasoning_text,omitempty"`
	// ReasoningDetails is OpenRouter's structured reasoning, returned verbatim
	// in place of the plain text field because it can carry encrypted entries.
	ReasoningDetails []json.RawMessage `json:"reasoning_details,omitempty"`
	ToolCalls        []chatToolCall    `json:"tool_calls,omitempty"`
	ToolCallID       string            `json:"tool_call_id,omitempty"`
}

// chatImageURL carries one image reference; the wire form is
// {"url": "data:..."} and data URIs carry base64 bytes for vision models.
type chatImageURL struct {
	URL string `json:"url"`
}

// chatContentPart is one typed piece of multimodal message content.
type chatContentPart struct {
	Type string `json:"type"`
	Text string `json:"text,omitempty"`
	// ImageURL is set only on image parts.
	ImageURL *chatImageURL `json:"image_url,omitempty"`
}

// Placeholders stand in for an image the request cannot carry. They are fixed
// strings so a session renders the same bytes on every request, which keeps
// the cached prompt prefix intact.
const (
	imageOmittedText     = "[image omitted: the active model does not accept image input]"
	imageUnavailableText = "[image unavailable: its stored copy could not be read]"
)

// messageContent renders a user or tool message. A message without image parts
// keeps the compact string form. Image parts become multimodal content, except
// that an image is replaced by a text placeholder when readImage is nil (the
// model has no vision) or its blob cannot be read: failing the request instead
// would break every later turn, compaction included. If no image survives, the
// content collapses back to a string, since a server without vision may reject
// the multimodal form outright.
func messageContent(message session.Message, readImage func(string) ([]byte, error)) any {
	hasImage := false
	for _, part := range message.Parts {
		if part.Type == session.PartImage {
			hasImage = true
			break
		}
	}
	if !hasImage {
		text := message.Text()
		return &text
	}
	parts := make([]chatContentPart, 0, len(message.Parts))
	texts := make([]string, 0, len(message.Parts))
	images := 0
	addText := func(text string) {
		parts = append(parts, chatContentPart{Type: "text", Text: text})
		texts = append(texts, text)
	}
	for _, part := range message.Parts {
		switch {
		case part.Type == session.PartText && part.Text != "":
			addText(part.Text)
		case part.Type == session.PartToolResult && part.ToolOutput != "":
			addText(part.ToolOutput)
		case part.Type == session.PartImage && readImage == nil:
			addText(imageOmittedText)
		case part.Type == session.PartImage:
			data, err := readImage(part.ImageHash)
			if err != nil {
				addText(imageUnavailableText)
				continue
			}
			uri := "data:" + part.ImageMIME + ";base64," + base64.StdEncoding.EncodeToString(data)
			parts = append(parts, chatContentPart{Type: "image_url", ImageURL: &chatImageURL{URL: uri}})
			images++
		}
	}
	if images == 0 {
		text := strings.Join(texts, "\n")
		return &text
	}
	return &parts
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

// chatReplay controls how earlier assistant reasoning is sent back. The zero
// value returns every message's reasoning in its recorded field.
type chatReplay struct {
	// model is the requesting model's ID. Reasoning written by a different
	// model is dropped: its format and any encrypted payload belong to that
	// model, and handing it to another invites a rejected request.
	model string
	// defaultField names the reasoning field for a message that did not record
	// the one its reasoning arrived in.
	defaultField string
	// emptyReasoning sends an empty reasoning_content on every assistant
	// message that has none, which DeepSeek's API requires of a reasoning
	// model once the history holds a message without reasoning.
	emptyReasoning bool
}

// replay is this model's reasoning replay policy. The default field comes
// from the wire format. The empty-reasoning rule is a quirk of one service,
// DeepSeek's own API, rather than of a format; an explicit profile carries no
// service identity, so the base URL is the only signal for it.
func (m *chatModel) replay() chatReplay {
	return chatReplay{
		model:          m.model,
		defaultField:   m.spec.ReasoningField,
		emptyReasoning: m.reasoning && strings.Contains(strings.ToLower(m.baseURL), "deepseek.com"),
	}
}

// toChatMessages maps the durable conversation onto chat completions messages.
// Persisted reasoning is sent back with every assistant message the same model
// wrote: models that think across tool calls expect their earlier reasoning in
// the history, and without it the next request's context is smaller than the
// reported usage that preceded it.
func toChatMessages(messages []session.Message, replay chatReplay, readImage func(string) ([]byte, error)) ([]chatMessage, error) {
	out := make([]chatMessage, 0, len(messages))
	for i, message := range messages {
		switch message.Role {
		case session.RoleSystem, session.RoleUser:
			text := message.Text()
			wire := chatMessage{Role: string(message.Role), Content: &text}
			// Image parts are honored on user messages (and tool messages
			// below); the system prompt is plain text by construction.
			if message.Role == session.RoleUser {
				wire.Content = messageContent(message, readImage)
			}
			out = append(out, wire)
		case session.RoleAssistant:
			// A partial turn interrupted before any answer text carries only
			// reasoning. Skipping it avoids an assistant message with no
			// content, which several servers reject. The reasoning stays in
			// the durable log and the transcript.
			text := message.Text()
			calls := message.ToolCalls()
			if text == "" && len(calls) == 0 {
				continue
			}
			wire := chatMessage{Role: string(message.Role)}
			if text != "" {
				wire.Content = &text
			}
			replay.attachReasoning(&wire, message)
			for _, call := range calls {
				wire.ToolCalls = append(wire.ToolCalls, chatToolCall{
					ID:       call.ID.String(),
					Type:     "function",
					Function: chatFunction{Name: call.Function.Name, Arguments: string(call.Function.Arguments)},
				})
			}
			out = append(out, wire)
		case session.RoleTool:
			// Tool messages always carry content, even when a tool returned
			// nothing: the field is required for the role. Image parts from
			// tools like read upgrade the content to multimodal form.
			id, _ := message.ToolResult()
			wire := chatMessage{Role: string(message.Role), ToolCallID: id.String(), Content: messageContent(message, readImage)}
			out = append(out, wire)
		default:
			return nil, fmt.Errorf("message %d has role %q which this wire format cannot send", i, message.Role)
		}
	}
	return out, nil
}

// attachReasoning sets the wire reasoning for one assistant message.
func (replay chatReplay) attachReasoning(wire *chatMessage, message session.Message) {
	sameModel := replay.model == "" || message.Model.String() == "" || message.Model.String() == replay.model
	if sameModel {
		options := decodeChatOptions(message.ProviderOptions)
		field := options.ReasoningField
		if field == "" {
			field = replay.defaultField
		}
		switch text := message.Reasoning(); {
		case len(options.ReasoningDetails) > 0:
			wire.ReasoningDetails = options.ReasoningDetails
		case text == "":
		case field == "reasoning":
			wire.Reasoning = text
		case field == "reasoning_text":
			wire.ReasoningText = text
		default:
			wire.ReasoningContent = &text
		}
	}
	if replay.emptyReasoning && wire.ReasoningContent == nil {
		empty := ""
		wire.ReasoningContent = &empty
	}
}

// chatOptions is the metadata this backend keeps in an assistant message's
// provider_options, so its reasoning can be returned the way it arrived.
type chatOptions struct {
	ReasoningField   string            `json:"reasoning_field,omitempty"`
	ReasoningDetails []json.RawMessage `json:"reasoning_details,omitempty"`
}

// decodeChatOptions reads a message's metadata. Metadata another backend
// wrote, or none at all, decodes to the zero value.
func decodeChatOptions(raw json.RawMessage) chatOptions {
	var options chatOptions
	if len(raw) > 0 {
		_ = json.Unmarshal(raw, &options)
	}
	return options
}

// encode returns the metadata for a message, or nil when there is none.
func (options chatOptions) encode() json.RawMessage {
	if options.ReasoningField == "" && len(options.ReasoningDetails) == 0 {
		return nil
	}
	raw, err := json.Marshal(options)
	if err != nil {
		return nil
	}
	return raw
}

func toChatTools(tools []session.ToolDefinition) []chatTool {
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
func (m *chatModel) Stream(ctx context.Context, messages []session.Message, tools []session.ToolDefinition, emit func(Event)) (Response, error) {
	wireMessages, err := toChatMessages(messages, m.replay(), m.readImage)
	if err != nil {
		return Response{}, err
	}
	payload := m.request(wireMessages)
	payload.Tools = toChatTools(tools)
	payload.Stream = true
	payload.StreamOptions = &chatStreamOptions{IncludeUsage: true}
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
func (m *chatModel) Complete(ctx context.Context, messages []session.Message, tools []session.ToolDefinition, maxTokens tokens.Count) (Response, error) {
	wireMessages, err := toChatMessages(messages, m.replay(), m.readImage)
	if err != nil {
		return Response{}, err
	}
	payload := m.request(wireMessages)
	payload.MaxTokens = maxTokens
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
		var options chatOptions
		if reasoning, field := choice.Message.reasoning(); reasoning != "" {
			result.Parts = append(result.Parts, session.Part{Type: session.PartReasoning, Text: reasoning})
			options.ReasoningField = field
		}
		for _, detail := range choice.Message.ReasoningDetails {
			options.ReasoningDetails = appendReasoningDetail(options.ReasoningDetails, detail)
		}
		result.ProviderOptions = options.encode()
		if choice.Message.Content != "" {
			result.Parts = append(result.Parts, session.Part{Type: session.PartText, Text: choice.Message.Content})
		}
		result.Finish = chatFinishReason(choice.Finish)
		for _, call := range choice.Message.ToolCalls {
			result.Parts = append(result.Parts, session.Part{Type: session.PartToolCall, ToolCallID: typedid.ExternalToolCallID(call.ID), ToolName: call.Function.Name, ToolInput: json.RawMessage(call.Function.Arguments)})
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
	// Set before the configured headers so a profile can override it.
	request.Header.Set("User-Agent", buildinfo.UserAgent())
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

// Streamed response bodies.

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
	Role    string `json:"role"`
	Content string `json:"content"`
	chatReasoningFields
	ToolCalls []chatToolCall `json:"tool_calls"`
}

// chatReasoningFields are the places compatible servers put reasoning, in a
// streamed delta and a complete reply alike.
type chatReasoningFields struct {
	ReasoningContent string            `json:"reasoning_content"`
	Reasoning        json.RawMessage   `json:"reasoning"`
	ReasoningText    string            `json:"reasoning_text"`
	ReasoningDetails []json.RawMessage `json:"reasoning_details"`
}

// reasoning returns the reasoning text and the field it arrived in. Servers
// use reasoning_content (DeepSeek, llama.cpp), reasoning (OpenRouter, vLLM),
// or reasoning_text; the first non-empty one wins, because some servers fill
// two with the same text. A non-string reasoning value decodes into Reasoning
// raw and yields nothing rather than failing the chunk.
func (f chatReasoningFields) reasoning() (text, field string) {
	if f.ReasoningContent != "" {
		return f.ReasoningContent, "reasoning_content"
	}
	if len(f.Reasoning) > 0 {
		if err := json.Unmarshal(f.Reasoning, &text); err == nil && text != "" {
			return text, "reasoning"
		}
	}
	if f.ReasoningText != "" {
		return f.ReasoningText, "reasoning_text"
	}
	return "", ""
}

// appendReasoningDetail folds one reasoning_details entry into the list.
// OpenRouter streams them as deltas: consecutive text or summary entries are
// pieces of one logical entry and are joined, while encrypted entries stay
// discrete. An entry of an unknown type is dropped rather than sent back.
func appendReasoningDetail(details []json.RawMessage, raw json.RawMessage) []json.RawMessage {
	var detail map[string]json.RawMessage
	if json.Unmarshal(raw, &detail) != nil {
		return details
	}
	kind := detailString(detail, "type")
	key, ok := map[string]string{"reasoning.text": "text", "reasoning.summary": "summary", "reasoning.encrypted": "data"}[kind]
	if !ok {
		return details
	}
	if _, present := detail[key]; !present {
		return details
	}
	if n := len(details); n > 0 && kind != "reasoning.encrypted" {
		var last map[string]json.RawMessage
		if json.Unmarshal(details[n-1], &last) == nil && detailString(last, "type") == kind {
			joined, _ := json.Marshal(detailString(last, key) + detailString(detail, key))
			last[key] = joined
			// A later piece can carry what the first lacked, such as the
			// signature that closes a text entry.
			for field, value := range detail {
				if _, present := last[field]; !present || isEmptyDetailValue(last[field]) {
					last[field] = value
				}
			}
			if merged, err := json.Marshal(last); err == nil {
				details[n-1] = merged
			}
			return details
		}
	}
	return append(details, append(json.RawMessage(nil), raw...))
}

func detailString(detail map[string]json.RawMessage, key string) string {
	var value string
	_ = json.Unmarshal(detail[key], &value)
	return value
}

func isEmptyDetailValue(value json.RawMessage) bool {
	trimmed := strings.TrimSpace(string(value))
	return trimmed == "" || trimmed == "null" || trimmed == `""`
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
	PromptTokens        tokens.Count `json:"prompt_tokens"`
	CompletionTokens    tokens.Count `json:"completion_tokens"`
	TotalTokens         tokens.Count `json:"total_tokens"`
	PromptTokensDetails *struct {
		CachedTokens tokens.Count `json:"cached_tokens"`
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
	Content string `json:"content"`
	chatReasoningFields
	ToolCalls []chatToolCall `json:"tool_calls"`
}

// decodeChatStream reads an SSE event stream, assembling assistant text,
// reasoning, and tool calls from deltas. Events are buffered per the SSE spec:
// consecutive data lines join with newlines until a blank line.
func decodeChatStream(r io.Reader, emit func(Event)) (Response, error) {
	state := chatStreamState{callParts: make(map[int]int)}
	scanner := bufio.NewScanner(r)
	scanner.Buffer(make([]byte, 0, 64*1024), maxEventSize)
	var data []string
	done := false
	apply := func(event string) error {
		if event == "[DONE]" {
			return errStreamDone
		}
		return applyChatChunk(&state, event, emit)
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
					return state.result(), nil
				}
				// Keep deltas assembled before a mid-stream provider error so
				// the partial turn is not discarded.
				return state.result(), err
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
			return state.result(), fmt.Errorf("read chat stream: event exceeded %d bytes: %w", maxEventSize, err)
		}
		return state.result(), fmt.Errorf("read chat stream: %w", err)
	}
	// Flush a trailing event whose blank-line separator never arrived.
	if len(data) > 0 {
		if err := apply(strings.Join(data, "\n")); err != nil {
			if errors.Is(err, errStreamDone) {
				done = true
			} else {
				return state.result(), err
			}
		}
	}
	// A stream that ends without the [DONE] sentinel or a finish reason was cut
	// short even though the transport closed cleanly. Report it as an error so
	// the caller keeps the partial turn instead of mistaking it for complete.
	if !done && state.Finish == "" {
		return state.result(), errors.New("read chat stream: connection closed before the stream finished")
	}
	return state.result(), nil
}

type chatStreamState struct {
	Response
	callParts map[int]int
	options   chatOptions
}

// result is the assembled response with its reasoning metadata attached. Every
// return path uses it, so a partial turn keeps the metadata that arrived too.
func (state *chatStreamState) result() Response {
	response := state.Response
	response.ProviderOptions = state.options.encode()
	return response
}

// errStreamDone marks the [DONE] sentinel and unwinds decodeChatStream.
var errStreamDone = errors.New("stream done")

// applyChatChunk folds one SSE data payload into the response.
func applyChatChunk(state *chatStreamState, payload string, emit func(Event)) error {
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
			appendStreamText(&state.Parts, session.PartText, text)
			if emit != nil {
				emit(Event{Text: text})
			}
		}
		if reasoning, field := choice.Delta.reasoning(); reasoning != "" {
			if state.options.ReasoningField == "" {
				state.options.ReasoningField = field
			}
			appendStreamText(&state.Parts, session.PartReasoning, reasoning)
			if emit != nil {
				emit(Event{Text: reasoning, Thinking: true})
			}
		}
		for _, detail := range choice.Delta.ReasoningDetails {
			state.options.ReasoningDetails = appendReasoningDetail(state.options.ReasoningDetails, detail)
		}
		for _, call := range choice.Delta.ToolCalls {
			applyChatToolCallDelta(state, call)
		}
		if choice.FinishReason != nil && *choice.FinishReason != "" {
			state.Finish = chatFinishReason(*choice.FinishReason)
		}
	}
	if usage := chunk.Usage.usage(); usage != nil {
		state.Usage = usage
	}
	return nil
}

func appendStreamText(parts *[]session.Part, kind, text string) {
	if n := len(*parts); n > 0 && (*parts)[n-1].Type == kind {
		(*parts)[n-1].Text += text
		return
	}
	*parts = append(*parts, session.Part{Type: kind, Text: text})
}

// applyChatToolCallDelta folds one streamed tool-call delta into the response.
// Deltas arrive in index order: the first for an index carries the ID and
// function name, later ones append argument fragments.
func applyChatToolCallDelta(state *chatStreamState, delta chatToolCall) {
	if delta.Index < 0 {
		return
	}
	partIndex, ok := state.callParts[delta.Index]
	if !ok {
		partIndex = len(state.Parts)
		state.callParts[delta.Index] = partIndex
		state.Parts = append(state.Parts, session.Part{Type: session.PartToolCall})
	}
	call := &state.Parts[partIndex]
	if delta.ID != "" {
		call.ToolCallID = typedid.ExternalToolCallID(delta.ID)
	}
	if delta.Function.Name != "" {
		call.ToolName += delta.Function.Name
	}
	call.ToolInput = append(call.ToolInput, delta.Function.Arguments...)
}

// finalizeToolCalls normalizes assembled tool calls: empty argument objects
// become {}, malformed arguments fail loudly, and missing IDs — which some
// compatible servers omit — get stable synthetic ones.
func finalizeToolCalls(response *Response) error {
	callIndex := 0
	for i := range response.Parts {
		call := &response.Parts[i]
		if call.Type != session.PartToolCall {
			continue
		}
		if call.ToolCallID.String() == "" {
			call.ToolCallID = typedid.ExternalToolCallID(fmt.Sprintf("call_%d", callIndex))
		}
		callIndex++
		args := strings.TrimSpace(string(call.ToolInput))
		switch {
		case args == "":
			call.ToolInput = json.RawMessage(`{}`)
		case json.Valid([]byte(args)):
			call.ToolInput = json.RawMessage(args)
		default:
			return fmt.Errorf("provider returned malformed arguments for tool %q", call.ToolName)
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
