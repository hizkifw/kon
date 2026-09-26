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
	"strings"

	"github.com/hizkifw/kon/internal/buildinfo"
	"github.com/hizkifw/kon/internal/provider/wire"
	"github.com/hizkifw/kon/internal/session"
	"github.com/hizkifw/kon/internal/tokens"
	"github.com/hizkifw/kon/internal/typedid"
)

// responsesModel implements Model for OpenAI's Responses API. kon runs it
// stateless (store: false): the session file stays the only copy of the
// conversation, so every request carries the whole history, with reasoning
// replayed from the encrypted items the server returned.
type responsesModel struct {
	client    *http.Client
	baseURL   string
	apiKey    string
	headers   map[string]string
	model     string
	spec      wire.Spec
	effort    string
	reasoning bool
	readImage func(string) ([]byte, error)
}

func newResponsesModel(model Spec, spec wire.Spec, readImage func(string) ([]byte, error)) (*responsesModel, error) {
	baseURL, err := spec.BaseURL(model.BaseURL)
	if err != nil {
		return nil, fmt.Errorf("model %q: %w", model.Name, err)
	}
	return &responsesModel{
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

type responsesRequest struct {
	Model string `json:"model"`
	// Input mixes items kon builds with items replayed verbatim.
	Input           []any               `json:"input"`
	Tools           []responsesTool     `json:"tools,omitempty"`
	ToolChoice      string              `json:"tool_choice,omitempty"`
	Reasoning       *responsesReasoning `json:"reasoning,omitempty"`
	Include         []string            `json:"include,omitempty"`
	MaxOutputTokens tokens.Count        `json:"max_output_tokens,omitempty"`
	Store           bool                `json:"store"`
	Stream          bool                `json:"stream"`
}

type responsesReasoning struct {
	Effort string `json:"effort,omitempty"`
	// Summary asks for readable reasoning; without it the thinking view stays
	// empty.
	Summary string `json:"summary"`
}

type responsesTool struct {
	Type        string          `json:"type"`
	Name        string          `json:"name"`
	Description string          `json:"description,omitempty"`
	Parameters  json.RawMessage `json:"parameters"`
	// Strict is sent as false: the Responses API otherwise enforces strict
	// schemas, which kon's optional tool arguments do not satisfy.
	Strict bool `json:"strict"`
}

type responsesMessage struct {
	Role    string `json:"role"`
	Content any    `json:"content"`
}

type responsesContent struct {
	Type     string `json:"type"`
	Text     string `json:"text,omitempty"`
	ImageURL string `json:"image_url,omitempty"`
}

type responsesFunctionCall struct {
	Type      string `json:"type"`
	CallID    string `json:"call_id"`
	Name      string `json:"name"`
	Arguments string `json:"arguments"`
}

type responsesFunctionOutput struct {
	Type   string `json:"type"`
	CallID string `json:"call_id"`
	Output any    `json:"output"`
}

// itemOptions is what a part keeps in its provider_options: the output item
// exactly as the server sent it, so it can be replayed byte for byte.
type itemOptions struct {
	Item json.RawMessage `json:"item,omitempty"`
}

func decodeItemOptions(raw json.RawMessage) itemOptions {
	var options itemOptions
	if len(raw) > 0 {
		_ = json.Unmarshal(raw, &options)
	}
	return options
}

// toResponsesInput maps the durable conversation onto input items. Parts this
// model wrote go back as the items it returned, which carries reasoning across
// turns and keeps the request bytes stable for the prompt cache. Anything
// else is rebuilt from the part, and reasoning from another model is left
// out: its encrypted content is readable only by the model that wrote it.
func (m *responsesModel) toResponsesInput(messages []session.Message) []any {
	var input []any
	for _, message := range messages {
		switch message.Role {
		case session.RoleSystem:
			input = append(input, responsesMessage{Role: "system", Content: message.Text()})
		case session.RoleUser:
			input = append(input, responsesMessage{Role: "user", Content: m.responsesContent(message)})
		case session.RoleTool:
			id, _ := message.ToolResult()
			input = append(input, responsesFunctionOutput{Type: "function_call_output", CallID: id.String(), Output: m.responsesContent(message)})
		case session.RoleAssistant:
			ownItems := message.Model.String() == "" || message.Model.String() == m.model
			for _, part := range message.Parts {
				if item := decodeItemOptions(part.ProviderOptions).Item; ownItems && len(item) > 0 {
					input = append(input, item)
					continue
				}
				switch {
				case part.Type == session.PartText && part.Text != "":
					input = append(input, responsesMessage{Role: "assistant", Content: part.Text})
				case part.Type == session.PartToolCall:
					input = append(input, responsesFunctionCall{Type: "function_call", CallID: part.ToolCallID.String(), Name: part.ToolName, Arguments: string(part.ToolInput)})
				}
			}
		}
	}
	return input
}

// responsesContent renders a user message or tool output: a plain string when
// it is only text, otherwise a list with images, using the chat backend's
// placeholders for an image the request cannot carry.
func (m *responsesModel) responsesContent(message session.Message) any {
	var content []responsesContent
	images := 0
	for _, part := range message.Parts {
		switch {
		case part.Type == session.PartText && part.Text != "":
			content = append(content, responsesContent{Type: "input_text", Text: part.Text})
		case part.Type == session.PartToolResult && part.ToolOutput != "":
			content = append(content, responsesContent{Type: "input_text", Text: part.ToolOutput})
		case part.Type == session.PartImage && m.readImage == nil:
			content = append(content, responsesContent{Type: "input_text", Text: imageOmittedText})
		case part.Type == session.PartImage:
			data, err := m.readImage(part.ImageHash)
			if err != nil {
				content = append(content, responsesContent{Type: "input_text", Text: imageUnavailableText})
				continue
			}
			content = append(content, responsesContent{Type: "input_image", ImageURL: "data:" + part.ImageMIME + ";base64," + base64.StdEncoding.EncodeToString(data)})
			images++
		}
	}
	if images > 0 {
		return content
	}
	texts := make([]string, 0, len(content))
	for _, c := range content {
		texts = append(texts, c.Text)
	}
	return strings.Join(texts, "\n")
}

func (m *responsesModel) request(messages []session.Message, tools []session.ToolDefinition) responsesRequest {
	payload := responsesRequest{Model: m.model, Input: m.toResponsesInput(messages), Stream: true}
	for _, tool := range tools {
		payload.Tools = append(payload.Tools, responsesTool{Type: "function", Name: tool.Name, Description: tool.Description, Parameters: tool.Parameters})
	}
	if m.reasoning {
		payload.Reasoning = &responsesReasoning{Effort: m.effort, Summary: "auto"}
		payload.Include = []string{"reasoning.encrypted_content"}
	}
	return payload
}

func (m *responsesModel) Stream(ctx context.Context, messages []session.Message, tools []session.ToolDefinition, emit func(Event)) (Response, error) {
	return m.stream(ctx, m.request(messages, tools), emit)
}

// Complete streams too, sharing one decoder with Stream. tools, when set,
// keeps the streaming turn's cached prefix, with tool calls forbidden.
func (m *responsesModel) Complete(ctx context.Context, messages []session.Message, tools []session.ToolDefinition, maxTokens tokens.Count) (Response, error) {
	payload := m.request(messages, tools)
	payload.MaxOutputTokens = maxTokens
	if len(payload.Tools) > 0 {
		payload.ToolChoice = "none"
	}
	if _, hasDeadline := ctx.Deadline(); !hasDeadline {
		var cancel context.CancelFunc
		ctx, cancel = context.WithTimeout(ctx, completeTimeout)
		defer cancel()
	}
	return m.stream(ctx, payload, nil)
}

func (m *responsesModel) stream(ctx context.Context, payload responsesRequest, emit func(Event)) (Response, error) {
	body, err := json.Marshal(payload)
	if err != nil {
		return Response{}, fmt.Errorf("encode responses request: %w", err)
	}
	url := strings.TrimRight(m.baseURL, "/") + "/responses"
	request, err := http.NewRequestWithContext(ctx, http.MethodPost, url, bytes.NewReader(body))
	if err != nil {
		return Response{}, fmt.Errorf("build responses request: %w", err)
	}
	request.Header.Set("User-Agent", buildinfo.UserAgent())
	request.Header.Set("Content-Type", "application/json")
	request.Header.Set("Accept", "text/event-stream")
	for key, value := range m.spec.AuthHeaders(m.apiKey) {
		request.Header.Set(key, value)
	}
	for key, value := range m.headers {
		request.Header.Set(key, value)
	}
	response, err := m.client.Do(request)
	if err != nil {
		return Response{}, fmt.Errorf("responses request: %w", err)
	}
	defer response.Body.Close()
	if response.StatusCode != http.StatusOK {
		raw, _ := io.ReadAll(io.LimitReader(response.Body, maxEventSize))
		return Response{}, parseAPIError(response.StatusCode, raw)
	}
	result, err := decodeResponsesStream(response.Body, emit)
	if err != nil {
		return result, err
	}
	if err := finalizeToolCalls(&result); err != nil {
		return Response{}, err
	}
	return result, nil
}

// Streamed events.

type responsesEvent struct {
	Type        string             `json:"type"`
	OutputIndex int                `json:"output_index"`
	Item        json.RawMessage    `json:"item"`
	Delta       string             `json:"delta"`
	Response    *responsesResponse `json:"response"`
	// Code and Message are set on a top-level error event.
	Code    string `json:"code"`
	Message string `json:"message"`
}

type responsesResponse struct {
	Usage             *responsesUsage `json:"usage"`
	IncompleteDetails *struct {
		Reason string `json:"reason"`
	} `json:"incomplete_details"`
	Error *chatError `json:"error"`
}

type responsesUsage struct {
	InputTokens        tokens.Count `json:"input_tokens"`
	OutputTokens       tokens.Count `json:"output_tokens"`
	TotalTokens        tokens.Count `json:"total_tokens"`
	InputTokensDetails *struct {
		CachedTokens tokens.Count `json:"cached_tokens"`
	} `json:"input_tokens_details"`
}

// usage converts the report. Here input_tokens covers every input token, the
// cached share included, as chat completions' prompt_tokens does.
func (u *responsesUsage) usage() *session.Usage {
	if u == nil || (u.InputTokens == 0 && u.OutputTokens == 0) {
		return nil
	}
	usage := &session.Usage{PromptTokens: u.InputTokens, CompletionTokens: u.OutputTokens, TotalTokens: u.TotalTokens}
	if usage.TotalTokens == 0 {
		usage.TotalTokens = u.InputTokens + u.OutputTokens
	}
	if u.InputTokensDetails != nil {
		usage.CachedTokens = u.InputTokensDetails.CachedTokens
	}
	return usage
}

// responsesItem is the part of an output item kon reads; the item itself is
// kept raw for replay.
type responsesItem struct {
	Type    string `json:"type"`
	CallID  string `json:"call_id"`
	Name    string `json:"name"`
	Args    string `json:"arguments"`
	Summary []struct {
		Text string `json:"text"`
	} `json:"summary"`
	Content []struct {
		Type string `json:"type"`
		Text string `json:"text"`
	} `json:"content"`
}

type responsesStreamState struct {
	Response
	// items maps an output index to its part; texts collects streamed text
	// until the finished item replaces it.
	items map[int]int
	texts map[int]*strings.Builder
	done  bool
}

func (state *responsesStreamState) result() Response {
	response := state.Response
	response.Parts = append([]session.Part(nil), state.Parts...)
	for index, text := range state.texts {
		response.Parts[state.items[index]].Text = text.String()
	}
	return response
}

// decodeResponsesStream assembles a response from the SSE stream. Each output
// item opens a part when it is added, streams into it, and is replaced by the
// finished item, which is what gets replayed.
func decodeResponsesStream(r io.Reader, emit func(Event)) (Response, error) {
	state := &responsesStreamState{items: map[int]int{}, texts: map[int]*strings.Builder{}}
	scanner := bufio.NewScanner(r)
	scanner.Buffer(make([]byte, 0, 64*1024), maxEventSize)
	var data []string
	flush := func() error {
		if len(data) == 0 {
			return nil
		}
		payload := strings.Join(data, "\n")
		data = nil
		return state.apply(payload, emit)
	}
	for scanner.Scan() {
		line := scanner.Text()
		switch {
		case strings.HasPrefix(line, "data:"):
			data = append(data, strings.TrimSpace(line[len("data:"):]))
		case line == "":
			if err := flush(); err != nil {
				return state.result(), err
			}
			if state.done {
				return state.result(), nil
			}
		}
	}
	if err := scanner.Err(); err != nil {
		return state.result(), fmt.Errorf("read responses stream: %w", err)
	}
	if err := flush(); err != nil {
		return state.result(), err
	}
	if !state.done {
		return state.result(), errors.New("read responses stream: connection closed before the stream finished")
	}
	return state.result(), nil
}

func (state *responsesStreamState) apply(payload string, emit func(Event)) error {
	var event responsesEvent
	if err := json.Unmarshal([]byte(payload), &event); err != nil {
		return fmt.Errorf("decode responses stream event: %w", err)
	}
	switch event.Type {
	case "error":
		return &APIError{Code: event.Code, Message: event.Message, Body: payload}
	case "response.failed":
		if event.Response != nil && event.Response.Error != nil {
			return event.Response.Error.apiError([]byte(payload))
		}
		return &APIError{Body: payload}
	case "response.output_item.added":
		state.addItem(event.OutputIndex, event.Item)
	case "response.output_text.delta":
		state.appendText(event.OutputIndex, event.Delta)
		if emit != nil && event.Delta != "" {
			emit(Event{Text: event.Delta})
		}
	case "response.reasoning_summary_text.delta":
		state.appendText(event.OutputIndex, event.Delta)
		if emit != nil && event.Delta != "" {
			emit(Event{Text: event.Delta, Thinking: true})
		}
	case "response.function_call_arguments.delta":
		if at, ok := state.items[event.OutputIndex]; ok {
			state.Parts[at].ToolInput = append(state.Parts[at].ToolInput, event.Delta...)
		}
	case "response.output_item.done":
		state.finishItem(event.OutputIndex, event.Item)
	case "response.completed", "response.incomplete":
		state.done = true
		if event.Response != nil {
			state.Usage = event.Response.Usage.usage()
		}
		state.Finish = "stop"
		if len(state.ToolCalls()) > 0 {
			state.Finish = "tool_calls"
		}
		if event.Type == "response.incomplete" && event.Response != nil && event.Response.IncompleteDetails != nil {
			state.Finish = session.FinishReason(event.Response.IncompleteDetails.Reason)
			if state.Finish == "max_output_tokens" {
				state.Finish = session.FinishLength
			}
		}
	}
	return nil
}

// addItem opens the part an output item streams into. Item types kon does
// not use, such as a built-in tool's, get none.
func (state *responsesStreamState) addItem(index int, raw json.RawMessage) {
	var item responsesItem
	if json.Unmarshal(raw, &item) != nil {
		return
	}
	var part session.Part
	switch item.Type {
	case "message":
		part = session.Part{Type: session.PartText}
	case "reasoning":
		part = session.Part{Type: session.PartReasoning}
	case "function_call":
		part = session.Part{Type: session.PartToolCall, ToolCallID: typedid.ExternalToolCallID(item.CallID), ToolName: item.Name}
	default:
		return
	}
	state.items[index] = len(state.Parts)
	state.Parts = append(state.Parts, part)
	if part.Type != session.PartToolCall {
		state.texts[index] = &strings.Builder{}
	}
}

// finishItem replaces a streamed part with the finished item's content and
// keeps the item itself for replay.
func (state *responsesStreamState) finishItem(index int, raw json.RawMessage) {
	at, ok := state.items[index]
	if !ok {
		return
	}
	var item responsesItem
	if json.Unmarshal(raw, &item) != nil {
		return
	}
	part := &state.Parts[at]
	switch item.Type {
	case "message":
		var texts []string
		for _, c := range item.Content {
			if c.Type == "output_text" {
				texts = append(texts, c.Text)
			}
		}
		part.Text = strings.Join(texts, "")
	case "reasoning":
		var texts []string
		for _, s := range item.Summary {
			texts = append(texts, s.Text)
		}
		part.Text = strings.Join(texts, "\n\n")
	case "function_call":
		part.ToolCallID, part.ToolName, part.ToolInput = typedid.ExternalToolCallID(item.CallID), item.Name, json.RawMessage(item.Args)
	}
	delete(state.texts, index)
	part.ProviderOptions, _ = json.Marshal(itemOptions{Item: append(json.RawMessage(nil), raw...)})
}

func (state *responsesStreamState) appendText(index int, text string) {
	if builder, ok := state.texts[index]; ok {
		builder.WriteString(text)
	}
}
