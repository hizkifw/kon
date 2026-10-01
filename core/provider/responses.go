package provider

import (
	"bufio"
	"bytes"
	"context"
	"crypto/sha256"
	"encoding/base64"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/http"
	"strings"
	"sync"

	"kon.kitsu.red/core/provider/wire"
	"kon.kitsu.red/core/session"
	"kon.kitsu.red/core/tokens"
	"kon.kitsu.red/core/typedid"
)

// responsesModel implements backend for OpenAI's Responses API. kon runs it
// stateless (store: false): the session file stays the only copy of the
// conversation, so every request carries the whole history, with reasoning
// replayed from the encrypted items the server returned.
type responsesModel struct {
	client *http.Client
	// retry is how transient failures are retried; see retryPolicy.
	retry     retryPolicy
	baseURL   string
	apiKey    string
	userAgent string
	headers   map[string]string
	model     string
	spec      wire.Spec
	effort    string
	reasoning bool
	readImage func(string) ([]byte, error)

	// What the server has told this model about itself, each learned from
	// one rejected request and kept, so later requests are right the first
	// time.
	mu sync.Mutex
	// noSummary leaves out reasoning.summary, which OpenAI refuses for an
	// organization that is not verified for it.
	noSummary bool
	// noCacheKey leaves out prompt_cache_key, for a compatible server that
	// does not know it.
	noCacheKey bool
}

func newResponsesModel(model Spec, spec wire.Spec, readImage func(string) ([]byte, error)) (*responsesModel, error) {
	baseURL, err := spec.BaseURL(model.BaseURL)
	if err != nil {
		return nil, fmt.Errorf("model %q: %w", model.Name, err)
	}
	return &responsesModel{
		client:    &http.Client{},
		retry:     defaultRetryPolicy,
		baseURL:   baseURL,
		apiKey:    model.APIKey,
		userAgent: model.UserAgent,
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
	Reasoning       *responsesReasoning `json:"reasoning,omitempty"`
	Include         []string            `json:"include,omitempty"`
	MaxOutputTokens tokens.Count        `json:"max_output_tokens,omitempty"`
	// PromptCacheKey joins the request to the server's cache for this
	// conversation; see promptCacheKey.
	PromptCacheKey string `json:"prompt_cache_key,omitempty"`
	Store          bool   `json:"store"`
	Stream         bool   `json:"stream"`
}

type responsesReasoning struct {
	Effort string `json:"effort,omitempty"`
	// Summary asks for readable reasoning; without it the thinking view stays
	// empty.
	Summary string `json:"summary,omitempty"`
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
			for i, part := range message.Parts {
				if item := decodeItemOptions(part.ProviderOptions).Item; ownItems && len(item) > 0 {
					if part.Type == session.PartReasoning && !replaysItem(message.Parts, i+1) {
						continue
					}
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

// replaysItem reports whether the part at i follows a reasoning item as the
// item it led to. The API rejects a reasoning item without one, which an
// interrupted turn leaves behind: its unfinished message has no item to
// replay, and its tool calls are dropped. The reasoning is left out then,
// rather than failing every later request.
func replaysItem(parts []session.Part, i int) bool {
	return i < len(parts) && parts[i].Type != session.PartReasoning && len(decodeItemOptions(parts[i].ProviderOptions).Item) > 0
}

// promptCacheKey names the conversation for the server's prompt cache, which
// routes requests with the same key and prefix together. It hashes the
// system prompt and the first message after it, which open every request of
// a session, its compaction summaries and side questions included, until a
// compaction replaces that message.
func promptCacheKey(messages []session.Message) string {
	hash := sha256.New()
	for _, message := range messages[:min(len(messages), 2)] {
		hash.Write([]byte(message.Role))
		hash.Write([]byte{0})
		hash.Write([]byte(message.Text()))
		hash.Write([]byte{0})
	}
	return "kon-" + hex.EncodeToString(hash.Sum(nil))[:32]
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

// request builds a payload; maxTokens caps the output when positive.
func (m *responsesModel) request(messages []session.Message, tools []session.ToolDefinition, maxTokens tokens.Count) responsesRequest {
	m.mu.Lock()
	noSummary, noCacheKey := m.noSummary, m.noCacheKey
	m.mu.Unlock()
	payload := responsesRequest{Model: m.model, Input: m.toResponsesInput(messages), MaxOutputTokens: maxTokens, Stream: true}
	if !noCacheKey && len(messages) > 0 {
		payload.PromptCacheKey = promptCacheKey(messages)
	}
	for _, tool := range tools {
		payload.Tools = append(payload.Tools, responsesTool{Type: "function", Name: tool.Name, Description: tool.Description, Parameters: tool.Parameters})
	}
	if m.reasoning {
		payload.Reasoning = &responsesReasoning{Effort: m.effort, Summary: "auto"}
		if noSummary {
			payload.Reasoning.Summary = ""
		}
		payload.Include = []string{"reasoning.encrypted_content"}
	}
	return payload
}

func (m *responsesModel) Stream(ctx context.Context, messages []session.Message, tools []session.ToolDefinition, emit func(Event)) (generation, error) {
	return m.run(ctx, messages, tools, 0, emit)
}

// Complete runs one capped generation, forwarding its deltas through emit when
// set. tools, when set, keeps the streaming turn's cached prefix.
func (m *responsesModel) Complete(ctx context.Context, messages []session.Message, tools []session.ToolDefinition, maxTokens tokens.Count, emit func(Event)) (generation, error) {
	if _, hasDeadline := ctx.Deadline(); !hasDeadline {
		var cancel context.CancelFunc
		ctx, cancel = context.WithTimeout(ctx, completeTimeout)
		defer cancel()
	}
	return m.run(ctx, messages, tools, maxTokens, emit)
}

// run sends the request, adapting once to each fact a rejection teaches, and
// keeps what it learned for later requests.
func (m *responsesModel) run(ctx context.Context, messages []session.Message, tools []session.ToolDefinition, maxTokens tokens.Count, emit func(Event)) (generation, error) {
	for attempt := 0; ; attempt++ {
		response, err := m.stream(ctx, m.request(messages, tools, maxTokens), emit)
		if err == nil || attempt >= 2 || !m.learn(err) {
			return response, err
		}
	}
}

// learn records what a rejected request says about the server and reports
// whether a retry could now succeed.
func (m *responsesModel) learn(err error) bool {
	m.mu.Lock()
	defer m.mu.Unlock()
	if !m.noSummary && m.reasoning && isSummaryRejected(err) {
		m.noSummary = true
		return true
	}
	if !m.noCacheKey && rejectedField(err, "prompt_cache_key") {
		m.noCacheKey = true
		return true
	}
	return false
}

// isSummaryRejected reports a 400 refusing reasoning summaries, as OpenAI
// sends an organization that has not been verified for them: "Your
// organization must be verified to generate reasoning summaries."
func isSummaryRejected(err error) bool {
	var apiErr *APIError
	if !errors.As(err, &apiErr) || apiErr.Status != http.StatusBadRequest {
		return false
	}
	body := strings.ToLower(apiErr.Message + " " + apiErr.Body)
	return strings.Contains(body, "reasoning summar") || strings.Contains(body, "reasoning.summary")
}

// stream sends payload, retrying transient failures.
func (m *responsesModel) stream(ctx context.Context, payload responsesRequest, emit func(Event)) (generation, error) {
	return withRetries(ctx, m.retry, emit, func(emit func(Event)) (generation, error) {
		return m.streamOnce(ctx, payload, emit)
	})
}

func (m *responsesModel) streamOnce(ctx context.Context, payload responsesRequest, emit func(Event)) (generation, error) {
	body, err := json.Marshal(payload)
	if err != nil {
		return generation{}, fmt.Errorf("encode responses request: %w", err)
	}
	url := strings.TrimRight(m.baseURL, "/") + "/responses"
	request, err := http.NewRequestWithContext(ctx, http.MethodPost, url, bytes.NewReader(body))
	if err != nil {
		return generation{}, fmt.Errorf("build responses request: %w", err)
	}
	if m.userAgent != "" {
		request.Header.Set("User-Agent", m.userAgent)
	}
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
		return generation{}, fmt.Errorf("responses request: %w", err)
	}
	defer response.Body.Close()
	if response.StatusCode != http.StatusOK {
		return generation{}, responseError(response)
	}
	result, err := decodeResponsesStream(response.Body, emit)
	if err != nil {
		return result, err
	}
	if err := finalizeToolCalls(&result); err != nil {
		return generation{}, err
	}
	return result, nil
}

// Streamed events.

type responsesEvent struct {
	Type        string          `json:"type"`
	OutputIndex int             `json:"output_index"`
	Item        json.RawMessage `json:"item"`
	Delta       string          `json:"delta"`
	// SummaryIndex numbers a reasoning summary part within its item.
	SummaryIndex int                `json:"summary_index"`
	Response     *responsesResponse `json:"response"`
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
		Type    string `json:"type"`
		Text    string `json:"text"`
		Refusal string `json:"refusal"`
	} `json:"content"`
}

type responsesStreamState struct {
	generation
	// items maps an output index to its part; texts collects streamed text
	// until the finished item replaces it.
	items map[int]int
	texts map[int]*strings.Builder
	done  bool
	// refused marks a message the model declined to write; its refusal
	// stands in for the answer.
	refused bool
}

func (state *responsesStreamState) result() generation {
	response := state.generation
	response.Parts = append([]session.Part(nil), state.Parts...)
	for index, text := range state.texts {
		response.Parts[state.items[index]].Text = text.String()
	}
	return response
}

// decodeResponsesStream assembles a response from the SSE stream. Each output
// item opens a part when it is added, streams into it, and is replaced by the
// finished item, which is what gets replayed.
func decodeResponsesStream(r io.Reader, emit func(Event)) (generation, error) {
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
		return state.result(), fmt.Errorf("read responses stream: %w", errStreamClosed)
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
	case "response.refusal.delta":
		state.refused = true
		state.appendText(event.OutputIndex, event.Delta)
		if emit != nil && event.Delta != "" {
			emit(Event{Text: event.Delta})
		}
	case "response.reasoning_summary_part.added":
		// Summary parts are separate paragraphs, as the finished item's
		// text joins them.
		if event.SummaryIndex > 0 {
			state.appendText(event.OutputIndex, "\n\n")
			if emit != nil {
				emit(Event{Text: "\n\n", Thinking: true})
			}
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
		if state.refused {
			state.Finish = "refusal"
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
			switch c.Type {
			case "output_text":
				texts = append(texts, c.Text)
			case "refusal":
				state.refused = true
				texts = append(texts, c.Refusal)
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
