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
	"regexp"
	"strconv"
	"strings"
	"sync"

	"github.com/hizkifw/kon/internal/buildinfo"
	"github.com/hizkifw/kon/internal/provider/wire"
	"github.com/hizkifw/kon/internal/session"
	"github.com/hizkifw/kon/internal/tokens"
	"github.com/hizkifw/kon/internal/typedid"
)

// messagesModel implements Model for Anthropic's Messages API. Like the chat
// backend it owns every byte on the wire; there is no SDK in between.
type messagesModel struct {
	client    *http.Client
	baseURL   string
	apiKey    string
	headers   map[string]string
	model     string
	spec      wire.Spec
	effort    string
	reasoning bool
	readImage func(string) ([]byte, error)

	// What the server has told this model about itself. Each is learned from
	// one rejected request and kept, so the next request is right the first
	// time instead of being rejected again.
	mu sync.Mutex
	// budgetThinking asks for thinking with a token budget, for a model that
	// predates adaptive thinking.
	budgetThinking bool
	// outputCap is the model's output limit, when it is below
	// defaultMessagesMaxTokens.
	outputCap tokens.Count
	// dropMismatched opts into dropping thinking blocks the server no longer
	// accepts; see isBoundThinkingError.
	dropMismatched bool
}

// defaultMessagesMaxTokens is the output budget of a streamed turn. The API
// requires one; a model with a lower limit reports it, and kon adopts it.
const defaultMessagesMaxTokens tokens.Count = 32_000

// minThinkingBudget is the smallest budget a model with budgeted thinking
// accepts.
const minThinkingBudget tokens.Count = 1024

// thinkingBindingBeta lets a request drop thinking blocks whose recorded
// conversation no longer matches, instead of failing.
const thinkingBindingBeta = "thinking-binding-controls-2026-08-01"

func newMessagesModel(model Spec, spec wire.Spec, readImage func(string) ([]byte, error)) (*messagesModel, error) {
	baseURL, err := spec.BaseURL(model.BaseURL)
	if err != nil {
		return nil, fmt.Errorf("model %q: %w", model.Name, err)
	}
	return &messagesModel{
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

// Request bodies.

type messagesRequest struct {
	Model        string                `json:"model"`
	MaxTokens    tokens.Count          `json:"max_tokens"`
	System       []messagesBlock       `json:"system,omitempty"`
	Messages     []messagesMessage     `json:"messages"`
	Tools        []messagesTool        `json:"tools,omitempty"`
	ToolChoice   *messagesToolChoice   `json:"tool_choice,omitempty"`
	Thinking     *messagesThinking     `json:"thinking,omitempty"`
	OutputConfig *messagesOutputConfig `json:"output_config,omitempty"`
	Stream       bool                  `json:"stream"`
}

type messagesMessage struct {
	Role    string          `json:"role"`
	Content []messagesBlock `json:"content"`
}

// messagesBlock is one content block of any type; each type fills its own
// fields and omitempty keeps the rest off the wire.
type messagesBlock struct {
	Type string `json:"type"`
	Text string `json:"text,omitempty"`
	// Thinking is a pointer because a thinking block whose text the server
	// omitted must still go back with an empty string.
	Thinking  *string `json:"thinking,omitempty"`
	Signature string  `json:"signature,omitempty"`
	// Data is a redacted_thinking block's opaque payload.
	Data      string               `json:"data,omitempty"`
	ID        string               `json:"id,omitempty"`
	Name      string               `json:"name,omitempty"`
	Input     json.RawMessage      `json:"input,omitempty"`
	ToolUseID string               `json:"tool_use_id,omitempty"`
	Content   []messagesBlock      `json:"content,omitempty"`
	IsError   bool                 `json:"is_error,omitempty"`
	Source    *messagesImageSource `json:"source,omitempty"`
	// CacheControl marks a prompt cache breakpoint; see withCacheBreakpoints.
	CacheControl *messagesCacheControl `json:"cache_control,omitempty"`
}

type messagesImageSource struct {
	Type      string `json:"type"`
	MediaType string `json:"media_type"`
	Data      string `json:"data"`
}

type messagesCacheControl struct {
	Type string `json:"type"`
}

type messagesTool struct {
	Name        string          `json:"name"`
	Description string          `json:"description,omitempty"`
	InputSchema json.RawMessage `json:"input_schema"`
}

type messagesToolChoice struct {
	Type string `json:"type"`
}

type messagesThinking struct {
	Type string `json:"type"`
	// Display asks for readable reasoning; newer models omit it by default,
	// which would leave kon's thinking view empty.
	Display      string       `json:"display,omitempty"`
	BudgetTokens tokens.Count `json:"budget_tokens,omitempty"`
}

type messagesOutputConfig struct {
	Effort string `json:"effort"`
}

// thinkingOptions is what an assistant message's reasoning part keeps in its
// provider_options: the proof the API needs to accept the block back.
type thinkingOptions struct {
	Signature string `json:"signature,omitempty"`
	// Redacted is a redacted_thinking block's payload, which has no text.
	Redacted string `json:"redacted,omitempty"`
}

func decodeThinkingOptions(raw json.RawMessage) thinkingOptions {
	var options thinkingOptions
	if len(raw) > 0 {
		_ = json.Unmarshal(raw, &options)
	}
	return options
}

// toMessagesRequest maps the durable conversation onto the Messages API.
// System messages become the top-level system prompt. A tool result becomes
// a tool_result block in a user turn, and consecutive turns of one role merge
// into a single turn: every result of a parallel batch belongs together, and a
// steering message that follows them joins that turn as trailing text.
func toMessagesRequest(messages []session.Message, readImage func(string) ([]byte, error)) ([]messagesBlock, []messagesMessage) {
	var system []messagesBlock
	var out []messagesMessage
	add := func(role string, blocks []messagesBlock) {
		if len(blocks) == 0 {
			return
		}
		if n := len(out); n > 0 && out[n-1].Role == role {
			out[n-1].Content = append(out[n-1].Content, blocks...)
			return
		}
		out = append(out, messagesMessage{Role: role, Content: blocks})
	}
	for _, message := range messages {
		switch message.Role {
		case session.RoleSystem:
			if text := message.Text(); text != "" {
				system = append(system, messagesBlock{Type: "text", Text: text})
			}
		case session.RoleUser:
			add("user", contentBlocks(message, readImage))
		case session.RoleTool:
			id, _ := message.ToolResult()
			add("user", []messagesBlock{{Type: "tool_result", ToolUseID: id.String(), Content: contentBlocks(message, readImage), IsError: message.IsError}})
		case session.RoleAssistant:
			add("assistant", assistantBlocks(message))
		}
	}
	return system, out
}

// contentBlocks renders a user message or tool result: text as text blocks
// and images as image blocks, with the chat backend's placeholders standing in
// for an image the request cannot carry.
func contentBlocks(message session.Message, readImage func(string) ([]byte, error)) []messagesBlock {
	var blocks []messagesBlock
	for _, part := range message.Parts {
		switch {
		case part.Type == session.PartText && part.Text != "":
			blocks = append(blocks, messagesBlock{Type: "text", Text: part.Text})
		case part.Type == session.PartToolResult && part.ToolOutput != "":
			blocks = append(blocks, messagesBlock{Type: "text", Text: part.ToolOutput})
		case part.Type == session.PartImage && readImage == nil:
			blocks = append(blocks, messagesBlock{Type: "text", Text: imageOmittedText})
		case part.Type == session.PartImage:
			data, err := readImage(part.ImageHash)
			if err != nil {
				blocks = append(blocks, messagesBlock{Type: "text", Text: imageUnavailableText})
				continue
			}
			blocks = append(blocks, messagesBlock{Type: "image", Source: &messagesImageSource{Type: "base64", MediaType: part.ImageMIME, Data: base64.StdEncoding.EncodeToString(data)}})
		}
	}
	return blocks
}

// assistantBlocks renders an assistant message in its original order.
// Thinking goes back unchanged with its signature, which the API requires; a
// reasoning part without one, written by another provider or cut off before
// its signature arrived, cannot be sent and is left out.
func assistantBlocks(message session.Message) []messagesBlock {
	var blocks []messagesBlock
	for _, part := range message.Parts {
		switch part.Type {
		case session.PartReasoning:
			options := decodeThinkingOptions(part.ProviderOptions)
			switch {
			case options.Redacted != "":
				blocks = append(blocks, messagesBlock{Type: "redacted_thinking", Data: options.Redacted})
			case options.Signature != "":
				text := part.Text
				blocks = append(blocks, messagesBlock{Type: "thinking", Thinking: &text, Signature: options.Signature})
			}
		case session.PartText:
			if part.Text != "" {
				blocks = append(blocks, messagesBlock{Type: "text", Text: part.Text})
			}
		case session.PartToolCall:
			input := part.ToolInput
			if !json.Valid(input) {
				input = json.RawMessage(`{}`)
			}
			blocks = append(blocks, messagesBlock{Type: "tool_use", ID: part.ToolCallID.String(), Name: part.ToolName, Input: input})
		}
	}
	// A turn of nothing but thinking is an interrupted one; the API rejects
	// an assistant turn that ends in thinking, so it is dropped whole.
	for _, block := range blocks {
		if block.Type != "thinking" && block.Type != "redacted_thinking" {
			return blocks
		}
	}
	return nil
}

// withCacheBreakpoints marks the two places the prompt cache should cover:
// the end of the system prompt, which caches the tool roster with it and
// never changes in a session, and the end of the conversation, so the next
// request reads everything this one wrote. Only the latter moves, and it only
// ever moves forward.
func withCacheBreakpoints(system []messagesBlock, messages []messagesMessage) {
	ephemeral := &messagesCacheControl{Type: "ephemeral"}
	if n := len(system); n > 0 {
		system[n-1].CacheControl = ephemeral
	}
	if n := len(messages); n > 0 {
		content := messages[n-1].Content
		if last := len(content) - 1; last >= 0 && content[last].Type != "thinking" && content[last].Type != "redacted_thinking" {
			content[last].CacheControl = ephemeral
		}
	}
}

func toMessagesTools(tools []session.ToolDefinition) []messagesTool {
	out := make([]messagesTool, 0, len(tools))
	for _, tool := range tools {
		out = append(out, messagesTool{Name: tool.Name, Description: tool.Description, InputSchema: tool.Parameters})
	}
	return out
}

// request builds a payload. maxTokens is the output budget; thinking follows
// what the model has taught this backend, so it depends on the budget too.
func (m *messagesModel) request(messages []session.Message, tools []session.ToolDefinition, maxTokens tokens.Count) messagesRequest {
	m.mu.Lock()
	budgetThinking, outputCap := m.budgetThinking, m.outputCap
	m.mu.Unlock()
	if outputCap > 0 {
		maxTokens = min(maxTokens, outputCap)
	}
	system, wireMessages := toMessagesRequest(messages, m.readImage)
	withCacheBreakpoints(system, wireMessages)
	payload := messagesRequest{Model: m.model, MaxTokens: maxTokens, System: system, Messages: wireMessages, Stream: true}
	if len(tools) > 0 {
		payload.Tools = toMessagesTools(tools)
	}
	switch {
	case !m.reasoning:
	case !budgetThinking:
		payload.Thinking = &messagesThinking{Type: "adaptive", Display: "summarized"}
	case maxTokens/2 >= minThinkingBudget:
		// The budget must stay below max_tokens, leaving room to answer.
		payload.Thinking = &messagesThinking{Type: "enabled", BudgetTokens: maxTokens / 2}
	}
	if m.effort != "" {
		payload.OutputConfig = &messagesOutputConfig{Effort: m.effort}
	}
	return payload
}

// Stream runs one streamed generation.
func (m *messagesModel) Stream(ctx context.Context, messages []session.Message, tools []session.ToolDefinition, emit func(Event)) (Response, error) {
	return m.run(ctx, messages, tools, defaultMessagesMaxTokens, emit, false)
}

// Complete runs one generation without forwarding deltas. It streams anyway:
// the API holds a non-streamed request open for the whole generation, and
// streaming shares one decoder with Stream. tools, when set, keeps the cached
// prefix of the streaming turn, with tool calls forbidden.
func (m *messagesModel) Complete(ctx context.Context, messages []session.Message, tools []session.ToolDefinition, maxTokens tokens.Count) (Response, error) {
	if maxTokens <= 0 {
		maxTokens = defaultMessagesMaxTokens
	}
	if _, hasDeadline := ctx.Deadline(); !hasDeadline {
		var cancel context.CancelFunc
		ctx, cancel = context.WithTimeout(ctx, completeTimeout)
		defer cancel()
	}
	return m.run(ctx, messages, tools, maxTokens, nil, true)
}

// run sends the request, adapting once to each fact a rejection teaches: an
// output limit below the budget, a model without adaptive thinking, and
// thinking blocks the server no longer accepts. Each is kept for later
// requests.
func (m *messagesModel) run(ctx context.Context, messages []session.Message, tools []session.ToolDefinition, maxTokens tokens.Count, emit func(Event), noTools bool) (Response, error) {
	for attempt := 0; ; attempt++ {
		payload := m.request(messages, tools, maxTokens)
		if noTools && len(payload.Tools) > 0 {
			payload.ToolChoice = &messagesToolChoice{Type: "none"}
		}
		response, err := m.stream(ctx, payload, emit)
		if err == nil || attempt >= 3 || !m.learn(err) {
			return response, err
		}
	}
}

// learn records what a rejected request says about the model and reports
// whether a retry could now succeed.
func (m *messagesModel) learn(err error) bool {
	var apiErr *APIError
	if !errors.As(err, &apiErr) || apiErr.Status != http.StatusBadRequest {
		return false
	}
	body := apiErr.Message + " " + apiErr.Body
	m.mu.Lock()
	defer m.mu.Unlock()
	if limit := outputLimit(body); limit > 0 && (m.outputCap == 0 || limit < m.outputCap) {
		m.outputCap = limit
		return true
	}
	if m.reasoning && !m.budgetThinking && strings.Contains(body, "adaptive") {
		m.budgetThinking = true
		return true
	}
	if !m.dropMismatched && isBoundThinkingError(body) {
		m.dropMismatched = true
		return true
	}
	return false
}

// outputLimitPattern finds the model's limit in a rejection such as
// "max_tokens: 32000 > 8192, which is the maximum allowed".
var outputLimitPattern = regexp.MustCompile(`max_tokens: \d+ > (\d+)`)

func outputLimit(body string) tokens.Count {
	match := outputLimitPattern.FindStringSubmatch(body)
	if match == nil {
		return 0
	}
	limit, err := strconv.Atoi(match[1])
	if err != nil || limit <= 0 {
		return 0
	}
	return tokens.Count(limit)
}

// isBoundThinkingError reports the rejection of a thinking block whose
// recorded conversation no longer matches the request. kon's compaction keeps
// recent turns verbatim after a summary, which changes what precedes their
// thinking; with the binding beta the server drops those blocks instead.
func isBoundThinkingError(body string) bool {
	return strings.Contains(body, "bound to a different conversation")
}

func (m *messagesModel) stream(ctx context.Context, payload messagesRequest, emit func(Event)) (Response, error) {
	body, err := json.Marshal(payload)
	if err != nil {
		return Response{}, fmt.Errorf("encode messages request: %w", err)
	}
	url := strings.TrimRight(m.baseURL, "/") + "/messages"
	request, err := http.NewRequestWithContext(ctx, http.MethodPost, url, bytes.NewReader(body))
	if err != nil {
		return Response{}, fmt.Errorf("build messages request: %w", err)
	}
	request.Header.Set("User-Agent", buildinfo.UserAgent())
	request.Header.Set("Content-Type", "application/json")
	request.Header.Set("Accept", "text/event-stream")
	for key, value := range m.spec.AuthHeaders(m.apiKey) {
		request.Header.Set(key, value)
	}
	m.mu.Lock()
	drop := m.dropMismatched
	m.mu.Unlock()
	if drop {
		request.Header.Set("anthropic-beta", thinkingBindingBeta)
	}
	// Configured headers come last so a profile can override any of these.
	for key, value := range m.headers {
		request.Header.Set(key, value)
	}
	response, err := m.client.Do(request)
	if err != nil {
		return Response{}, fmt.Errorf("messages request: %w", err)
	}
	defer response.Body.Close()
	if response.StatusCode != http.StatusOK {
		raw, _ := io.ReadAll(io.LimitReader(response.Body, maxEventSize))
		return Response{}, parseAPIError(response.StatusCode, raw)
	}
	result, err := decodeMessagesStream(response.Body, emit)
	if err != nil {
		return result, err
	}
	if err := finalizeToolCalls(&result); err != nil {
		return Response{}, err
	}
	if result.Finish == "refusal" && result.Text() == "" && len(result.ToolCalls()) == 0 {
		return Response{}, errors.New("model declined to respond (reason: refusal)")
	}
	return result, nil
}

// Streamed events. Every event names its own type, so the SSE event: line is
// not needed.

type messagesEvent struct {
	Type         string              `json:"type"`
	Index        int                 `json:"index"`
	Message      *messagesStartBody  `json:"message"`
	ContentBlock *messagesStartBlock `json:"content_block"`
	Delta        json.RawMessage     `json:"delta"`
	Usage        *messagesUsage      `json:"usage"`
	Error        *chatError          `json:"error"`
}

type messagesStartBody struct {
	Usage *messagesUsage `json:"usage"`
}

type messagesStartBlock struct {
	Type      string `json:"type"`
	Text      string `json:"text"`
	Thinking  string `json:"thinking"`
	Signature string `json:"signature"`
	Data      string `json:"data"`
	ID        string `json:"id"`
	Name      string `json:"name"`
}

type messagesDelta struct {
	Type        string `json:"type"`
	Text        string `json:"text"`
	Thinking    string `json:"thinking"`
	Signature   string `json:"signature"`
	PartialJSON string `json:"partial_json"`
	StopReason  string `json:"stop_reason"`
}

type messagesUsage struct {
	InputTokens              tokens.Count `json:"input_tokens"`
	OutputTokens             tokens.Count `json:"output_tokens"`
	CacheCreationInputTokens tokens.Count `json:"cache_creation_input_tokens"`
	CacheReadInputTokens     tokens.Count `json:"cache_read_input_tokens"`
}

// merge folds a later report into an earlier one. message_start carries the
// input side and message_delta the final output count; a field a report
// leaves at zero keeps its earlier value.
func (u *messagesUsage) merge(later *messagesUsage) {
	if later == nil {
		return
	}
	for _, pair := range []struct{ dst, src *tokens.Count }{
		{&u.InputTokens, &later.InputTokens}, {&u.OutputTokens, &later.OutputTokens},
		{&u.CacheCreationInputTokens, &later.CacheCreationInputTokens}, {&u.CacheReadInputTokens, &later.CacheReadInputTokens},
	} {
		if *pair.src > 0 {
			*pair.dst = *pair.src
		}
	}
}

// usage converts the report. The API's input_tokens counts only the uncached
// remainder, so the prompt size kon stores is the sum of all three input
// fields, with the cache reads recorded as the cached share.
func (u messagesUsage) usage() *session.Usage {
	prompt := u.InputTokens + u.CacheCreationInputTokens + u.CacheReadInputTokens
	if prompt == 0 && u.OutputTokens == 0 {
		return nil
	}
	return &session.Usage{PromptTokens: prompt, CompletionTokens: u.OutputTokens, TotalTokens: prompt + u.OutputTokens, CachedTokens: u.CacheReadInputTokens}
}

type messagesStreamState struct {
	Response
	// blocks maps a content block's index to its part, and texts collects
	// that part's text: appending to a string per delta would copy the whole
	// answer every time.
	blocks map[int]int
	texts  map[int]*strings.Builder
	usage  messagesUsage
	done   bool
}

// result is the response assembled so far, whether or not the stream ended.
func (state *messagesStreamState) result() Response {
	response := state.Response
	response.Parts = append([]session.Part(nil), state.Parts...)
	for index, text := range state.texts {
		response.Parts[state.blocks[index]].Text = text.String()
	}
	response.Usage = state.usage.usage()
	return response
}

// decodeMessagesStream assembles a response from the SSE stream, forwarding
// text and thinking deltas through emit. A stream cut short returns what had
// arrived with an error, so the partial turn survives.
func decodeMessagesStream(r io.Reader, emit func(Event)) (Response, error) {
	state := &messagesStreamState{blocks: map[int]int{}, texts: map[int]*strings.Builder{}}
	result := state.result
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
				return result(), err
			}
			if state.done {
				return result(), nil
			}
		}
	}
	if err := scanner.Err(); err != nil {
		return result(), fmt.Errorf("read messages stream: %w", err)
	}
	if err := flush(); err != nil {
		return result(), err
	}
	if !state.done {
		return result(), errors.New("read messages stream: connection closed before the stream finished")
	}
	return result(), nil
}

// apply folds one event into the response.
func (state *messagesStreamState) apply(payload string, emit func(Event)) error {
	var event messagesEvent
	if err := json.Unmarshal([]byte(payload), &event); err != nil {
		return fmt.Errorf("decode messages stream event: %w", err)
	}
	switch event.Type {
	case "error":
		if event.Error != nil {
			return event.Error.apiError([]byte(payload))
		}
		return &APIError{Body: payload}
	case "message_start":
		if event.Message != nil {
			state.usage.merge(event.Message.Usage)
		}
	case "content_block_start":
		state.startBlock(event.Index, event.ContentBlock, emit)
	case "content_block_delta":
		var delta messagesDelta
		if err := json.Unmarshal(event.Delta, &delta); err != nil {
			return fmt.Errorf("decode messages stream delta: %w", err)
		}
		state.applyDelta(event.Index, delta, emit)
	case "message_delta":
		var delta messagesDelta
		if len(event.Delta) > 0 {
			_ = json.Unmarshal(event.Delta, &delta)
		}
		if delta.StopReason != "" {
			state.Finish = messagesFinishReason(delta.StopReason)
		}
		state.usage.merge(event.Usage)
	case "message_stop":
		state.done = true
	}
	return nil
}

func (state *messagesStreamState) startBlock(index int, block *messagesStartBlock, emit func(Event)) {
	if block == nil {
		return
	}
	var part session.Part
	switch block.Type {
	case "text":
		part = session.Part{Type: session.PartText}
	case "thinking":
		part = session.Part{Type: session.PartReasoning}
	case "redacted_thinking":
		options, _ := json.Marshal(thinkingOptions{Redacted: block.Data})
		part = session.Part{Type: session.PartReasoning, ProviderOptions: options}
	case "tool_use":
		part = session.Part{Type: session.PartToolCall, ToolCallID: typedid.ExternalToolCallID(block.ID), ToolName: block.Name}
	default:
		// A block type kon does not know, such as a server tool's, has
		// nothing it can replay.
		return
	}
	state.blocks[index] = len(state.Parts)
	state.Parts = append(state.Parts, part)
	if part.Type != session.PartToolCall {
		state.texts[index] = &strings.Builder{}
	}
	// Start events usually arrive empty, but carry text when they do not.
	if block.Text != "" {
		state.applyDelta(index, messagesDelta{Type: "text_delta", Text: block.Text}, emit)
	}
	if block.Thinking != "" {
		state.applyDelta(index, messagesDelta{Type: "thinking_delta", Thinking: block.Thinking}, emit)
	}
	if block.Signature != "" {
		state.applyDelta(index, messagesDelta{Type: "signature_delta", Signature: block.Signature}, emit)
	}
}

func (state *messagesStreamState) applyDelta(index int, delta messagesDelta, emit func(Event)) {
	at, ok := state.blocks[index]
	if !ok {
		return
	}
	part := &state.Parts[at]
	switch delta.Type {
	case "text_delta":
		state.appendText(index, delta.Text)
		if emit != nil && delta.Text != "" {
			emit(Event{Text: delta.Text})
		}
	case "thinking_delta":
		state.appendText(index, delta.Thinking)
		if emit != nil && delta.Thinking != "" {
			emit(Event{Text: delta.Thinking, Thinking: true})
		}
	case "signature_delta":
		options := decodeThinkingOptions(part.ProviderOptions)
		options.Signature += delta.Signature
		part.ProviderOptions, _ = json.Marshal(options)
	case "input_json_delta":
		part.ToolInput = append(part.ToolInput, delta.PartialJSON...)
	}
}

func (state *messagesStreamState) appendText(index int, text string) {
	if builder, ok := state.texts[index]; ok {
		builder.WriteString(text)
	}
}

// messagesFinishReason maps a stop reason onto the spellings the rest of kon
// checks for, keeping any other reason as the API sent it.
func messagesFinishReason(reason string) session.FinishReason {
	switch reason {
	case "end_turn", "stop_sequence":
		return "stop"
	case "tool_use":
		return "tool_calls"
	case "max_tokens", "model_context_window_exceeded":
		return session.FinishLength
	}
	return session.FinishReason(reason)
}
