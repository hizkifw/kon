// Package agent coordinates durable messages, provider calls, tools, and compaction.
package agent

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"path/filepath"
	"strings"
	"time"

	"github.com/hizkifw/kon/internal/contextfiles"
	"github.com/hizkifw/kon/internal/provider"
	"github.com/hizkifw/kon/internal/session"
	"github.com/hizkifw/kon/internal/tokens"
	"github.com/hizkifw/kon/internal/tools"
	"github.com/hizkifw/kon/internal/typedid"
)

type Provider interface {
	Stream(context.Context, []session.Message, []session.ToolDefinition, func(provider.Event)) (session.Message, error)
	Complete(context.Context, []session.Message, []session.ToolDefinition, tokens.Count, func(provider.Event)) (session.Message, error)
}

// ErrNothingToCompact reports that the conversation has no safe cut point yet,
// so a forced compaction (manual /compact or context-overflow recovery) cannot
// make progress. It is not an operational failure.
var ErrNothingToCompact = errors.New("nothing to compact")

type EventKind int

const (
	EventText EventKind = iota
	EventThinking
	EventAssistantDone
	EventToolStart
	EventToolOutput
	EventToolDone
	EventCompacted
	EventUsage
	// EventSteered reports steering messages the runner has just added to the
	// conversation; Text is the user message as sent.
	EventSteered
	// EventCompacting starts a compaction summary, with Tokens the context
	// being compacted. It comes again if the summary is restarted in its
	// fallback form, and what streamed before it is then discarded. The
	// summary ends with EventCompacted, or with the error the run returns.
	EventCompacting
	// EventCompactionText is a delta of the summary being written.
	EventCompactionText
	// EventRetrying reports a provider request that failed before anything
	// streamed and will be sent again: Text is why, Attempt counts retries
	// from 1 up to MaxAttempts, and Delay is the wait before this one. It may
	// come several times; what follows it means the request got through.
	EventRetrying
)

type Event struct {
	Kind EventKind
	Text string
	// CallID identifies the tool call a tool event belongs to, so a consumer
	// can pair each start with its output and result.
	CallID    typedid.ToolCallID
	Tool      string
	Arguments string
	IsError   bool
	Details   json.RawMessage
	Tokens    tokens.Count
	Estimated bool
	// Cost is what the response an EventUsage reports cost, in US dollars,
	// or zero when the model has no price.
	Cost float64
	// Display carries an EventToolOutput snapshot: the running tool's own
	// presentation of the call so far. It replaces any earlier snapshot for
	// the same call.
	Display tools.Display
	// Attempt, MaxAttempts, and Delay describe an EventRetrying retry.
	Attempt, MaxAttempts int
	Delay                time.Duration
}

// RetryEvent reports a provider retry as an EventRetrying.
func RetryEvent(retry *provider.Retry) Event {
	return Event{Kind: EventRetrying, Text: retry.Reason, Attempt: retry.Attempt, MaxAttempts: retry.Max, Delay: retry.Delay}
}

type Runner struct {
	limits   Limits
	provider Provider
	session  *session.Store
	tools    *tools.Executor
	// measured is the provider-reported size of the context through the
	// assistant message measuredAt: the prompt that produced it plus its
	// completion. Messages after measuredAt are estimated on top. A zero
	// measuredAt means no measurement applies, as after a compaction.
	measured   tokens.Count
	measuredAt typedid.EntryID
}

func New(limits Limits, provider Provider, store *session.Store, executor *tools.Executor) *Runner {
	r := &Runner{limits: limits, provider: provider, session: store, tools: executor}
	r.seedUsage()
	return r
}

// seedUsage restores the newest provider-reported usage so a resumed session
// can reuse it for the context indicator and the compaction threshold. Usage
// reported before the latest compaction measured a context that no longer
// exists, so only an assistant message after it counts.
func (r *Runner) seedUsage() {
	for _, entry := range r.session.ActivePath() {
		switch {
		case entry.Type == session.EntryTypeCompaction:
			r.measured, r.measuredAt = 0, typedid.EntryID{}
		case entry.Message != nil && entry.Message.Role == session.RoleAssistant && entry.Message.Usage != nil:
			r.measure(entry.ID, *entry.Message.Usage)
		}
	}
}

// measure records usage reported for the assistant message id.
func (r *Runner) measure(id typedid.EntryID, usage session.Usage) {
	r.measured, r.measuredAt = usage.PromptTokens+usage.CompletionTokens, id
}

// ContextUsage reports the last provider-reported context size in tokens, so a
// resumed session can reuse it instead of falling back to an unknown value. The
// second result is false unless that measurement covers the whole current
// context, which is not the case for a fresh session before its first turn.
func (r *Runner) ContextUsage() (tokens.Count, bool) {
	items, err := Context(r.session)
	if err != nil {
		return 0, false
	}
	if at := r.measuredIndex(items); at < 0 || at != len(items)-1 {
		return 0, false
	}
	return r.measured, true
}

// measuredIndex is the position of the measured assistant message in items,
// or -1 when it is absent or no measurement applies.
func (r *Runner) measuredIndex(items []session.ContextMessage) int {
	if r.measuredAt.IsZero() {
		return -1
	}
	for i := len(items) - 1; i >= 0; i-- {
		if items[i].EntryID == r.measuredAt {
			return i
		}
	}
	return -1
}

// usageFor sizes the context for compaction decisions. The provider's report
// covers everything through the measured assistant message, images included,
// so only the messages after it are estimated. Those are usually tool results
// or the new prompt; a result the store synthesized for an unanswered call is
// estimated like any other. Without a measurement the whole context and the
// tool roster are estimated. estimated is true whenever any part is a guess.
func (r *Runner) usageFor(items []session.ContextMessage) (used tokens.Count, estimated bool) {
	at := r.measuredIndex(items)
	if at < 0 {
		return estimateContext(items, r.tools.Definitions()), true
	}
	newer := items[at+1:]
	if len(newer) == 0 {
		return r.measured, false
	}
	return r.measured + estimateContext(newer, nil), true
}

// Interrupt escalates cancellation of the tool call in flight. The UI sends
// the number of consecutive interrupt presses; the runner forwards them to
// every registered tool, whose shells are interrupted on the first press and
// force-killed on the second if they ignored the interrupt.
func (r *Runner) Interrupt(attempt int) bool {
	return r.tools.Interrupt(attempt)
}

// SystemPrompt builds the durable system prompt persisted as the session's root
// message. It deliberately does not name the model: the prompt is never
// rebuilt, so a later /model switch would leave the name stale, and telling
// the model through a later message risks one that distrusts a user speaking
// as the system. executable is the absolute path to this kon binary. contextFiles
// are AGENTS.md-style instructions, the user's global file first and then the
// project's ordered outermost to innermost; they precede the cwd so a project
// can describe conventions before the model sees where it is working. The
// result is byte-stable for a given input, which is what keeps the provider
// prompt cache valid across compactions.
func SystemPrompt(cwd, executable string, contextFiles []contextfiles.File) string {
	prompt := `You are kon, a coding agent. Work directly in the current working directory.
Use read to inspect files, edit for exact replacements, write for complete files, and shell for commands.
Inspect relevant code before changing it. Tools execute without a sandbox or confirmation.
Your output will be displayed in a terminal with a markdown renderer.
`
	prompt += "\nCurrent kon executable: " + executable
	prompt += "\n\nFor questions about kon itself, run `kon docs` using the executable path above, then read the relevant bundled documentation before answering."
	prompt += "\n\nSubagents are available: run `kon run \"<task>\"` in a background shell job; it will send a message when done, so don't poll it. Use them only when the user explicitly asks."
	prompt += "\n\nMore tools run through the executable above: `kon tool webfetch <url>` prints a web page as Markdown. Run `kon tool --help` for their usage."

	prompt += renderContextFiles(contextFiles)
	prompt += "\nCurrent working directory: " + filepath.Clean(cwd)
	return prompt
}

// renderContextFiles formats discovered instruction files as tagged blocks. The
// path attribute lets the model attribute an instruction to its file when it
// reports or applies it.
func renderContextFiles(files []contextfiles.File) string {
	if len(files) == 0 {
		return ""
	}
	var out strings.Builder
	out.WriteString("\n\nUser and project instructions:")
	for _, file := range files {
		out.WriteString("\n\n<instructions path=\"")
		out.WriteString(file.Path)
		out.WriteString("\">\n")
		out.WriteString(strings.TrimSpace(file.Content))
		out.WriteString("\n</instructions>")
	}
	out.WriteString("\n")
	return out.String()
}

// Run appends prompt before any network work, then drives tool calls to a final
// response. Messages pushed to inbox while it runs are delivered before the
// next request, and a final response with steering still pending does not end
// the run. The turn is bracketed by start and end entries so a replay can show
// its duration; the end is written however the turn returns, including on
// cancellation, so only a process that dies mid-turn leaves a start unmatched.
func (r *Runner) Run(ctx context.Context, prompt string, inbox *Inbox, emit func(Event)) (err error) {
	start := time.Now()
	if _, err := r.session.AppendTurnStart(); err != nil {
		return err
	}
	defer func() {
		if _, endErr := r.session.AppendTurnEnd(time.Since(start)); endErr != nil && err == nil {
			err = endErr
		}
	}()
	return r.run(ctx, prompt, inbox, emit)
}

func (r *Runner) run(ctx context.Context, prompt string, inbox *Inbox, emit func(Event)) error {
	if _, err := r.session.AppendMessage(session.TextMessage(session.RoleUser, prompt)); err != nil {
		return err
	}

	overflowRetried := false
	for {
		if _, err := r.compactIfNeeded(ctx, false, emit); err != nil {
			return err
		}
		messages, err := r.messages()
		if err != nil {
			return err
		}
		assistant, err := r.provider.Stream(ctx, messages, r.tools.Definitions(), func(event provider.Event) {
			if event.Retry != nil {
				emit(RetryEvent(event.Retry))
				return
			}
			if event.Text == "" {
				return
			}
			if event.Thinking {
				emit(Event{Kind: EventThinking, Text: event.Text})
				return
			}
			emit(Event{Kind: EventText, Text: event.Text})
		})
		// Overflow is retried once after a forced compaction, which needs no
		// known context window: the server has just said the context is full.
		if err != nil && provider.IsContextOverflow(err) && !overflowRetried {
			overflowRetried = true
			compacted, compactErr := r.compactIfNeeded(ctx, true, emit)
			if compactErr != nil {
				return fmt.Errorf("provider context overflow; compaction failed: %w", compactErr)
			}
			if !compacted {
				return fmt.Errorf("provider context overflow, and nothing older is left to compact")
			}
			continue
		}
		if err != nil {
			// A cancelled or dropped stream may still have produced a partial
			// assistant message. Persist it so the turn is retained and the
			// next request continues from where it stopped, then surface the
			// original error.
			if assistant.Role == session.RoleAssistant {
				if _, appendErr := r.session.AppendMessage(assistant); appendErr != nil {
					return err
				}
			}
			return err
		}
		assistantID, err := r.session.AppendMessage(assistant)
		if err != nil {
			return err
		}
		emit(Event{Kind: EventAssistantDone})
		if assistant.Usage != nil {
			r.measure(assistantID, *assistant.Usage)
			emit(Event{Kind: EventUsage, Tokens: r.measured, Cost: assistant.Usage.Cost})
		}
		calls := assistant.ToolCalls()
		if len(calls) == 0 {
			// Steering that arrived during the final response is the user's
			// next word on the same task, so the run continues with it.
			steered, err := r.deliver(inbox, emit)
			if err != nil {
				return err
			}
			if steered {
				continue
			}
			_, compactErr := r.compactIfNeeded(ctx, false, emit)
			return compactErr
		}

		for i, call := range calls {
			arguments := string(call.Function.Arguments)
			emit(Event{Kind: EventToolStart, CallID: call.ID, Tool: call.Function.Name, Arguments: arguments})
			// A long-running tool publishes live display snapshots; they are
			// forwarded as coalescible events that replace the running call's
			// presentation in the transcript.
			report := func(d tools.Display) {
				emit(Event{Kind: EventToolOutput, CallID: call.ID, Tool: call.Function.Name, Arguments: arguments, Display: d})
			}
			result, isError := r.tools.Execute(ctx, call.Function.Name, call.Function.Arguments, report)
			message := session.ToolResultMessage(call.ID, call.Function.Name, result.Content)
			message.IsError, message.Details = isError, result.Details
			// Image bytes are stored beside the session before the result
			// references them; only their hashes stay in the context tree.
			for _, image := range result.Images {
				part, err := r.session.SaveImage(image.Data, image.MIME)
				if err != nil {
					return err
				}
				message.Parts = append(message.Parts, part)
			}
			if _, err := r.session.AppendMessage(message); err != nil {
				return err
			}
			// The done event carries the raw result; the transcript resolves
			// the final display through the owning tool, which supersedes any
			// live snapshots the call published.
			emit(Event{Kind: EventToolDone, CallID: call.ID, Tool: call.Function.Name, Arguments: arguments, Text: result.Content, IsError: isError, Details: result.Details})
			if ctx.Err() != nil {
				if err := r.appendInterruptedToolResults(calls[i+1:]); err != nil {
					return err
				}
				return ctx.Err()
			}
		}
		if _, err := r.deliver(inbox, emit); err != nil {
			return err
		}
	}
}

// deliver appends every pending steering message as one user message after
// the newest entry, so the conversation only grows at its end and the cached
// prefix survives. Stacked messages are joined rather than sent as separate
// user messages: some chat templates reject two user messages in a row.
func (r *Runner) deliver(inbox *Inbox, emit func(Event)) (bool, error) {
	pending := inbox.Take()
	if len(pending) == 0 {
		return false, nil
	}
	text := strings.Join(pending, "\n\n")
	if _, err := r.session.AppendMessage(session.TextMessage(session.RoleUser, text)); err != nil {
		return false, err
	}
	emit(Event{Kind: EventSteered, Text: text})
	return true, nil
}

// appendInterruptedToolResults closes the assistant's tool-call batch when a
// turn is cancelled after one call. Keeping one result per call makes the
// persisted conversation valid for providers that require a complete batch.
func (r *Runner) appendInterruptedToolResults(calls []session.ToolCall) error {
	for _, call := range calls {
		message := session.ToolResultMessage(call.ID, call.Function.Name, session.InterruptedToolResult)
		message.IsError = true
		if _, err := r.session.AppendMessage(message); err != nil {
			return err
		}
	}
	return nil
}

func (r *Runner) messages() ([]session.Message, error) {
	contextMessages, err := Context(r.session)
	if err != nil {
		return nil, err
	}
	messages := make([]session.Message, 0, len(contextMessages))
	for _, item := range contextMessages {
		messages = append(messages, item.Message)
	}
	return messages, nil
}

// Compact forces a compaction of the current context regardless of the
// configured threshold, appending a summary entry. It is the manual /compact
// path. When everything since the last summary still fits in the kept window
// it returns ErrNothingToCompact.
func (r *Runner) Compact(ctx context.Context, emit func(Event)) error {
	if err := ctx.Err(); err != nil {
		return err
	}
	compacted, err := r.compactIfNeeded(ctx, true, emit)
	if err != nil {
		return err
	}
	if !compacted {
		return ErrNothingToCompact
	}
	return nil
}

// compactIfNeeded compacts when the context is over its threshold, or always
// when force is set. Only the threshold needs a known context window: a forced
// compaction, from /compact or after a provider overflow, runs without one.
// It returns false with no error when nothing is old enough to fold away.
func (r *Runner) compactIfNeeded(ctx context.Context, force bool, emit func(Event)) (bool, error) {
	window := r.limits.ContextWindow
	if window == 0 && !force {
		return false, nil
	}
	items, err := Context(r.session)
	if err != nil {
		return false, err
	}
	used, estimated := r.usageFor(items)
	if !force && used <= r.limits.threshold() {
		return false, nil
	}

	historyStart := 1
	if len(items) > 1 && items[1].Summary {
		historyStart = 2
	}
	cut := selectCut(items, r.limits.keepRecent())
	if cut <= 1 || cut >= len(items) || items[cut].EntryID.IsZero() || items[cut].Summary {
		if estimateContext(items[min(historyStart, len(items)):], nil) < r.limits.keepRecent() {
			// Everything since the last summary fits in the kept window, so
			// there is nothing older to fold away.
			return false, nil
		}
		return false, errors.New("active turn is too large to compact safely")
	}
	response, tail, err := r.summarize(ctx, items, cut, used, estimated, emit)
	// A summary cut off at its limit would be persisted and the turns it
	// replaces dropped for good, so it is refused before anything is written.
	if provider.IsOutputLimit(err) || (err == nil && response.Finish == session.FinishLength) {
		return false, fmt.Errorf("compaction summary reached its %d-token limit, so older turns were kept; a lower reasoning effort leaves more of that budget for the summary", r.limits.summaryBudget())
	}
	if err != nil {
		return false, err
	}
	// The cache-preserving request carried the whole live context plus the
	// trailing summary request, so its reported prompt size measures the
	// context being compacted far better than the byte estimate. Only the
	// small tail after the live context is estimated and taken back out.
	if tail != nil && estimated && response.Usage != nil {
		request := make([]session.ContextMessage, 0, len(tail))
		for _, message := range tail {
			request = append(request, session.ContextMessage{Message: message})
		}
		if reported := response.Usage.PromptTokens - estimateContext(request, nil); reported > 0 {
			used, estimated = reported, false
		}
	}
	summary := strings.TrimSpace(response.Text())
	if summary == "" {
		return false, errors.New("provider returned an empty compaction summary")
	}
	if _, err := r.session.AppendCompaction(summary, items[cut].EntryID, used, estimated, response.Usage); err != nil {
		return false, err
	}
	r.measured, r.measuredAt = 0, typedid.EntryID{}
	emit(Event{Kind: EventCompacted, Text: summary, Tokens: used, Estimated: estimated})
	var cost float64
	if response.Usage != nil {
		cost = response.Usage.Cost
	}
	emit(Event{Kind: EventUsage, Tokens: -1, Cost: cost})
	return true, nil
}

// CompactSummaryRequest is appended as the trailing user message of a
// compaction request. Instructions live here rather than in a system message
// because the cache-preserving request must reuse the live turn's exact system
// prompt and prefix to stay cacheable. It is the compaction instruction of
// DeepSeek Harness's compaction-basic package, verbatim, under the MIT license
// (see THIRD_PARTY_NOTICES). It names a prior summary by the
// <compacted-summary> tags Context wraps it in.
const CompactSummaryRequest = `You are now acting as a compaction engine for this AI coding assistant. Condense the conversation ABOVE into a structured checkpoint that lets another model resume the work with no loss of essential context.

Output EXACTLY the Markdown structure below: keep every section, in order. Use terse bullets, not prose paragraphs. Write "(none)" for an empty section — never drop a section.

## Primary Request and Intent
- [the user's original and evolving goals; quote verbatim where the exact wording matters]

## Key Technical Concepts
- [technologies, frameworks, patterns, and conventions in play]

## Files and Code
- [exact path: why it matters, key changes or snippets]

## Errors and Fixes
- [error: how it was resolved, plus any related user feedback]

## Pending Jobs
- [explicitly requested work not yet completed]

## Current Work
- [precisely what was in progress at this checkpoint]

## Next Step
- [the single next action, directly in line with the most recent request, or "(none)"]

## Critical Context
- [decisions and their rationale, constraints, user preferences, open questions, data needed to continue]

Rules:
- Write concise English engineering prose. Preserve exact file paths, commands, error strings, identifiers, numeric values, function signatures, and syntax fragments.
- Capture user feedback and explicit instructions faithfully, especially corrections.
- Do NOT mention this summarization request or that the context was compacted.
- Output only the checkpoint text: do not call any tool or take any other action.
- If the conversation already contains a <compacted-summary> block, it is a PRIOR checkpoint. Do not copy it forward verbatim: preserve still-true facts, drop stale ones, and merge newer information into a single consolidated summary under the same structure.`

// summarize asks the provider for a compaction summary.
//
// The preferred, cache-preserving form sends the live turn's exact prefix — the
// system prompt, projected prior summary, every message, and the tool roster —
// with the summary request appended as one trailing user message. Everything but
// that trailing message then reads the provider prompt cache the last streaming
// turn populated. The live prefix is only known to be unusable once the context
// has already reached the window; the isolated form is used then, and as a
// fallback if the provider still rejects the larger request as too long. It
// keeps the system prompt, and serializes the history before the cut, a prior
// summary included, into one user message ending with the same instruction.
//
// It returns the messages the cache-preserving request appended after the
// live context, or nil when the isolated form answered. With them, the
// response's prompt usage measures the live context.
func (r *Runner) summarize(ctx context.Context, items []session.ContextMessage, cut int, used tokens.Count, estimated bool, emit func(Event)) (session.Message, []session.Message, error) {
	maxSummary := r.limits.summaryBudget()
	// Reasoning is left out: the summary is what the reader is waiting on.
	forward := func(event provider.Event) {
		switch {
		case event.Retry != nil:
			emit(RetryEvent(event.Retry))
		case !event.Thinking && event.Text != "":
			emit(Event{Kind: EventCompactionText, Text: event.Text})
		}
	}
	if r.limits.ContextWindow <= 0 || used < r.limits.ContextWindow {
		request := make([]session.Message, 0, len(items)+1)
		for _, item := range items {
			request = append(request, item.Message)
		}
		request = append(request, session.TextMessage(session.RoleUser, CompactSummaryRequest))
		emit(Event{Kind: EventCompacting, Tokens: used, Estimated: estimated})
		// The live tool roster keeps the prefix cached, so a tool call is
		// answered as unavailable rather than forbidden.
		response, request, err := AnswerWithoutTools(request, summaryToolUnavailable, func(request []session.Message) (session.Message, error) {
			return r.provider.Complete(ctx, request, r.tools.Definitions(), maxSummary, forward)
		})
		if err == nil || !provider.IsContextOverflow(err) {
			return response, request[len(items):], err
		}
		// The prefix did not fit after all; fall through to the isolated form.
	}
	transcript := serializeForSummary(items[1:cut])
	request := []session.Message{items[0].Message, session.TextMessage(session.RoleUser, transcript+CompactSummaryRequest)}
	emit(Event{Kind: EventCompacting, Tokens: used, Estimated: estimated})
	response, err := r.provider.Complete(ctx, request, nil, maxSummary, forward)
	return response, nil, err
}

// summaryToolUnavailable answers a tool call made during compaction.
const summaryToolUnavailable = "Tools are not available while writing the compaction summary. Reply with the summary only, using the conversation above."

// ErrToolsUnavailable reports a model that kept calling tools in a request
// that cannot run them.
var ErrToolsUnavailable = errors.New("model kept calling tools that are unavailable")

// toolRetries bounds how often a request that cannot run tools answers a tool
// call and asks again.
const toolRetries = 2

// AnswerWithoutTools runs a request that must not run tools but still sends
// the live tool roster, so it reads the prompt cache the main conversation
// wrote. Forbidding tool calls through tool_choice would invalidate that cache
// on some providers, so the model may call a tool anyway: each call is
// answered with unavailable as an error result, and the model asked again.
// Each retry only appends, so it still reads the cache.
//
// It returns the request as last sent, and the answer's cost covers every
// attempt. A model still calling tools after the retries yields its last
// response with ErrToolsUnavailable.
func AnswerWithoutTools(request []session.Message, unavailable string, generate func([]session.Message) (session.Message, error)) (session.Message, []session.Message, error) {
	var cost float64
	for attempt := 0; ; attempt++ {
		response, err := generate(request)
		if response.Usage != nil {
			cost += response.Usage.Cost
			response.Usage.Cost = cost
		}
		calls := response.ToolCalls()
		if err != nil || len(calls) == 0 {
			return response, request, err
		}
		if attempt == toolRetries {
			return response, request, ErrToolsUnavailable
		}
		request = append(request, response)
		for _, call := range calls {
			result := session.ToolResultMessage(call.ID, call.Function.Name, unavailable)
			result.IsError = true
			request = append(request, result)
		}
	}
}

func estimateContext(items []session.ContextMessage, definitions []session.ToolDefinition) tokens.Count {
	bytes := 0
	for _, item := range items {
		message := item.Message
		bytes += len(message.Role) + 32
		for _, part := range message.Parts {
			if part.Type != session.PartImage {
				bytes += len(part.Text) + len(part.ToolOutput) + len(part.ToolCallID.String()) + len(part.ToolName) + len(part.ToolInput) + 32
			}
		}
		// Image parts are deliberately left out. Their true cost is decided by
		// the model's vision encoder — dimensions and tiling, not byte size —
		// and guessing from the base64 payload overcounts by two orders of
		// magnitude, which forced compaction on every image. The provider
		// reports the real cost in PromptTokens from the first response on;
		// a turn that genuinely exceeds the window is caught by the context
		// overflow retry in Run instead.
	}
	for _, definition := range definitions {
		bytes += len(definition.Name) + len(definition.Description) + len(definition.Parameters) + 32
	}
	return tokens.Count((bytes + 3) / 4)
}

// selectCut keeps complete turns where possible. A turn starts at a user
// message. Synthetic compaction-summary messages are never boundaries: they
// carry the previous summary and must stay on the summarized side.
func selectCut(items []session.ContextMessage, keepTokens tokens.Count) int {
	if len(items) <= 2 {
		return -1
	}
	var accumulated tokens.Count
	candidate := len(items) - 1
	for i := len(items) - 1; i >= 1; i-- {
		accumulated += estimateContext(items[i:i+1], nil)
		candidate = i
		if accumulated >= keepTokens {
			break
		}
	}
	for candidate > 1 && (items[candidate].Summary || items[candidate].Message.Role != session.RoleUser) {
		candidate--
	}
	if candidate > 1 && !items[candidate].Summary {
		return candidate
	}
	// A single oversized turn can split before an assistant/tool-call group.
	accumulated = 0
	for i := len(items) - 1; i >= 2; i-- {
		accumulated += estimateContext(items[i:i+1], nil)
		if accumulated >= keepTokens && !items[i].Summary && items[i].Message.Role == session.RoleAssistant {
			return i
		}
	}
	return -1
}

func serializeForSummary(items []session.ContextMessage) string {
	var out strings.Builder
	for _, item := range items {
		message := item.Message
		fmt.Fprintf(&out, "[%s]", message.Role)
		if _, name := message.ToolResult(); name != "" {
			fmt.Fprintf(&out, " %s", name)
		}
		out.WriteByte('\n')
		if message.Text() != "" {
			out.WriteString(message.Text())
			out.WriteByte('\n')
		}
		for _, call := range message.ToolCalls() {
			fmt.Fprintf(&out, "tool call %s: %s\n", call.Function.Name, call.Function.Arguments)
		}
		out.WriteByte('\n')
	}
	return out.String()
}
