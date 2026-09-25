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
	Complete(context.Context, []session.Message, []session.ToolDefinition, tokens.Count) (session.Message, error)
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
)

type Event struct {
	Kind      EventKind
	Text      string
	Tool      string
	Arguments string
	IsError   bool
	Details   json.RawMessage
	Tokens    tokens.Count
	Estimated bool
	// Display carries an EventToolOutput snapshot: the running tool's own
	// presentation of the call so far. It replaces any earlier snapshot for
	// the same call.
	Display tools.Display
}

// Limits sizes the context a runner keeps: the model's window and the
// compaction budgets within it. They are the runner's own options, resolved by
// the caller from the model and configuration.
type Limits struct {
	// ContextWindow is the model's context size. Zero means unknown, which
	// turns off automatic compaction; a forced one still runs.
	ContextWindow tokens.Count
	// ReserveTokens is the headroom kept free below the window. Compaction
	// runs once the context would eat into it.
	ReserveTokens tokens.Count
	// KeepRecentTokens is how much recent conversation a compaction keeps
	// verbatim instead of summarizing.
	KeepRecentTokens tokens.Count
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
	items, err := r.session.Context()
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
// are AGENTS.md-style project instructions, ordered
// outermost to innermost; they precede the cwd so a project can describe
// conventions before the model sees where it is working. instructions is the
// user's configured override and comes last, which makes it the most specific
// signal in the prompt. The result is byte-stable for a given input, which is
// what keeps the provider prompt cache valid across compactions.
func SystemPrompt(cwd, executable string, contextFiles []contextfiles.File, instructions string) string {
	prompt := `You are kon, a coding agent. Work directly in the current working directory.
Use read to inspect files, edit for exact replacements, write for complete files, and shell for commands.
Inspect relevant code before changing it. Tools execute without a sandbox or confirmation.
Your output will be displayed in a terminal with a markdown renderer.
`
	prompt += "\nCurrent kon executable: " + executable
	prompt += "\nFor questions about kon itself, run `kon docs` using the executable path above, then read the relevant bundled documentation before answering."

	prompt += renderContextFiles(contextFiles)
	prompt += "\nCurrent working directory: " + filepath.Clean(cwd)

	if strings.TrimSpace(instructions) != "" {
		prompt += "\n\nAdditional user instructions:\n" + strings.TrimSpace(instructions)
	}
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
	out.WriteString("\nProject-specific instructions:")
	for _, file := range files {
		out.WriteString("\n\n<project_instructions path=\"")
		out.WriteString(file.Path)
		out.WriteString("\">\n")
		out.WriteString(strings.TrimSpace(file.Content))
		out.WriteString("\n</project_instructions>")
	}
	out.WriteString("\n")
	return out.String()
}

// Run appends prompt before any network work, then drives tool calls to a final
// response. The turn is bracketed by start and end entries so a replay can show
// its duration; the end is written however the turn returns, including on
// cancellation, so only a process that dies mid-turn leaves a start unmatched.
func (r *Runner) Run(ctx context.Context, prompt string, emit func(Event)) (err error) {
	start := time.Now()
	if _, err := r.session.AppendTurnStart(); err != nil {
		return err
	}
	defer func() {
		if _, endErr := r.session.AppendTurnEnd(time.Since(start)); endErr != nil && err == nil {
			err = endErr
		}
	}()
	return r.run(ctx, prompt, emit)
}

func (r *Runner) run(ctx context.Context, prompt string, emit func(Event)) error {
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
			emit(Event{Kind: EventUsage, Tokens: r.measured})
		}
		calls := assistant.ToolCalls()
		if len(calls) == 0 {
			_, compactErr := r.compactIfNeeded(ctx, false, emit)
			return compactErr
		}

		for i, call := range calls {
			arguments := string(call.Function.Arguments)
			emit(Event{Kind: EventToolStart, Tool: call.Function.Name, Arguments: arguments})
			// A long-running tool publishes live display snapshots; they are
			// forwarded as coalescible events that replace the running call's
			// presentation in the transcript.
			report := func(d tools.Display) {
				emit(Event{Kind: EventToolOutput, Tool: call.Function.Name, Arguments: arguments, Display: d})
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
			emit(Event{Kind: EventToolDone, Tool: call.Function.Name, Arguments: arguments, Text: result.Content, IsError: isError, Details: result.Details})
			if ctx.Err() != nil {
				if err := r.appendInterruptedToolResults(calls[i+1:]); err != nil {
					return err
				}
				return ctx.Err()
			}
		}
	}
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
	contextMessages, err := r.session.Context()
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
	items, err := r.session.Context()
	if err != nil {
		return false, err
	}
	used, estimated := r.usageFor(items)
	if !force && used <= window-r.limits.ReserveTokens {
		return false, nil
	}

	historyStart := 1
	var previous string
	if len(items) > 1 && items[1].Summary {
		previous = projectedSummary(items[1].Message.Text())
		historyStart = 2
	}
	cut := selectCut(items, r.limits.KeepRecentTokens)
	if cut <= 1 || cut >= len(items) || items[cut].EntryID.IsZero() || items[cut].Summary {
		if estimateContext(items[min(historyStart, len(items)):], nil) < r.limits.KeepRecentTokens {
			// Everything since the last summary fits in the kept window, so
			// there is nothing older to fold away.
			return false, nil
		}
		return false, errors.New("active turn is too large to compact safely")
	}
	response, live, err := r.summarize(ctx, items, historyStart, cut, used, previous)
	// A summary cut off at its limit would be persisted and the turns it
	// replaces dropped for good, so it is refused before anything is written.
	if errors.Is(err, provider.ErrOutputLimit) || (err == nil && response.Finish == session.FinishLength) {
		return false, fmt.Errorf("compaction summary reached its %d-token limit, so older turns were kept; a lower reasoning effort leaves more of that budget for the summary", r.summaryBudget())
	}
	if err != nil {
		return false, err
	}
	// The cache-preserving request carried the whole live context plus the
	// trailing summary request, so its reported prompt size measures the
	// context being compacted far better than the byte estimate. Only the
	// small request message itself is estimated and taken back out.
	if live && estimated && response.Usage != nil {
		request := []session.ContextMessage{{Message: session.TextMessage(session.RoleUser, CompactSummaryRequest)}}
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
	emit(Event{Kind: EventUsage, Tokens: -1})
	return true, nil
}

// summaryBudget caps a compaction summary's output tokens.
func (r *Runner) summaryBudget() tokens.Count { return min(4096, r.limits.ReserveTokens/2) }

// CompactSummaryRequest is appended as the trailing user message of a
// cache-preserving compaction request. Instructions live here rather than in a
// system message because the request must reuse the live turn's exact system
// prompt and prefix to stay cacheable.
const CompactSummaryRequest = `Context is running low. Summarize the work done so far so this conversation can continue once older turns are dropped. If a previous summary appears above, update it rather than starting over. Preserve the goal, constraints, decisions, completed work, current state, important command results, file paths, and exact next steps. Omit chatter. Reply with concise Markdown using these sections: Goal, Constraints, Progress, Decisions, Next Steps, Critical Context.`

// isolatedSummaryPrompt is the system message for the fallback request used when
// the live context no longer fits the window.
const isolatedSummaryPrompt = `You are a context summarization assistant. Summarize the supplied coding-agent conversation for continuation. Preserve the goal, constraints, decisions, completed work, current state, important command results, file paths, and exact next steps. Omit chatter. Use concise Markdown with: Goal, Constraints, Progress, Decisions, Next Steps, Critical Context.`

// summarize asks the provider for a compaction summary. The live result
// reports whether the request carried the live context, making its prompt
// usage a measurement of that context.
//
// The preferred, cache-preserving form sends the live turn's exact prefix — the
// system prompt, projected prior summary, every message, and the tool roster —
// with the summary request appended as one trailing user message. Everything but
// that trailing message then reads the provider prompt cache the last streaming
// turn populated. The live prefix is only known to be unusable once the context
// has already reached the window; the isolated form is used then, and as a
// fallback if the provider still rejects the larger request as too long.
func (r *Runner) summarize(ctx context.Context, items []session.ContextMessage, historyStart, cut int, used tokens.Count, previous string) (session.Message, bool, error) {
	maxSummary := r.summaryBudget()
	if r.limits.ContextWindow <= 0 || used < r.limits.ContextWindow {
		request := make([]session.Message, 0, len(items)+1)
		for _, item := range items {
			request = append(request, item.Message)
		}
		request = append(request, session.TextMessage(session.RoleUser, CompactSummaryRequest))
		response, err := r.provider.Complete(ctx, request, r.tools.Definitions(), maxSummary)
		if err == nil || !provider.IsContextOverflow(err) {
			return response, true, err
		}
		// The prefix did not fit after all; fall through to the isolated form.
	}
	transcript := serializeForSummary(items[historyStart:cut])
	if previous != "" {
		transcript = "Previous summary:\n" + previous + "\n\nNewer conversation to merge:\n" + transcript
	}
	request := []session.Message{session.TextMessage(session.RoleSystem, isolatedSummaryPrompt), session.TextMessage(session.RoleUser, transcript)}
	response, err := r.provider.Complete(ctx, request, nil, maxSummary)
	return response, false, err
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

// projectedSummary unwraps a compaction summary from the synthetic user message
// produced by session.Store.Context.
func projectedSummary(content string) string {
	if !strings.HasPrefix(content, session.CompactionSummaryPrefix) {
		return ""
	}
	value := strings.TrimPrefix(content, session.CompactionSummaryPrefix)
	return strings.TrimSuffix(value, session.CompactionSummarySuffix)
}
