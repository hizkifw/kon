// Package agent coordinates durable messages, provider calls, tools, and compaction.
package agent

import (
	"context"
	"errors"
	"fmt"
	"path/filepath"
	"strings"

	"github.com/hizkifw/kon/internal/config"
	"github.com/hizkifw/kon/internal/provider"
	"github.com/hizkifw/kon/internal/session"
	"github.com/hizkifw/kon/internal/tools"
)

type Provider interface {
	Stream(context.Context, []session.Message, []provider.Tool, func(provider.Event)) (session.Message, error)
	Complete(context.Context, []session.Message, int) (session.Message, error)
}

type EventKind int

const (
	EventText EventKind = iota
	EventThinking
	EventAssistantDone
	EventToolStart
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
	Tokens    int
	Estimated bool
}

type Runner struct {
	contextWindow    int
	compaction       config.Compaction
	provider         Provider
	session          *session.Store
	tools            *tools.Executor
	lastUsage        *session.Usage
	lastUsageEntries int
}

func New(model config.Model, compaction config.Compaction, provider Provider, store *session.Store, executor *tools.Executor) *Runner {
	return &Runner{contextWindow: model.ContextWindowTokens, compaction: compaction, provider: provider, session: store, tools: executor}
}

// KillShell force-kills the shell command the runner is currently executing,
// if any, and reports whether a command was killed.
func (r *Runner) KillShell() bool {
	return r.tools.KillShell()
}

func SystemPrompt(cwd, instructions string) string {
	prompt := `You are kon, a concise coding agent. Work directly in the current working directory.
Use read to inspect files, edit for exact replacements, write for complete files, and shell for commands.
Every shell call must include a timeout in whole seconds (1-600); pick a realistic upper bound for the command.
Inspect relevant code before changing it. Keep tool calls focused and report the result clearly.
Tools execute without a sandbox or confirmation.`
	prompt += "\nCurrent working directory: " + filepath.Clean(cwd)
	if strings.TrimSpace(instructions) != "" {
		prompt += "\n\nAdditional user instructions:\n" + strings.TrimSpace(instructions)
	}
	return prompt
}

// Run appends prompt before any network work, then drives tool calls to a final response.
func (r *Runner) Run(ctx context.Context, prompt string, emit func(Event)) error {
	if _, err := r.session.AppendMessage(session.Message{Role: session.RoleUser, Content: prompt}); err != nil {
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
		assistant, err := r.provider.Stream(ctx, messages, tools.Definitions(), func(event provider.Event) {
			if event.Text == "" {
				return
			}
			if event.Thinking {
				emit(Event{Kind: EventThinking, Text: event.Text})
				return
			}
			emit(Event{Kind: EventText, Text: event.Text})
		})
		if err != nil && provider.IsContextOverflow(err) && !overflowRetried && r.contextWindow > 0 {
			overflowRetried = true
			compacted, compactErr := r.compactIfNeeded(ctx, true, emit)
			if compactErr != nil {
				return fmt.Errorf("provider context overflow; compaction failed: %w", compactErr)
			}
			if !compacted {
				return fmt.Errorf("provider context overflow: no safe compaction boundary")
			}
			continue
		}
		if err != nil {
			return err
		}
		if _, err := r.session.AppendMessage(assistant); err != nil {
			return err
		}
		emit(Event{Kind: EventAssistantDone})
		if assistant.Usage != nil {
			copy := *assistant.Usage
			r.lastUsage = &copy
			if current, contextErr := r.session.Context(); contextErr == nil {
				r.lastUsageEntries = len(current)
			}
			emit(Event{Kind: EventUsage, Tokens: assistant.Usage.PromptTokens + assistant.Usage.CompletionTokens})
		}
		if len(assistant.ToolCalls) == 0 {
			_, compactErr := r.compactIfNeeded(ctx, false, emit)
			return compactErr
		}

		for _, call := range assistant.ToolCalls {
			arguments := string(call.Function.Arguments)
			emit(Event{Kind: EventToolStart, Tool: call.Function.Name, Arguments: arguments})
			result, isError := r.tools.Execute(ctx, call.Function.Name, call.Function.Arguments)
			message := session.Message{
				Role: session.RoleTool, Content: result, ToolCallID: call.ID, Name: call.Function.Name,
			}
			if _, err := r.session.AppendMessage(message); err != nil {
				return err
			}
			emit(Event{Kind: EventToolDone, Tool: call.Function.Name, Arguments: arguments, Text: result, IsError: isError})
			if ctx.Err() != nil {
				return ctx.Err()
			}
		}
	}
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

func (r *Runner) compactIfNeeded(ctx context.Context, force bool, emit func(Event)) (bool, error) {
	window := r.contextWindow
	if window == 0 {
		return false, nil
	}
	items, err := r.session.Context()
	if err != nil {
		return false, err
	}
	estimatedTokens := estimateContext(items, tools.Definitions())
	used := estimatedTokens
	estimated := true
	if r.lastUsage != nil && r.lastUsageEntries == len(items) {
		reported := r.lastUsage.PromptTokens + r.lastUsage.CompletionTokens
		if reported >= used {
			used = reported
			estimated = false
		}
	}
	threshold := window - r.compaction.ReserveTokens
	if !force && used <= threshold {
		return false, nil
	}

	cut := selectCut(items, r.compaction.KeepRecentTokens)
	if cut <= 1 || cut >= len(items) || items[cut].EntryID.IsZero() {
		return false, errors.New("active turn is too large to compact safely")
	}
	transcript := serializeForSummary(items[1:cut])
	if previous := extractSummary(items[0].Message.Content); previous != "" {
		transcript = "Previous summary:\n" + previous + "\n\nNewer conversation to merge:\n" + transcript
	}
	request := []session.Message{
		{Role: session.RoleSystem, Content: `Summarize the supplied coding-agent conversation for continuation. Preserve the goal, constraints, decisions, completed work, current state, important command results, file paths, and exact next steps. Omit chatter. Use concise Markdown with: Goal, Constraints, Progress, Decisions, Next Steps, Critical Context.`},
		{Role: session.RoleUser, Content: transcript},
	}
	maxSummary := min(4096, r.compaction.ReserveTokens/2)
	response, err := r.provider.Complete(ctx, request, maxSummary)
	if err != nil {
		return false, err
	}
	summary := strings.TrimSpace(response.Content)
	if summary == "" {
		return false, errors.New("provider returned an empty compaction summary")
	}
	if _, err := r.session.AppendCompaction(summary, items[cut].EntryID, used, estimated, response.Usage); err != nil {
		return false, err
	}
	r.lastUsage = nil
	r.lastUsageEntries = 0
	emit(Event{Kind: EventCompacted, Text: summary, Tokens: used, Estimated: estimated})
	emit(Event{Kind: EventUsage, Tokens: -1})
	return true, nil
}

func estimateContext(items []session.ContextMessage, definitions []provider.Tool) int {
	bytes := 0
	for _, item := range items {
		message := item.Message
		bytes += len(message.Role) + len(message.Content) + len(message.ToolCallID.String()) + len(message.Name) + 32
		for _, call := range message.ToolCalls {
			bytes += len(call.ID.String()) + len(call.Function.Name) + len(call.Function.Arguments) + 32
		}
	}
	for _, definition := range definitions {
		bytes += len(definition.Name) + len(definition.Description) + len(definition.Parameters) + 32
	}
	return (bytes + 3) / 4
}

// selectCut keeps complete turns where possible. A turn starts at a user message.
func selectCut(items []session.ContextMessage, keepTokens int) int {
	if len(items) <= 2 {
		return -1
	}
	accumulated := 0
	candidate := len(items) - 1
	for i := len(items) - 1; i >= 1; i-- {
		accumulated += estimateContext(items[i:i+1], nil)
		candidate = i
		if accumulated >= keepTokens {
			break
		}
	}
	for candidate > 1 && items[candidate].Message.Role != session.RoleUser {
		candidate--
	}
	if candidate > 1 {
		return candidate
	}
	// A single oversized turn can split before an assistant/tool-call group.
	accumulated = 0
	for i := len(items) - 1; i >= 2; i-- {
		accumulated += estimateContext(items[i:i+1], nil)
		if accumulated >= keepTokens && items[i].Message.Role == session.RoleAssistant {
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
		if message.Name != "" {
			fmt.Fprintf(&out, " %s", message.Name)
		}
		out.WriteByte('\n')
		if message.Content != "" {
			out.WriteString(message.Content)
			out.WriteByte('\n')
		}
		for _, call := range message.ToolCalls {
			fmt.Fprintf(&out, "tool call %s: %s\n", call.Function.Name, call.Function.Arguments)
		}
		out.WriteByte('\n')
	}
	return out.String()
}

func extractSummary(system string) string {
	const start = "<conversation-summary>\n"
	const end = "\n</conversation-summary>"
	startAt := strings.LastIndex(system, start)
	if startAt < 0 {
		return ""
	}
	value := system[startAt+len(start):]
	endAt := strings.Index(value, end)
	if endAt < 0 {
		return ""
	}
	return value[:endAt]
}
