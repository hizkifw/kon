package app

import (
	"context"
	"errors"
	"fmt"
	"strings"

	"github.com/hizkifw/kon/core/agent"
	"github.com/hizkifw/kon/core/provider"
	"github.com/hizkifw/kon/core/session"
	"github.com/hizkifw/kon/internal/codetools"
)

// ErrSideChatTools lets the frontend explain an attempted tool call without
// presenting it as an executed action.
var ErrSideChatTools = errors.New("side chat cannot use tools")

// sideChatToolUnavailable answers a tool call the side chat will not run.
const sideChatToolUnavailable = "Tools are unavailable in this side chat. Answer from the conversation above, or explain what the main conversation would need to check."

const sideChatInstructions = `This is a separate side question. Answer it directly, using the conversation above as context when useful.
Tools are unavailable in this side chat, even though the main conversation has them. Do not emit tool calls or imitate tool-call syntax.
If answering requires reading new files, running a command, or checking live information, explain that limitation briefly and suggest asking in the main conversation. Do not invent results or claim to have performed an action.
Do not continue the main task. This exchange will not be added to the main conversation.`

// SideChat answers a one-off question against a snapshot of the live context.
// It may overlap Run, but never writes the session or executes tools. Its text
// and cost events belong to the side chat, not the main runner's context usage.
// Reasoning is reported only as textless thinking events: the side view shows
// that the model is thinking, not what it thought.
func (r *Runtime) SideChat(ctx context.Context, question string, emit func(agent.Event)) error {
	r.mu.Lock()
	client, messages, definitions, err := r.prepareSideChat(question)
	if err != nil {
		r.mu.Unlock()
		return err
	}
	opCtx, cancel := context.WithCancel(ctx)
	done := make(chan struct{})
	r.sideCancel, r.sideDone = cancel, done
	r.mu.Unlock()
	defer func() {
		cancel()
		r.mu.Lock()
		r.sideCancel, r.sideDone = nil, nil
		close(done)
		r.mu.Unlock()
	}()

	forward := func(event provider.Event) {
		switch {
		case emit == nil:
		case event.Retry != nil:
			emit(agent.RetryEvent(event.Retry))
		case event.Text == "":
		case event.Thinking:
			emit(agent.Event{Kind: agent.EventThinking})
		default:
			emit(agent.Event{Kind: agent.EventText, Text: event.Text})
		}
	}
	// The main conversation's tools go along to keep its prompt cache, so the
	// model may still call one; it is told they are unavailable instead.
	answer, _, err := agent.AnswerWithoutTools(messages, sideChatToolUnavailable, func(request []session.Message) (session.Message, error) {
		return client.Stream(opCtx, request, definitions, forward)
	})
	if answer.Usage != nil && emit != nil {
		// -1 means no main-context token update; the side view only consumes cost.
		emit(agent.Event{Kind: agent.EventUsage, Tokens: -1, Cost: answer.Usage.Cost})
	}
	if errors.Is(err, agent.ErrToolsUnavailable) {
		return ErrSideChatTools
	}
	return err
}

// prepareSideChat holds the runtime lock until the snapshot and provider are
// ready, so a session or model switch cannot split them across two sessions.
func (r *Runtime) prepareSideChat(question string) (*provider.Client, []session.Message, []session.ToolDefinition, error) {
	switch r.phase {
	case PhaseClosed:
		return nil, nil, nil, ErrClosed
	case PhaseFollowing:
		return nil, nil, nil, ErrReadOnly
	case PhaseNeedsConfiguration:
		return nil, nil, nil, ErrNotReady
	}
	if r.sideDone != nil {
		return nil, nil, nil, ErrBusy
	}
	if r.store == nil || r.runner == nil {
		return nil, nil, nil, ErrNotReady
	}
	if strings.TrimSpace(question) == "" {
		return nil, nil, nil, errors.New("side chat requires a question")
	}
	profile := r.active
	if !profile.resolved {
		if resolved, ok := r.resolvedSpec(profile.Name); ok {
			profile = resolved
		}
	}
	snapshot, err := agent.Context(r.store)
	if err != nil {
		return nil, nil, nil, fmt.Errorf("side chat context: %w", err)
	}
	// Detach images as well as messages before releasing the runtime lock.
	// Session switches can then close even an incognito store immediately.
	type imageResult struct {
		data []byte
		err  error
	}
	images := make(map[string]imageResult)
	messages := make([]session.Message, 0, len(snapshot)+1)
	for _, item := range snapshot {
		for _, part := range item.Message.Parts {
			if !profile.Vision || part.Type != session.PartImage {
				continue
			}
			if _, ok := images[part.ImageHash]; !ok {
				data, err := r.store.ReadImage(part.ImageHash)
				images[part.ImageHash] = imageResult{data, err}
			}
		}
		// Context repairs missing results as interrupted calls. A live main
		// task may still be executing them, so qualify only those placeholders.
		if item.EntryID.IsZero() && item.Message.Role == session.RoleTool {
			id, _ := item.Message.ToolResult()
			item.Message = session.ToolResultMessage(id, item.Message.Parts[0].ToolName, "The result was not available when this side question began. The main task may still be running this tool.")
		}
		messages = append(messages, item.Message)
	}
	// Keep every earlier message byte-identical, especially the root system
	// prompt, so this detour does not change the main conversation's prefix.
	messages = append(messages, session.TextMessage(session.RoleUser, question+"\n\n["+sideChatInstructions+"]"))
	client, err := provider.New(profile.providerSpec(), func(hash string) ([]byte, error) {
		result, ok := images[hash]
		if !ok {
			return nil, errors.New("image absent from side chat snapshot")
		}
		return result.data, result.err
	})
	if err != nil {
		return nil, nil, nil, err
	}
	return client, messages, codetools.Registry(nil).Definitions(), nil
}
