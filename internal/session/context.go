package session

import (
	"errors"
	"fmt"

	"github.com/hizkifw/kon/internal/typedid"
)

// InterruptedToolResult is the model-facing result synthesized for a tool call
// that never ran because its turn was cancelled or the process exited. Both the
// agent, which writes it when a turn is cancelled, and context projection, which
// recreates it for a crash, use this exact string.
const InterruptedToolResult = "not executed: interrupted"

type ContextMessage struct {
	EntryID typedid.EntryID
	Message Message
	// Summary marks a synthetic user message projected from a compaction
	// entry, holding the summary text as persisted. It carries no entry of its
	// own and is never a valid compaction cut point. How the summary is framed
	// for the model is the agent's business, not the session's.
	Summary bool
}

// Context walks parent links and applies the newest compaction on that path.
//
// The newest compaction summary is projected as a user message immediately
// after the untouched system message, followed by the retained tail and any
// messages appended after the compaction. Keeping the system prompt verbatim
// across compactions is deliberate: provider prompt caches key on a stable
// leading prefix, and folding the summary into the system message would force a
// full cache miss on every compaction.
func (s *Store) Context() ([]ContextMessage, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	path, err := s.activePathLocked()
	if err != nil {
		return nil, err
	}
	if len(path) == 0 {
		return nil, nil
	}
	latestCompaction := -1
	for i := range path {
		if path[i].Type == EntryTypeCompaction {
			latestCompaction = i
		}
	}

	var out []ContextMessage
	if latestCompaction < 0 {
		return repairUnansweredToolCalls(messagesFromEntries(path)), nil
	}
	comp := path[latestCompaction]
	if path[0].Type != EntryTypeMessage || path[0].Message == nil || path[0].Message.Role != RoleSystem {
		return nil, errors.New("session has no root system message")
	}
	out = append(out, ContextMessage{EntryID: path[0].ID, Message: *path[0].Message})

	kept := -1
	for i := 1; i < latestCompaction; i++ {
		if comp.FirstKeptEntryID != nil && path[i].ID == *comp.FirstKeptEntryID {
			kept = i
			break
		}
	}
	if kept < 0 {
		return nil, fmt.Errorf("compaction %q refers to missing entry %v", comp.ID, comp.FirstKeptEntryID)
	}
	out = append(out, ContextMessage{
		EntryID: comp.ID,
		Message: TextMessage(RoleUser, comp.Summary),
		Summary: true,
	})
	out = append(out, messagesFromEntries(path[kept:latestCompaction])...)
	out = append(out, messagesFromEntries(path[latestCompaction+1:])...)
	return repairUnansweredToolCalls(out), nil
}

// repairUnansweredToolCalls keeps projected provider context valid when a
// process exited before it could append every tool result. The durable log
// remains append-only; the synthetic result is recreated on each projection.
//
// Results are grouped with the batch they answer, in call order, because wire
// formats require a result for each call in the order the calls were made. The
// common case needs no repair and returns messages as-is.
func repairUnansweredToolCalls(messages []ContextMessage) []ContextMessage {
	// A tool result answers the batch of the assistant message that precedes
	// it. Compatible servers reuse call IDs across turns, so matching globally
	// by ID would let an earlier turn's result answer a later turn's call and
	// mask an incomplete batch.
	owner := groupToolBatches(messages)
	if !repairNeeded(messages, owner) {
		return messages
	}
	return fillUnansweredToolCalls(messages, owner)
}

// groupToolBatches maps each message to the index of the assistant tool-call
// batch it belongs to, or -1 when it belongs to none. Tool results after an
// assistant with calls join that batch; any other message closes it.
func groupToolBatches(messages []ContextMessage) []int {
	owner := make([]int, len(messages))
	batch := -1
	for i := range owner {
		owner[i] = -1
	}
	for i, item := range messages {
		switch item.Message.Role {
		case RoleAssistant:
			if len(item.Message.ToolCalls()) == 0 {
				batch = -1
				continue
			}
			batch = i
			owner[i] = i
		case RoleTool:
			owner[i] = batch
		default:
			batch = -1
		}
	}
	return owner
}

func repairNeeded(messages []ContextMessage, owner []int) bool {
	for i, item := range messages {
		if item.Message.Role != RoleAssistant || len(item.Message.ToolCalls()) == 0 {
			continue
		}
		answered := make(map[typedid.ToolCallID]bool)
		for j := i + 1; j < len(messages) && owner[j] == i; j++ {
			id, _ := messages[j].Message.ToolResult()
			answered[id] = true
		}
		for _, call := range item.Message.ToolCalls() {
			if !answered[call.ID] {
				return true
			}
		}
	}
	return false
}

// fillUnansweredToolCalls rebuilds each batch a cancelled or crashed turn left
// open, placing a result for every call in call order.
func fillUnansweredToolCalls(messages []ContextMessage, owner []int) []ContextMessage {
	results := make(map[int]map[typedid.ToolCallID]ContextMessage)
	for i, item := range messages {
		if item.Message.Role != RoleTool || owner[i] < 0 {
			continue
		}
		batch := owner[i]
		if results[batch] == nil {
			results[batch] = make(map[typedid.ToolCallID]ContextMessage)
		}
		id, _ := item.Message.ToolResult()
		results[batch][id] = item
	}
	// emitted records the results already placed with their batch, so an orphan
	// result — one whose ID matches no call in its batch — can still be emitted
	// in its original position rather than dropped.
	emitted := make(map[int]map[typedid.ToolCallID]bool)
	var out []ContextMessage
	for i, item := range messages {
		switch item.Message.Role {
		case RoleTool:
			id, _ := item.Message.ToolResult()
			if owner[i] < 0 || !emitted[owner[i]][id] {
				out = append(out, item)
			}
		case RoleAssistant:
			out = append(out, item)
			for _, call := range item.Message.ToolCalls() {
				if emitted[i] == nil {
					emitted[i] = make(map[typedid.ToolCallID]bool)
				}
				emitted[i][call.ID] = true
				if result, ok := results[i][call.ID]; ok {
					out = append(out, result)
					continue
				}
				result := ToolResultMessage(call.ID, call.Function.Name, InterruptedToolResult)
				result.IsError = true
				out = append(out, ContextMessage{Message: result})
			}
		default:
			out = append(out, item)
		}
	}
	return out
}

func messagesFromEntries(entries []Entry) []ContextMessage {
	out := make([]ContextMessage, 0, len(entries))
	for _, entry := range entries {
		if entry.Type == EntryTypeMessage && entry.Message != nil {
			out = append(out, ContextMessage{EntryID: entry.ID, Message: *entry.Message})
		}
	}
	return out
}
