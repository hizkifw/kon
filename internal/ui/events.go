package ui

import (
	"encoding/json"

	"github.com/hizkifw/kon/internal/agent"
	"github.com/hizkifw/kon/internal/session"
	"github.com/hizkifw/kon/internal/tokens"
	"github.com/hizkifw/kon/internal/tools"
	"github.com/hizkifw/kon/internal/typedid"
)

// maxResultChars bounds tool result text kept in the transcript. Display
// formatting trims it further; the full result stays in the session history.
const maxResultChars = 4000

// applyAgentEvent returns true for text and thinking deltas, which may be
// coalesced before repainting. Structural events repaint immediately.
func (m *Model) applyAgentEvent(event agent.Event) bool {
	switch event.Kind {
	case agent.EventText:
		m.transcript.appendStream(sanitize(event.Text))
		m.status = "streaming…"
		return true
	case agent.EventThinking:
		m.transcript.appendThinking(sanitize(event.Text))
		m.status = "thinking…"
		return true
	case agent.EventAssistantDone:
		m.transcript.finishStream()
	case agent.EventToolStart:
		m.status = "running " + event.Tool + "…"
		m.transcript.add(m.toolBlock(event.Tool, sanitize(event.Arguments)))
	case agent.EventToolOutput:
		// The running tool's own display snapshot. Events arrive one at a time
		// from the run channel, so the snapshot simply replaces the previous
		// one for the call, which is still the transcript's last tool block.
		m.transcript.updateToolLive(event.Display)
		m.status = "running " + event.Tool + "…"
	case agent.EventToolDone:
		m.status = ""
		m.transcript.add(m.toolResultBlock(event))
	case agent.EventCompacted:
		m.transcript.add(block{kind: blockContext, text: compactedLabel(event.Tokens, event.Estimated)})
		m.status = "context compacted"
	case agent.EventUsage:
		m.contextTokens = event.Tokens
		m.contextApprox = event.Estimated
	}
	return false
}

// toolBlock builds the request block for a starting tool call. The display is
// resolved through the owning tool so the request line renders even before
// any result exists.
func (m *Model) toolBlock(name string, args string) block {
	b := block{kind: blockTool, name: name, args: args}
	b.display = tools.Describe(name, []byte(args), "", false, nil, m.cwd)
	return b
}

// toolResultBlock turns a tool completion into a transcript block by asking
// the owning tool for its display. Truncation to maxResultChars protects the
// UI from pathological results; the model and the session history keep the
// full text.
func (m *Model) toolResultBlock(event agent.Event) block {
	text := sanitize(event.Text)
	display := tools.Describe(event.Tool, []byte(sanitize(event.Arguments)), text, event.IsError, event.Details, m.cwd)
	if len(text) > maxResultChars {
		for i, line := range display.Lines {
			if len(line) > maxResultChars {
				half := maxResultChars / 2
				display.Lines[i] = line[:half] + "… display truncated …" + line[len(line)-half:]
			}
		}
	}
	return block{
		kind:    blockResult,
		name:    event.Tool,
		args:    sanitize(event.Arguments),
		display: display,
	}
}

// applyHistory replays an opened session's active path into the transcript so a
// resumed conversation is visible before the next prompt. It mirrors the live
// event stream: user and assistant messages, thinking parts, tool calls paired
// with their results, the compacted marker left by each compaction, and each
// turn's recorded duration. Model changes are structural and are not echoed
// here. Tool displays are resolved through the owning tools with the call's
// persisted arguments, so replay looks exactly like the live rendering.
func (m *Model) applyHistory(entries []session.Entry) {
	m.applyHistoryTo(&m.transcript, entries)
}

// applyHistoryTo replays entries into the given transcript. A resume preview
// renders into a scratch transcript via the same path as a real resume, so the
// preview looks exactly like the session would once opened.
func (m *Model) applyHistoryTo(t *transcript, entries []session.Entry) {
	var r replayState
	m.replay(t, &r, entries)
	r.finish(t)
}

// replayState is what replay carries from one entry to the next. A followed
// session is replayed in batches as its writer appends, so the state outlives
// a single call.
type replayState struct {
	// callArgs maps a tool call ID to its persisted arguments so a tool
	// result resolves its display from the same arguments the call was made
	// with.
	callArgs map[typedid.ToolCallID]json.RawMessage
	// open marks a turn start without its end yet. The end closes it with the
	// duration the runner measured, which is shown as the same "Worked for …"
	// marker a live turn leaves behind.
	open bool
	// selected is the latest model selection on the path. The first one is
	// the model the session started on, which the header already names. Each
	// later one that picks a different model is a switch, shown the way a live
	// switch was.
	selected *session.ModelSelection
}

// finish closes a replay. A turn still open at the end of the history belongs
// to a process that died mid-turn.
func (r *replayState) finish(t *transcript) {
	if r.open {
		t.add(block{kind: blockElapsed, text: stoppedLabel})
	}
	r.open = false
}

// replay adds entries to the transcript, continuing from r. It leaves a turn
// open at the end, which only finish may call stopped: a followed session's
// last turn may still be running in its writer.
func (m *Model) replay(t *transcript, r *replayState, entries []session.Entry) {
	if r.callArgs == nil {
		r.callArgs = make(map[typedid.ToolCallID]json.RawMessage)
	}
	for _, entry := range entries {
		switch entry.Type {
		case session.EntryTypeTurnStart:
			// A start that opens while another is still open follows a
			// process that died mid-turn.
			r.finish(t)
			r.open = true
			continue
		case session.EntryTypeTurnEnd:
			// An end is shown even without its start, which a preview's window
			// can cut off: the duration it carries needs nothing else.
			r.open = false
			t.add(block{kind: blockElapsed, text: workedLabel(entry.TurnDuration())})
			continue
		case session.EntryTypeModelChange:
			if entry.Model == nil {
				continue
			}
			selection := *entry.Model
			if r.selected != nil && (selection.Name != r.selected.Name || selection.ExternalID != r.selected.ExternalID) {
				t.add(block{kind: blockModel, text: modelChangedText(m.runtime.DescribeSelection(selection)), model: &selection})
			}
			r.selected = &selection
			continue
		case session.EntryTypeCompaction:
			// A compaction entry has no message; it is echoed as the same
			// marker the live run emitted so a resumed transcript shows where
			// the context was folded.
			t.add(block{kind: blockContext, text: compactedLabel(entry.TokensBefore, entry.TokensBeforeEstimated)})
			continue
		}
		if entry.Message == nil {
			continue
		}
		switch entry.Message.Role {
		case session.RoleUser:
			t.add(block{kind: blockUser, text: sanitize(entry.Message.Text())})
		case session.RoleAssistant:
			for _, part := range entry.Message.Parts {
				switch part.Type {
				case session.PartReasoning:
					if part.Text != "" {
						t.add(block{kind: blockThinking, text: sanitize(part.Text)})
					}
				case session.PartText:
					if part.Text != "" {
						t.add(block{kind: blockAssistant, text: sanitize(part.Text)})
					}
				case session.PartToolCall:
					r.callArgs[part.ToolCallID] = part.ToolInput
					t.add(m.toolBlock(part.ToolName, sanitize(string(part.ToolInput))))
				}
			}
		case session.RoleTool:
			// The display comes from the owning tool, resolved against the
			// persisted content and the call's arguments, so a resumed
			// transcript renders exactly like the live one did.
			id, name := entry.Message.ToolResult()
			display := m.runtime.DescribeTool(name, r.callArgs[id], sanitize(entry.Message.Text()), entry.Message.IsError, entry.Message.Details)
			t.add(block{kind: blockResult, name: name, display: display})
		}
	}
}

// compactedLabel is the transcript marker for a compaction. It is built in one
// place so the live event and the replayed entry render identically, and so a
// resumed session shows the same marker the run did.
func compactedLabel(count tokens.Count, estimated bool) string {
	prefix := ""
	if estimated {
		prefix = "~"
	}
	return "compacted " + prefix + count.String() + " tokens"
}
