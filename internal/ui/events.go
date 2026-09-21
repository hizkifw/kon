package ui

import (
	"fmt"

	"github.com/hizkifw/kon/internal/agent"
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
		m.transcript.add(block{kind: blockTool, name: event.Tool, args: sanitize(event.Arguments)})
	case agent.EventToolDone:
		m.transcript.add(toolResultBlock(event))
	case agent.EventCompacted:
		prefix := ""
		if event.Estimated {
			prefix = "~"
		}
		m.transcript.add(block{kind: blockContext, text: fmt.Sprintf("compacted %s%d tokens", prefix, event.Tokens)})
		m.status = "context compacted"
	case agent.EventUsage:
		m.contextTokens = event.Tokens
		m.contextApprox = event.Estimated
	}
	return false
}

// toolResultBlock turns a tool completion into a transcript block. Successful
// read results collapse to a line count (the full contents stay in the session
// history for the model), and shell results keep their output trimmed to a
// tail plus the exit code at display time.
func toolResultBlock(event agent.Event) block {
	b := block{
		kind: blockResult, name: event.Tool, args: sanitize(event.Arguments), failed: event.IsError,
	}
	text := sanitize(event.Text)
	switch {
	case event.IsError:
		b.text = text
	case event.Tool == "read":
		if note := readNote(text); note != "" {
			b.note = note
		} else {
			b.text = text
		}
	case event.Tool == "shell":
		code, took, output := splitExitCode(text)
		b.exit, b.took, b.text = code, took, output
		if code != "" && code != "0" {
			b.failed = true
		}
	case event.Tool == "edit" || event.Tool == "write":
		b.text = "" // the request line already carries the path and size
	default:
		b.text = text
	}
	if len(b.text) > maxResultChars {
		half := maxResultChars / 2
		b.text = b.text[:half] + "\n… display truncated …\n" + b.text[len(b.text)-half:]
	}
	return b
}
