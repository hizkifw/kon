package ui

import (
	"context"
	"encoding/json"
	"strings"

	tea "charm.land/bubbletea/v2"
	"kon.kitsu.red/core/agent"
	"kon.kitsu.red/internal/app"
	"kon.kitsu.red/internal/tui"
)

// shellMode reports whether the prompt is a shell command, which starts with
// "!".
func shellMode(input string) bool { return strings.HasPrefix(input, "!") }

// shellCommand is the command after a prompt's "!", and whether it is one.
func shellCommand(text string) (string, bool) {
	command, ok := strings.CutPrefix(text, "!")
	return strings.TrimSpace(command), ok
}

// runShell runs the command after the prompt's "!" in the workspace, shown the
// way the agent's commands are, then sends it and its output to the agent.
// While the agent works, the command is steered or queued instead.
func (m Model) runShell(text string) (tea.Model, tea.Cmd) {
	command, _ := shellCommand(text)
	if command == "" {
		m.say(toneWarn, "type a shell command after !")
		return m, nil
	}
	if !m.canSend() {
		return m, nil
	}
	if err := m.history.append(m.cwd, text); err != nil {
		m.say(toneDanger, "error: "+err.Error())
		return m, nil
	}
	m.input.Reset()
	return m.sendShell(command)
}

// sendShell starts a run for command, which the caller has already recorded
// in the prompt history and taken out of the input.
func (m Model) sendShell(command string) (tea.Model, tea.Cmd) {
	m.transcript.add(shellBlock(command))
	m.resize()
	runtime, inbox := m.runtime, m.inbox
	cmd := m.startTurn("Working", func(ctx context.Context, emit func(agent.Event)) error {
		return runtime.Shell(ctx, command, inbox, emit)
	})
	m.viewport.GotoBottom()
	return m, cmd
}

// shellBlock is the prompt line a shell command leaves in the transcript.
func shellBlock(command string) block {
	return block{kind: blockUser, text: tui.Sanitize("!" + command)}
}

// replayShell shows a command the user ran as it was shown live: the prompt
// line, then the shell tool's call and result.
func (m *Model) replayShell(t *transcript, run app.ShellRun) {
	args := app.ShellArguments(run.Command)
	output := tui.Sanitize(run.Output)
	t.add(shellBlock(run.Command))
	t.add(m.toolBlock("shell", tui.Sanitize(string(args))))
	// A command that never finished reports why in place of its exit code.
	failed := strings.HasPrefix(output, "error: ")
	display := m.runtime.DescribeTool("shell", json.RawMessage(args), output, failed, nil)
	t.add(block{kind: blockResult, name: "shell", display: display})
}
