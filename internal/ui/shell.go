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

// runShell runs the command after the prompt's "!" in the workspace. It is
// shown the way the agent's commands are, and the agent sees it and its output
// with the next prompt.
func (m Model) runShell(text string) (tea.Model, tea.Cmd) {
	command := strings.TrimSpace(strings.TrimPrefix(text, "!"))
	if command == "" {
		m.say(toneWarn, "type a shell command after !")
		return m, nil
	}
	if m.busy() {
		// The command would run beside the agent's tools and land in the
		// middle of its turn.
		m.say(toneWarn, "wait for the turn to finish to run a shell command")
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
	m.transcript.add(shellBlock(command))
	m.resize()
	runtime := m.runtime
	cmd := m.startTurn("Running", func(ctx context.Context, emit func(agent.Event)) error {
		return runtime.Shell(ctx, command, emit)
	})
	m.turn.shell = true
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
