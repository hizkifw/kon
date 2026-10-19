package app

import (
	"context"
	"encoding/json"
	"strings"

	"kon.kitsu.red/core/agent"
)

// shellTimeout is the most a command the user runs from the prompt may take,
// in seconds: the longest the shell tool allows.
const shellTimeout = 600

// The text that frames a command the user ran, and its output, as the prompt
// the agent receives. The preface tells the agent the user ran it, so it does
// not take the command for one of its own.
const (
	shellPreface      = "The user ran this shell command themselves and shared its output with you.\n\n"
	shellCommandOpen  = "<user-shell-command>\n"
	shellCommandClose = "\n</user-shell-command>\n"
	shellOutputOpen   = "<user-shell-output>\n"
	shellOutputClose  = "\n</user-shell-output>"
)

// ShellRun is one command the user ran from the prompt and what it printed,
// as the shell tool reports it to the model.
type ShellRun struct {
	Command string
	Output  string
}

// ShellArguments are the shell tool's arguments for a command the user runs.
func ShellArguments(command string) json.RawMessage {
	args, _ := json.Marshal(struct {
		Command string `json:"command"`
		Timeout int    `json:"timeout"`
	}{command, shellTimeout})
	return args
}

// Shell runs command through the agent's own shell tool, so it is displayed,
// bounded, and interrupted the way the agent's commands are, then sends the
// command and its output to the agent as the user's prompt. A command the
// user interrupts is not sent: stopping it stops the turn.
func (r *Runtime) Shell(ctx context.Context, command string, inbox *agent.Inbox, emit func(agent.Event)) error {
	return r.operate(ctx, func(ctx context.Context, runner *agent.Runner) error {
		result, _ := runner.Exec(ctx, "shell", ShellArguments(command), emit)
		if err := ctx.Err(); err != nil {
			return err
		}
		prompt := shellPrompt(ShellRun{Command: command, Output: result.Content})
		return runner.RunPrompt(ctx, agent.Prompt{Text: prompt}, inbox, emit)
	})
}

// shellPrompt frames run as the prompt the agent receives.
func shellPrompt(run ShellRun) string {
	return shellPreface +
		shellCommandOpen + run.Command + shellCommandClose +
		shellOutputOpen + strings.TrimRight(run.Output, "\n") + shellOutputClose
}

// ParseShell recovers the command a prompt sent from the shell, so a replay
// can show it as it was shown live. It reports false for any other prompt.
func ParseShell(prompt string) (ShellRun, bool) {
	rest, ok := strings.CutPrefix(prompt, shellPreface+shellCommandOpen)
	if !ok {
		return ShellRun{}, false
	}
	command, rest, ok := strings.Cut(rest, shellCommandClose)
	if !ok {
		return ShellRun{}, false
	}
	rest, ok = strings.CutPrefix(rest, shellOutputOpen)
	if !ok {
		return ShellRun{}, false
	}
	output, ok := strings.CutSuffix(rest, shellOutputClose)
	if !ok {
		return ShellRun{}, false
	}
	return ShellRun{Command: command, Output: output}, true
}
