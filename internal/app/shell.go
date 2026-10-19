package app

import (
	"context"
	"encoding/json"
	"strings"

	"kon.kitsu.red/core/agent"
	"kon.kitsu.red/core/tool"
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

// ShellCall is command as a call the user steers with while the agent works:
// the runner runs it at its next request and sends it framed as Shell does.
func ShellCall(command string) agent.Call {
	return agent.Call{
		Tool:      "shell",
		Arguments: ShellArguments(command),
		Message: func(result tool.Result) string {
			return shellPrompt(ShellRun{Command: command, Output: result.Content})
		},
	}
}

// shellPrompt frames run as the prompt the agent receives.
func shellPrompt(run ShellRun) string {
	return shellPreface +
		shellCommandOpen + run.Command + shellCommandClose +
		shellOutputOpen + strings.TrimRight(run.Output, "\n") + shellOutputClose
}

// SplitShell takes the commands the user ran from the front of a user
// message, so a replay can show them as they were shown live, and returns
// the rest: steering sent along with them. A message that does not start
// with one is returned whole.
func SplitShell(message string) ([]ShellRun, string) {
	var runs []ShellRun
	for {
		rest, ok := strings.CutPrefix(message, shellPreface+shellCommandOpen)
		if !ok {
			return runs, message
		}
		command, rest, ok := strings.Cut(rest, shellCommandClose)
		if !ok {
			return runs, message
		}
		rest, ok = strings.CutPrefix(rest, shellOutputOpen)
		if !ok {
			return runs, message
		}
		output, rest, ok := strings.Cut(rest, shellOutputClose)
		if !ok {
			return runs, message
		}
		runs = append(runs, ShellRun{Command: command, Output: output})
		// Messages delivered together are joined by a blank line.
		message = strings.TrimPrefix(rest, "\n\n")
	}
}
