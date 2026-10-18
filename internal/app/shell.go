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

// The tags that frame a command the user ran, and its output, in the prompt
// the agent next receives.
const (
	shellCommandOpen  = "<user-shell-command>\n"
	shellCommandClose = "\n</user-shell-command>\n"
	shellOutputOpen   = "<user-shell-output>\n"
	shellOutputClose  = "\n</user-shell-output>\n\n"
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
// bounded, and interrupted the way the agent's commands are. It occupies the
// busy phase like a turn but sends nothing to the model: the command and its
// output wait to be sent ahead of the next prompt, so the agent sees what the
// user saw without a turn of its own. Output from a command cut short is kept
// too, since the user saw it.
func (r *Runtime) Shell(ctx context.Context, command string, emit func(agent.Event)) error {
	return r.operate(ctx, func(ctx context.Context, runner *agent.Runner) error {
		result, _ := runner.Exec(ctx, "shell", ShellArguments(command), emit)
		r.mu.Lock()
		r.shell = append(r.shell, ShellRun{Command: command, Output: result.Content})
		r.mu.Unlock()
		return ctx.Err()
	})
}

// takeShell removes the commands run since the last prompt and returns them
// framed for the next one, or "" when there were none.
func (r *Runtime) takeShell() string {
	r.mu.Lock()
	runs := r.shell
	r.shell = nil
	r.mu.Unlock()
	var b strings.Builder
	for _, run := range runs {
		b.WriteString(shellCommandOpen + run.Command + shellCommandClose)
		b.WriteString(shellOutputOpen + strings.TrimRight(run.Output, "\n") + shellOutputClose)
	}
	return b.String()
}

// SplitShell takes the commands the user ran from the front of a prompt, so a
// replay can show them as they were shown live, and returns the rest. A
// prompt that does not start with one is returned whole.
func SplitShell(prompt string) ([]ShellRun, string) {
	var runs []ShellRun
	for {
		rest, ok := strings.CutPrefix(prompt, shellCommandOpen)
		if !ok {
			return runs, prompt
		}
		command, rest, ok := strings.Cut(rest, shellCommandClose)
		if !ok {
			return runs, prompt
		}
		rest, ok = strings.CutPrefix(rest, shellOutputOpen)
		if !ok {
			return runs, prompt
		}
		output, rest, ok := strings.Cut(rest, shellOutputClose)
		if !ok {
			return runs, prompt
		}
		runs = append(runs, ShellRun{Command: command, Output: output})
		prompt = rest
	}
}
