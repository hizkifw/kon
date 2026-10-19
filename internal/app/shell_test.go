package app

import (
	"strings"
	"testing"

	"kon.kitsu.red/core/tool"
)

func TestShellPromptRoundTrips(t *testing.T) {
	prompt := shellPrompt(ShellRun{Command: "ls", Output: "go.mod\nexit code: 0 (took 1ms)\n"})
	if !strings.Contains(prompt, "The user ran") {
		t.Fatalf("prompt does not say the user ran it: %q", prompt)
	}
	runs, rest := SplitShell(prompt)
	if rest != "" || len(runs) != 1 || runs[0].Command != "ls" || runs[0].Output != "go.mod\nexit code: 0 (took 1ms)" {
		t.Fatalf("SplitShell = %q, %q", runs, rest)
	}
}

// Steered commands lead the steering they were delivered with.
func TestSplitShellSeparatesSteering(t *testing.T) {
	call := ShellCall("false")
	first := call.Message(tool.Result{Content: "exit code: 1"})
	message := shellPrompt(ShellRun{Command: "ls", Output: "go.mod"}) + "\n\n" + first + "\n\nwhy did it fail?"
	runs, rest := SplitShell(message)
	if rest != "why did it fail?" || len(runs) != 2 || runs[1].Command != "false" || runs[1].Output != "exit code: 1" {
		t.Fatalf("SplitShell = %q, %q", runs, rest)
	}
}

func TestSplitShellLeavesOrdinaryPrompts(t *testing.T) {
	for _, prompt := range []string{"hello", "<user-shell-command>\nls", ""} {
		if runs, rest := SplitShell(prompt); len(runs) != 0 || rest != prompt {
			t.Fatalf("SplitShell(%q) = %q, %q", prompt, runs, rest)
		}
	}
	if strings.Contains(string(ShellArguments(`echo "hi"`)), `"timeout":0`) {
		t.Fatal("a user command would run as a background job")
	}
}
