package app

import (
	"strings"
	"testing"
)

func TestShellPromptRoundTrips(t *testing.T) {
	prompt := shellPrompt(ShellRun{Command: "ls", Output: "go.mod\nexit code: 0 (took 1ms)\n"})
	if !strings.Contains(prompt, "The user ran") {
		t.Fatalf("prompt does not say the user ran it: %q", prompt)
	}
	run, ok := ParseShell(prompt)
	if !ok || run.Command != "ls" || run.Output != "go.mod\nexit code: 0 (took 1ms)" {
		t.Fatalf("ParseShell = %q, %v", run, ok)
	}
}

func TestParseShellLeavesOrdinaryPrompts(t *testing.T) {
	framed := shellPrompt(ShellRun{Command: "ls", Output: "go.mod"})
	for _, prompt := range []string{"hello", "<user-shell-command>\nls", "", framed + "\nmore"} {
		if _, ok := ParseShell(prompt); ok {
			t.Fatalf("ParseShell(%q) took an ordinary prompt", prompt)
		}
	}
	if strings.Contains(string(ShellArguments(`echo "hi"`)), `"timeout":0`) {
		t.Fatal("a user command would run as a background job")
	}
}
