package app

import (
	"strings"
	"testing"
)

func TestShellRunsLeadNextPromptOnce(t *testing.T) {
	r := &Runtime{shell: []ShellRun{{Command: "ls", Output: "go.mod\nexit code: 0 (took 1ms)\n"}, {Command: "false", Output: "exit code: 1 (took 1ms)"}}}
	framed := r.takeShell()
	if r.takeShell() != "" {
		t.Fatal("shell runs were sent twice")
	}
	runs, rest := SplitShell(framed + "why did false fail?")
	if rest != "why did false fail?" || len(runs) != 2 {
		t.Fatalf("runs = %q, rest = %q", runs, rest)
	}
	if runs[0].Command != "ls" || runs[0].Output != "go.mod\nexit code: 0 (took 1ms)" || runs[1].Command != "false" {
		t.Fatalf("runs = %q", runs)
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
