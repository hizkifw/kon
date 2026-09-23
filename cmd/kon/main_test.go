package main

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestRunNamesTheOffendingArgument guards the CLI error message: an unknown
// argument must be named by the one that failed, not by the first argument on
// the command line.
func TestRunNamesTheOffendingArgument(t *testing.T) {
	err := run([]string{"--resume", "--bogus"})
	if err == nil || !strings.Contains(err.Error(), `"--bogus"`) {
		t.Fatalf("error = %v, want it to name the offending argument", err)
	}
	if strings.Contains(err.Error(), `"--resume"`) {
		t.Fatalf("error blamed the wrong argument: %v", err)
	}
}

func TestDocsDoesNotInitializeConfig(t *testing.T) {
	configHome := t.TempDir()
	dataHome := t.TempDir()
	t.Setenv("XDG_CONFIG_HOME", configHome)
	t.Setenv("XDG_DATA_HOME", dataHome)
	if err := run([]string{"docs"}); err != nil {
		t.Fatal(err)
	}
	if _, err := os.Stat(filepath.Join(configHome, "kon", "config.json")); !os.IsNotExist(err) {
		t.Fatalf("docs initialized config: %v", err)
	}
	if _, err := os.Stat(filepath.Join(dataHome, "kon", "docs")); err != nil {
		t.Fatalf("docs were not extracted: %v", err)
	}
}

// TestRunRejectsMalformedResumeID checks that a --resume= value that is not a
// session ID is a clear error before any terminal or store work begins.
func TestRunRejectsMalformedResumeID(t *testing.T) {
	err := run([]string{"--resume=not-a-session"})
	if err == nil || !strings.Contains(err.Error(), "session ID") {
		t.Fatalf("error = %v, want an invalid session ID", err)
	}
}

func TestRunRejectsUnknownCommand(t *testing.T) {
	err := run([]string{"bogus"})
	if err == nil || !strings.Contains(err.Error(), `"bogus"`) {
		t.Fatalf("error = %v, want it to name the unknown command", err)
	}
}

// TestRootUsageListsEveryCommand guards the help index against drift: a command
// that is registered but missing from the root help is invisible to users.
func TestRootUsageListsEveryCommand(t *testing.T) {
	usage := rootUsage()
	for _, cmd := range commands() {
		if cmd.name == "" || cmd.summary == "" || cmd.synopsis == "" {
			t.Fatalf("command %+v is missing help metadata", cmd)
		}
		if !strings.Contains(usage, cmd.name) {
			t.Fatalf("root usage does not mention command %q:\n%s", cmd.name, usage)
		}
	}
}

func TestCommandHelp(t *testing.T) {
	for _, cmd := range commands() {
		help := cmd.help()
		if !strings.Contains(help, cmd.synopsis) || !strings.Contains(help, cmd.summary) {
			t.Fatalf("help for %q is missing its synopsis or summary:\n%s", cmd.name, help)
		}
		if err := run([]string{cmd.name, "--help"}); err != nil {
			t.Fatalf("%s --help: %v", cmd.name, err)
		}
	}
}
