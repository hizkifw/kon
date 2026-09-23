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
