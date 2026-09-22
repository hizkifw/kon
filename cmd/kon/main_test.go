package main

import (
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

// TestRunRejectsMalformedResumeID checks that a --resume= value that is not a
// session ID is a clear error before any terminal or store work begins.
func TestRunRejectsMalformedResumeID(t *testing.T) {
	err := run([]string{"--resume=not-a-session"})
	if err == nil || !strings.Contains(err.Error(), "session ID") {
		t.Fatalf("error = %v, want an invalid session ID", err)
	}
}
