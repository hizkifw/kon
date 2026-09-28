package main

import (
	"errors"
	"path/filepath"
	"strings"
	"testing"
)

func TestMdUsageErrorsExitWithTwo(t *testing.T) {
	for _, args := range [][]string{
		{"md", "--bogus"},
		{"md", "--width", "wide"},
		{"md", "-w", "-1"},
		{"md", "one.md", "two.md"},
	} {
		err := run(args)
		var exit *exitError
		if !errors.As(err, &exit) || exit.code != 2 {
			t.Fatalf("run(%q) error = %v, want exit status 2", args, err)
		}
	}
}

func TestMdNamesAMissingFile(t *testing.T) {
	missing := filepath.Join(t.TempDir(), "missing.md")
	if err := run([]string{"md", missing}); err == nil || !strings.Contains(err.Error(), missing) {
		t.Fatalf("error = %v, want it to name %s", err, missing)
	}
}
