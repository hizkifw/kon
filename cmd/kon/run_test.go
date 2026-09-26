package main

import (
	"errors"
	"strings"
	"testing"

	"github.com/hizkifw/kon/internal/headless"
)

func TestParseRunArgsStopsAtTheMessage(t *testing.T) {
	parsed, err := parseRunArgs([]string{"--model=fast", "--effort", "high", "-r", "fix", "the", "--model", "flag"})
	if err != nil {
		t.Fatal(err)
	}
	if parsed.model != "fast" || parsed.effort != "high" || !parsed.resume || !parsed.resumeID.IsZero() {
		t.Fatalf("parsed = %+v", parsed)
	}
	if got := strings.Join(parsed.message, " "); got != "fix the --model flag" {
		t.Fatalf("message = %q", got)
	}
	parsed, err = parseRunArgs([]string{"--format", "json", "--", "-h", "means help"})
	if err != nil || parsed.format != headless.FormatJSON || strings.Join(parsed.message, " ") != "-h means help" {
		t.Fatalf("parsed = %+v, %v", parsed, err)
	}
}

func TestParseRunArgsRejectsBadFlags(t *testing.T) {
	for _, args := range [][]string{
		{"--bogus", "hi"},
		{"--model"},
		{"--format", "yaml", "hi"},
		{"--resume=not-a-session", "hi"},
	} {
		if _, err := parseRunArgs(args); err == nil {
			t.Fatalf("%q parsed without error", args)
		}
	}
}

func TestRunPromptReadsStdinOnlyWhenAsked(t *testing.T) {
	args, err := parseRunArgs([]string{"--stdin", "review", "this"})
	if err != nil || !args.stdin {
		t.Fatalf("parse = %#v, %v", args, err)
	}
	prompt, err := args.prompt(strings.NewReader("diff --git a b\n"), true)
	if err != nil || prompt != "review this\n\ndiff --git a b" {
		t.Fatalf("prompt = %q, %v", prompt, err)
	}
	// A message without --stdin never touches stdin, which may never close.
	prompt, err = runArgs{message: []string{"hi"}}.prompt(blockingReader{t}, true)
	if err != nil || prompt != "hi" {
		t.Fatalf("message-only prompt = %q, %v", prompt, err)
	}
	prompt, err = runArgs{}.prompt(strings.NewReader("just stdin\n"), true)
	if err != nil || prompt != "just stdin" {
		t.Fatalf("stdin-only prompt = %q, %v", prompt, err)
	}
	if _, err := (runArgs{}).prompt(strings.NewReader(" \n"), true); err == nil {
		t.Fatal("an empty message was accepted")
	}
}

func TestRunUsageErrorsExitWithTwo(t *testing.T) {
	err := run([]string{"run", "--bogus"})
	var exit *exitError
	if !errors.As(err, &exit) || exit.code != 2 {
		t.Fatalf("error = %v, want exit status 2", err)
	}
}

func TestHelpStopsAtTheMessage(t *testing.T) {
	if wantsHelp([]string{"what", "does", "-h", "do"}) {
		t.Fatal("a message mentioning -h asked for help")
	}
	if !wantsHelp([]string{"--model", "-h"}) {
		t.Fatal("-h among the flags did not ask for help")
	}
}

// blockingReader fails the test if read, standing in for a stdin that stays
// open and silent.
type blockingReader struct{ t *testing.T }

func (r blockingReader) Read([]byte) (int, error) {
	r.t.Fatal("stdin was read")
	return 0, nil
}
