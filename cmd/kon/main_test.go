package main

import (
	"context"
	"errors"
	"io"
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

// The expected help is written out rather than rebuilt from commands(), so a
// command, summary, or flag list that drops out of the printed text fails the
// comparison instead of vanishing from both sides of it.
const rootHelp = `usage: kon [--resume [<id>]] [--help] [--version]
       kon <command> [flags]

Start a full-screen kon agent session in the current directory.

  --resume, -r          resume the most recent session in this directory
  --resume=<id>         resume a specific session
  --help, -h            show this help
  --version             print the version

commands:
  run      send one prompt without the full-screen UI
  docs     extract the bundled product guide
  models   list bundled or cached model IDs offline
  upgrade  install the latest kon release

Run "kon <command> --help" for command flags.

On exit, kon prints the session ID so the session can be resumed later.
`

const runHelp = `usage: kon run [flags] [message...]

send one prompt without the full-screen UI

Send one prompt in the current directory and stream the reply to stdout.
The message is the arguments after the flags, or piped stdin when there
are none. With --stdin, stdin is appended to the arguments after a blank line.
Tools run without confirmation, as they do in the full-screen UI.

  --model <name>         use this model for this run; the config is not changed
  --effort <level>       use this reasoning effort for this run
  --resume, -r           continue the most recent session in this directory
  --resume=<id>          continue a specific session
  --format text|json     stream text (default), or write one JSON event per line
  --stdin                append stdin to the message

Exit status is 0 when the turn completes, 1 on error, 2 on a usage error,
and 130 when interrupted.
`

// stdoutOf runs the CLI with args and returns what it printed. Help goes
// straight to os.Stdout, so the test swaps in a pipe for the call.
func stdoutOf(t *testing.T, args ...string) string {
	t.Helper()
	r, w, err := os.Pipe()
	if err != nil {
		t.Fatal(err)
	}
	printed := make(chan string)
	go func() {
		b, _ := io.ReadAll(r)
		r.Close()
		printed <- string(b)
	}()
	stdout := os.Stdout
	os.Stdout = w
	runErr := run(args)
	os.Stdout = stdout
	w.Close()
	out := <-printed
	if runErr != nil {
		t.Fatalf("kon %s: %v", strings.Join(args, " "), runErr)
	}
	return out
}

// TestRootUsageListsEveryCommand guards the help index against drift: a command
// that is registered but missing from the root help is invisible to users.
func TestRootUsageListsEveryCommand(t *testing.T) {
	for _, cmd := range commands() {
		if cmd.name == "" || cmd.summary == "" || cmd.synopsis == "" {
			t.Fatalf("command %+v is missing help metadata", cmd)
		}
	}
	if got := stdoutOf(t, "--help"); got != rootHelp {
		t.Fatalf("kon --help printed:\n%s\nwant:\n%s", got, rootHelp)
	}
}

func TestCommandHelp(t *testing.T) {
	if got := stdoutOf(t, "run", "--help"); got != runHelp {
		t.Fatalf("kon run --help printed:\n%s\nwant:\n%s", got, runHelp)
	}
	// Every command answers --help with its own usage, not an error or the
	// root index.
	for _, cmd := range commands() {
		if got := stdoutOf(t, cmd.name, "--help"); !strings.HasPrefix(got, "usage: kon "+cmd.name) {
			t.Fatalf("kon %s --help printed:\n%s", cmd.name, got)
		}
	}
}

// stubCatalogRefresh replaces the models.dev refresh for one test and records
// the cache path it was asked to write.
func stubCatalogRefresh(t *testing.T, err error) *string {
	t.Helper()
	var cachePath string
	original := refreshCatalog
	refreshCatalog = func(_ context.Context, path string) error {
		cachePath = path
		return err
	}
	t.Cleanup(func() { refreshCatalog = original })
	return &cachePath
}

// TestUpgradeFinalizeMigratesStorage checks the entrypoint earlier releases
// invoke on their replacement: it must accept no input, bring storage to this
// binary's version, and refresh the catalog cache in the data directory.
func TestUpgradeFinalizeMigratesStorage(t *testing.T) {
	configHome := t.TempDir()
	dataHome := t.TempDir()
	t.Setenv("XDG_CONFIG_HOME", configHome)
	t.Setenv("XDG_DATA_HOME", dataHome)
	refreshed := stubCatalogRefresh(t, nil)
	if err := run([]string{"upgrade", "--finalize"}); err != nil {
		t.Fatal(err)
	}
	if _, err := os.Stat(filepath.Join(dataHome, "kon", "storage-version")); err != nil {
		t.Fatalf("finalize did not record a storage version: %v", err)
	}
	if !strings.HasPrefix(*refreshed, filepath.Join(dataHome, "kon")) {
		t.Fatalf("catalog refresh path = %q, want one under the data directory", *refreshed)
	}
	if _, err := os.Stat(filepath.Join(configHome, "kon", "config.json")); !os.IsNotExist(err) {
		t.Fatalf("finalize initialized config: %v", err)
	}
}

// TestUpgradeFinalizeToleratesRefreshFailure keeps an offline upgrade
// successful: the binary is installed and migrated before the refresh runs.
func TestUpgradeFinalizeToleratesRefreshFailure(t *testing.T) {
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	t.Setenv("XDG_DATA_HOME", t.TempDir())
	stubCatalogRefresh(t, errors.New("offline"))
	if err := run([]string{"upgrade", "--finalize"}); err != nil {
		t.Fatalf("finalize failed on a refresh error: %v", err)
	}
}

func TestUpgradeRejectsArguments(t *testing.T) {
	for _, args := range [][]string{
		{"upgrade", "--finalize", "v0.1.0"},
		{"upgrade", "--check", "--finalize"},
		{"upgrade", "--from=v0.1.0"},
	} {
		if err := run(args); err == nil {
			t.Fatalf("run(%q) succeeded, want an error", args)
		}
	}
}

// TestUpgradeRefusesDevelopmentBuild runs before any network request: a
// source build has no release version to compare against.
func TestUpgradeRefusesDevelopmentBuild(t *testing.T) {
	err := run([]string{"upgrade", "--check"})
	if err == nil || !strings.Contains(err.Error(), "development build") {
		t.Fatalf("error = %v, want a development build refusal", err)
	}
}
