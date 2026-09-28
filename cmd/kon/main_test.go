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

// TestIncognitoRefusesResume checks that --incognito and --resume conflict in
// either order before any storage is touched: continuing a saved session would
// write to it.
func TestIncognitoRefusesResume(t *testing.T) {
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	t.Setenv("XDG_DATA_HOME", t.TempDir())
	for _, args := range [][]string{{"--incognito", "--resume"}, {"--resume=ses_x", "--incognito"}} {
		err := run(args)
		if err == nil || !strings.Contains(err.Error(), "--incognito cannot be combined with --resume") {
			t.Fatalf("%v: error = %v, want the flags to conflict", args, err)
		}
	}
}

func TestRunRejectsUnknownCommand(t *testing.T) {
	err := run([]string{"bogus"})
	if err == nil || !strings.Contains(err.Error(), `"bogus"`) {
		t.Fatalf("error = %v, want it to name the unknown command", err)
	}
}

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

// TestCommandHelp checks help dispatch: --help after a command name prints that
// command's usage instead of running it or falling back to the root index.
func TestCommandHelp(t *testing.T) {
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
