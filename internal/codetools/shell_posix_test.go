//go:build !windows

package codetools

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"
)

func TestMain(m *testing.M) {
	// The shell tests use POSIX syntax, which the developer's $SHELL (fish, say) may reject.
	os.Setenv("SHELL", "/bin/sh")
	// The display registry's package initialization already resolved the shell.
	shellBackendOnce = sync.Once{}
	os.Exit(m.Run())
}

func TestResolveShellForPrefersConfiguredShell(t *testing.T) {
	found := func(path string) func(string) (string, error) {
		return func(name string) (string, error) {
			if name == "/usr/bin/fish" {
				return path, nil
			}
			return "", errors.New("not found")
		}
	}
	got := resolveShellFor("/usr/bin/fish", found("/usr/bin/fish"))
	if got.path != "/usr/bin/fish" || got.name != "fish" {
		t.Fatalf("configured shell = %+v", got)
	}
	if len(got.args) != 1 || got.args[0] != "-c" {
		t.Fatalf("configured shell args = %v", got.args)
	}
}

func TestResolveShellForFallsBackToSh(t *testing.T) {
	missing := func(string) (string, error) { return "", errors.New("not found") }
	cases := map[string]shellBackend{
		"empty":     resolveShellFor("", missing),
		"not found": resolveShellFor("/opt/nope", missing),
	}
	for name, got := range cases {
		if got.path != "/bin/sh" || got.name != "/bin/sh" {
			t.Fatalf("%s: fallback = %+v", name, got)
		}
	}
}

// The model writes commands in the syntax of the interpreter the description
// names, so that must be the interpreter Run executes. The shell is linked
// under a name found nowhere else in the description; "sh" would match the
// word "shell" whatever the description named.
func TestShellDescriptionNamesTheInterpreterRunUses(t *testing.T) {
	link := filepath.Join(t.TempDir(), "konsh")
	if err := os.Symlink("/bin/sh", link); err != nil {
		t.Fatal(err)
	}
	t.Setenv("SHELL", link)
	shellBackendOnce = sync.Once{}
	// Resolution is lazy, so later tests resolve the restored $SHELL again.
	t.Cleanup(func() { shellBackendOnce = sync.Once{} })
	if description := (&shellTool{}).Definition().Description; !strings.Contains(description, "konsh") {
		t.Fatalf("shell description does not name the interpreter konsh: %q", description)
	}
	result, failed := newExecutor(t.TempDir(), false, nil).Execute(context.Background(), "shell", raw(map[string]any{"command": `echo "$0"`, "timeout": 10}), nil)
	if failed || !strings.Contains(result.Content, link) {
		t.Fatalf("shell ran as %q (failed=%v), want the interpreter the description names", result.Content, failed)
	}
}
