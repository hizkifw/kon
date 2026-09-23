//go:build !windows

package tools

import (
	"errors"
	"testing"
)

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
