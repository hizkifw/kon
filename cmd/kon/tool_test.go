package main

import (
	"strings"
	"testing"
)

// TestToolHelpListsEveryTool guards the index the system prompt points the
// model at: a tool missing from `kon tool --help` is one the agent never finds.
func TestToolHelpListsEveryTool(t *testing.T) {
	help := toolCommand().help()
	for _, tool := range toolCommands() {
		if tool.name == "" || tool.summary == "" || tool.synopsis == "" {
			t.Fatalf("tool %+v is missing help metadata", tool)
		}
		if !strings.Contains(help, tool.name) || !strings.Contains(help, tool.summary) {
			t.Fatalf("kon tool --help does not list %q:\n%s", tool.name, help)
		}
		if err := run([]string{"tool", tool.name, "--help"}); err != nil {
			t.Fatalf("tool %s --help: %v", tool.name, err)
		}
	}
	if err := run([]string{"tool"}); err != nil {
		t.Fatalf("kon tool without a name: %v", err)
	}
}

func TestToolRejectsUnknownTool(t *testing.T) {
	err := run([]string{"tool", "bogus"})
	if err == nil || !strings.Contains(err.Error(), `"bogus"`) {
		t.Fatalf("error = %v, want it to name the unknown tool", err)
	}
}

func TestWebfetchNeedsOneURL(t *testing.T) {
	for _, args := range [][]string{{"tool", "webfetch"}, {"tool", "webfetch", "a", "b"}} {
		if err := run(args); err == nil || !strings.Contains(err.Error(), "usage") {
			t.Fatalf("run(%q) error = %v, want a usage error", args, err)
		}
	}
}
