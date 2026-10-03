package prompt

import (
	"strings"
	"testing"

	"kon.kitsu.red/internal/contextfiles"
)

func TestSystemRendersContextFilesInOrder(t *testing.T) {
	prompt := System("", "/work/project", []contextfiles.File{
		{Path: "/work/AGENTS.md", Content: "outer rules\n"},
		{Path: "/work/project/AGENTS.md", Content: "inner rules"},
	}, false)
	if !strings.Contains(prompt, "<instructions path=\"/work/AGENTS.md\">\nouter rules\n</instructions>") {
		t.Fatalf("outer file not rendered with its path:\n%s", prompt)
	}
	outer := strings.Index(prompt, "outer rules")
	inner := strings.Index(prompt, "inner rules")
	if outer < 0 || inner < 0 || outer > inner {
		t.Fatalf("context files not rendered outermost first:\n%s", prompt)
	}
	if cwd := strings.Index(prompt, "Current working directory: /work/project"); cwd < 0 || cwd < inner {
		t.Fatalf("cwd should follow the context files:\n%s", prompt)
	}
}

func TestSystemOmitsContextSectionWhenEmpty(t *testing.T) {
	prompt := System("", "/work", nil, false)
	if strings.Contains(prompt, "<instructions") || strings.Contains(prompt, "User and project instructions") {
		t.Fatalf("empty context files produced a section:\n%s", prompt)
	}
}

func TestSystemPointsToBundledDocs(t *testing.T) {
	prompt := System("", "/work", nil, false)
	if !strings.Contains(prompt, "`kon docs`") {
		t.Fatalf("kon docs missing from prompt:\n%s", prompt)
	}
}

func TestSystemPointsToShellTools(t *testing.T) {
	prompt := System("", "/work", nil, false)
	if !strings.Contains(prompt, "`kon tool webfetch <url>`") || !strings.Contains(prompt, "`kon tool --help`") {
		t.Fatalf("shell tools missing from prompt:\n%s", prompt)
	}
	if strings.Contains(prompt, "websearch") {
		t.Fatalf("prompt names web search with no provider configured:\n%s", prompt)
	}
}

func TestSystemNamesWebSearchWhenConfigured(t *testing.T) {
	prompt := System("", "/work", nil, true)
	for _, want := range []string{"`kon tool websearch <query>`", "`kon tool webfetch <url>`", "`kon tool --help`"} {
		if !strings.Contains(prompt, want) {
			t.Fatalf("%s missing from prompt:\n%s", want, prompt)
		}
	}
}

func TestSystemReplacesBaseWhenGiven(t *testing.T) {
	prompt := System("custom rules\n", "/work", []contextfiles.File{{Path: "/work/AGENTS.md", Content: "project rules"}}, true)
	if !strings.HasPrefix(prompt, "custom rules\n\nUser and project instructions:") {
		t.Fatalf("custom base not used as the prefix:\n%s", prompt)
	}
	if strings.Contains(prompt, "`kon docs`") || strings.Contains(prompt, "kon tool") {
		t.Fatalf("built-in prompt kept alongside the custom base:\n%s", prompt)
	}
	if !strings.HasSuffix(prompt, "Current working directory: /work") {
		t.Fatalf("cwd missing after the custom base:\n%s", prompt)
	}
}

func TestSystemRendersInlineInstructionsWithoutPath(t *testing.T) {
	prompt := System("", "/work", []contextfiles.File{{Content: "be terse"}}, false)
	if !strings.Contains(prompt, "\n\n<instructions>\nbe terse\n</instructions>") {
		t.Fatalf("inline instructions not rendered as a bare block:\n%s", prompt)
	}
}
