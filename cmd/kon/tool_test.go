package main

import (
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"kon.kitsu.red/internal/config"
	"kon.kitsu.red/internal/websearch"
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

func TestWebsearchNeedsAQuery(t *testing.T) {
	for _, args := range [][]string{{"tool", "websearch"}, {"tool", "websearch", "-n", "3"}} {
		if err := run(args); err == nil || !strings.Contains(err.Error(), "usage") {
			t.Fatalf("run(%q) error = %v, want a usage error", args, err)
		}
	}
}

// TestWebsearchIsOffUntilConfigured covers both unconfigured states, a config
// with no provider and no config at all: each names the field to set.
func TestWebsearchIsOffUntilConfigured(t *testing.T) {
	root := t.TempDir()
	t.Setenv("XDG_CONFIG_HOME", root)
	check := func() {
		t.Helper()
		err := run([]string{"tool", "websearch", "go", "generics"})
		if err == nil || !strings.Contains(err.Error(), "web search is off") || !strings.Contains(err.Error(), "web_search.provider") {
			t.Fatalf("error = %v, want it to say web search is off and name the field", err)
		}
	}
	check()
	dir := filepath.Join(root, "kon")
	if err := os.MkdirAll(dir, 0o700); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, "config.json"), []byte(`{"models": [], "web_search": {"provider": "", "providers": {"brave": {"api_key": "k"}}}}`), 0o600); err != nil {
		t.Fatal(err)
	}
	check()
}

func TestSearchWebPrintsResults(t *testing.T) {
	results := `{"web": {"results": [{"title": "Go", "url": "https://go.dev/", "description": "The Go site."}]}}`
	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Write([]byte(results))
	}))
	defer server.Close()
	search := config.WebSearch{Provider: "brave", Providers: map[string]config.WebSearchProvider{"brave": {APIKey: "k", BaseURL: server.URL}}}
	query := websearch.Query{Text: "go", Count: websearch.DefaultCount}

	var out strings.Builder
	if err := searchWeb(search, "config.json", query, &out); err != nil {
		t.Fatal(err)
	}
	if want := "1. Go\n   https://go.dev/\n   The Go site.\n"; out.String() != want {
		t.Fatalf("output = %q, want %q", out.String(), want)
	}

	results = `{}`
	out.Reset()
	if err := searchWeb(search, "config.json", query, &out); err != nil || out.String() != "no results\n" {
		t.Fatalf("output = %q, err = %v, want no results", out.String(), err)
	}
}
