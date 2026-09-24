package ui

import (
	"testing"

	tea "charm.land/bubbletea/v2"
	"github.com/hizkifw/kon/internal/history"
)

// searchModel returns a model whose prompt history holds the given entries,
// newest last, matching the order they are loaded and appended.
func searchModel(t testing.TB, prompts ...string) Model {
	t.Helper()
	entries := make([]history.Entry, len(prompts))
	for i, text := range prompts {
		entries[i] = history.Entry{Text: text}
	}
	m := newTestModel(t)
	m.history = newPromptHistory(m.history.store, entries)
	return m
}

func TestCtrlROpensReverseSearchShowingNoResult(t *testing.T) {
	model := searchModel(t, "go build ./...", "make check", "go test ./...")
	model.input.SetValue("draft")
	updated, cmd := model.Update(tea.KeyPressMsg{Code: 'r', Mod: tea.ModCtrl})
	model = updated.(Model)
	if cmd != nil {
		t.Fatalf("ctrl+r returned a command: %v", cmd)
	}
	if model.search == nil {
		t.Fatal("ctrl+r did not open the reverse search")
	}
	// An empty query shows no result, like bash; the draft stays in the prompt
	// until a match replaces it.
	if got := model.input.Value(); got != "draft" {
		t.Fatalf("empty search changed the prompt: %q", got)
	}
	if model.status != "(reverse-i-search)`'" {
		t.Fatalf("status = %q", model.status)
	}
	// The search is prompt-and-status only: no popup list, like bash.
	if model.menu.open() {
		t.Fatalf("reverse search opened a popup with %d rows", len(model.menu.items))
	}
}

func TestReverseSearchNarrowsToQuery(t *testing.T) {
	model := searchModel(t, "go build ./...", "make check", "go test ./...")
	updated, _ := model.Update(tea.KeyPressMsg{Code: 'r', Mod: tea.ModCtrl})
	model = updated.(Model)
	// Type "make": only the middle entry matches.
	for _, r := range "make" {
		updated, _ = model.Update(tea.KeyPressMsg{Code: r, Text: string(r)})
		model = updated.(Model)
	}
	if got := model.input.Value(); got != "make check" {
		t.Fatalf("match after typing query = %q", got)
	}
	if model.status != "(reverse-i-search)`make'" {
		t.Fatalf("status = %q", model.status)
	}
}

func TestReverseSearchCyclesMatches(t *testing.T) {
	model := searchModel(t, "go build ./...", "go test ./...", "go vet ./...")
	updated, _ := model.Update(tea.KeyPressMsg{Code: 'r', Mod: tea.ModCtrl})
	model = updated.(Model)
	for _, r := range "go " {
		updated, _ = model.Update(tea.KeyPressMsg{Code: r, Text: string(r)})
		model = updated.(Model)
	}
	if got := model.input.Value(); got != "go vet ./..." {
		t.Fatalf("first match = %q", got)
	}
	// Ctrl+R again advances to the next-older match.
	updated, _ = model.Update(tea.KeyPressMsg{Code: 'r', Mod: tea.ModCtrl})
	model = updated.(Model)
	if got := model.input.Value(); got != "go test ./..." {
		t.Fatalf("cycled match = %q", got)
	}
}

func TestReverseSearchEscRestoresDraft(t *testing.T) {
	model := searchModel(t, "make check")
	model.input.SetValue("half typed")
	updated, _ := model.Update(tea.KeyPressMsg{Code: 'r', Mod: tea.ModCtrl})
	model = updated.(Model)
	updated, _ = model.Update(tea.KeyPressMsg{Code: tea.KeyEscape})
	model = updated.(Model)
	if model.search != nil {
		t.Fatal("esc did not close the search")
	}
	if got := model.input.Value(); got != "half typed" {
		t.Fatalf("esc did not restore the draft: %q", got)
	}
}

func TestReverseSearchNoMatchKeepsPrompt(t *testing.T) {
	model := searchModel(t, "make check")
	model.input.SetValue("draft")
	updated, _ := model.Update(tea.KeyPressMsg{Code: 'r', Mod: tea.ModCtrl})
	model = updated.(Model)
	// A query with no match must not clear the draft being typed.
	updated, _ = model.Update(tea.KeyPressMsg{Code: 'z', Text: "z"})
	model = updated.(Model)
	if got := model.status; got != "(failed reverse-i-search)`z'" {
		t.Fatalf("status = %q", got)
	}
	if got := model.input.Value(); got != "draft" {
		t.Fatalf("no-match cleared the prompt: %q", got)
	}
}

func TestReverseSearchEnterAccepts(t *testing.T) {
	model := searchModel(t, "go build ./...", "make check")
	updated, _ := model.Update(tea.KeyPressMsg{Code: 'r', Mod: tea.ModCtrl})
	model = updated.(Model)
	for _, r := range "make" {
		updated, _ = model.Update(tea.KeyPressMsg{Code: r, Text: string(r)})
		model = updated.(Model)
	}
	updated, _ = model.Update(tea.KeyPressMsg{Code: tea.KeyEnter})
	model = updated.(Model)
	if model.search != nil {
		t.Fatal("enter did not close the search")
	}
	if got := model.input.Value(); got != "make check" {
		t.Fatalf("accepted prompt = %q", got)
	}
	if model.busy {
		t.Fatal("enter during search submitted the prompt")
	}
}

func TestReverseSearchUpIsOlderAndDownIsNewer(t *testing.T) {
	model := searchModel(t, "go build ./...", "go test ./...", "go vet ./...")
	updated, _ := model.Update(tea.KeyPressMsg{Code: 'r', Mod: tea.ModCtrl})
	model = updated.(Model)
	for _, r := range "go " {
		updated, _ = model.Update(tea.KeyPressMsg{Code: r, Text: string(r)})
		model = updated.(Model)
	}
	updated, _ = model.Update(tea.KeyPressMsg{Code: tea.KeyUp})
	model = updated.(Model)
	if got := model.input.Value(); got != "go test ./..." {
		t.Fatalf("up moved to %q, want the older match", got)
	}
	updated, _ = model.Update(tea.KeyPressMsg{Code: tea.KeyDown})
	model = updated.(Model)
	if got := model.input.Value(); got != "go vet ./..." {
		t.Fatalf("down moved to %q, want the newer match", got)
	}
}

func TestReverseSearchSkipsRepeatedPrompts(t *testing.T) {
	model := searchModel(t, "go build ./...", "go test ./...", "go test ./...")
	updated, _ := model.Update(tea.KeyPressMsg{Code: 'r', Mod: tea.ModCtrl})
	model = updated.(Model)
	for _, r := range "go " {
		updated, _ = model.Update(tea.KeyPressMsg{Code: r, Text: string(r)})
		model = updated.(Model)
	}
	updated, _ = model.Update(tea.KeyPressMsg{Code: 'r', Mod: tea.ModCtrl})
	model = updated.(Model)
	if got := model.input.Value(); got != "go build ./..." {
		t.Fatalf("ctrl+r stayed on a repeated prompt: %q", got)
	}
}
