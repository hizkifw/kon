package ui

import (
	"testing"

	tea "charm.land/bubbletea/v2"
	"kon.kitsu.red/internal/history"
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

func key(m Model, k tea.KeyPressMsg) Model {
	updated, _ := m.Update(k)
	return updated.(Model)
}

var (
	ctrlR = tea.KeyPressMsg{Code: 'r', Mod: tea.ModCtrl}
	ctrlS = tea.KeyPressMsg{Code: 's', Mod: tea.ModCtrl}
	ctrlG = tea.KeyPressMsg{Code: 'g', Mod: tea.ModCtrl}
	ctrlC = tea.KeyPressMsg{Code: 'c', Mod: tea.ModCtrl}
)

// search opens the search and types query.
func search(m Model, query string) Model {
	m = key(m, ctrlR)
	for _, r := range query {
		m = key(m, tea.KeyPressMsg{Code: r, Text: string(r)})
	}
	return m
}

func TestCtrlROpensReverseSearchShowingNoResult(t *testing.T) {
	m := searchModel(t, "go build ./...", "make check", "go test ./...")
	m.input.SetValue("draft")
	updated, cmd := m.Update(ctrlR)
	m = updated.(Model)
	if cmd != nil || m.search == nil {
		t.Fatalf("ctrl+r: cmd = %v, search open = %v", cmd, m.search != nil)
	}
	// An empty query shows no result, like bash; the draft stays in the prompt
	// until a match replaces it.
	if got := m.input.Value(); got != "draft" {
		t.Fatalf("empty search changed the prompt: %q", got)
	}
	if got := m.statusText(); got != "(reverse-i-search)`'" {
		t.Fatalf("status = %q", got)
	}
	// The search is prompt-and-status only: no popup list, like bash.
	if m.menu.open() {
		t.Fatalf("reverse search opened a popup with %d rows", len(m.menu.items))
	}
}

func TestReverseSearchNarrowsToQuery(t *testing.T) {
	m := search(searchModel(t, "go build ./...", "make check", "go test ./..."), "make")
	if got := m.input.Value(); got != "make check" {
		t.Fatalf("match after typing query = %q", got)
	}
	if got := m.statusText(); got != "(reverse-i-search)`make'" {
		t.Fatalf("status = %q", got)
	}
}

// The cursor sits where the query was found, as in the shell, on whichever
// line of the prompt that is.
func TestReverseSearchPutsTheCursorOnTheMatch(t *testing.T) {
	m := search(searchModel(t, "first line\nthen make it"), "make")
	if m.input.Line() != 1 || m.input.Column() != len("then ") {
		t.Fatalf("cursor on line %d column %d", m.input.Line(), m.input.Column())
	}
}

func TestReverseSearchCyclesMatches(t *testing.T) {
	m := search(searchModel(t, "go build ./...", "go test ./...", "go vet ./..."), "go ")
	if got := m.input.Value(); got != "go vet ./..." {
		t.Fatalf("first match = %q", got)
	}
	if m = key(m, ctrlR); m.input.Value() != "go test ./..." {
		t.Fatalf("ctrl+r moved to %q, want the next older match", m.input.Value())
	}
}

func TestCtrlSSearchesTowardNewer(t *testing.T) {
	m := search(searchModel(t, "make a", "make b", "make c"), "make")
	m = key(key(m, ctrlR), ctrlR)
	if m = key(m, ctrlS); m.input.Value() != "make b" || m.statusText() != "(i-search)`make'" {
		t.Fatalf("ctrl+s moved to %q, status %q", m.input.Value(), m.statusText())
	}
}

func TestReverseSearchSkipsRepeatedPrompts(t *testing.T) {
	m := search(searchModel(t, "go build ./...", "go test ./...", "go test ./..."), "go ")
	if m = key(m, ctrlR); m.input.Value() != "go build ./..." {
		t.Fatalf("ctrl+r stayed on a repeated prompt: %q", m.input.Value())
	}
}

// Backspace goes back to where the shorter query was, which after Ctrl+R is
// not where a fresh search for it would land.
func TestBackspaceGoesBackToTheShorterMatch(t *testing.T) {
	m := search(searchModel(t, "go vet", "go test", "gold"), "go")
	if m = key(m, ctrlR); m.input.Value() != "go test" {
		t.Fatalf("ctrl+r moved to %q", m.input.Value())
	}
	m = key(m, tea.KeyPressMsg{Code: 'x', Text: "x"})
	m = key(m, tea.KeyPressMsg{Code: tea.KeyBackspace})
	if m.input.Value() != "go test" || m.search.query != "go" || m.search.failed {
		t.Fatalf("backspace left query %q on %q, failed %v", m.search.query, m.input.Value(), m.search.failed)
	}
}

// A query found nowhere further leaves the last match in the prompt, and no
// match at all leaves the draft being typed.
func TestFailedSearchKeepsWhatThePromptShows(t *testing.T) {
	m := searchModel(t, "make check")
	m.input.SetValue("draft")
	if m = search(m, "z"); m.statusText() != "(failed reverse-i-search)`z'" || m.input.Value() != "draft" {
		t.Fatalf("status %q, prompt %q", m.statusText(), m.input.Value())
	}
	m = search(searchModel(t, "make check"), "makez")
	if m.statusText() != "(failed reverse-i-search)`makez'" || m.input.Value() != "make check" {
		t.Fatalf("status %q, prompt %q", m.statusText(), m.input.Value())
	}
}

func TestEscEndsTheSearchKeepingTheMatch(t *testing.T) {
	m := searchModel(t, "make check")
	m.input.SetValue("half typed")
	m = key(search(m, "make"), tea.KeyPressMsg{Code: tea.KeyEscape})
	if m.search != nil || m.input.Value() != "make check" || m.busy() {
		t.Fatalf("after esc: search open %v, prompt %q, sent %v", m.search != nil, m.input.Value(), m.busy())
	}
}

func TestCtrlGGivesBackTheDraft(t *testing.T) {
	m := searchModel(t, "make check")
	m.input.SetValue("half typed")
	if m = key(search(m, "make"), ctrlG); m.search != nil || m.input.Value() != "half typed" {
		t.Fatalf("after ctrl+g: search open %v, prompt %q", m.search != nil, m.input.Value())
	}
}

func TestEnterSendsTheMatch(t *testing.T) {
	m := key(search(searchModel(t, "go build ./...", "make check"), "make"), tea.KeyPressMsg{Code: tea.KeyEnter})
	if m.search != nil || !m.busy() || m.input.Value() != "" {
		t.Fatalf("after enter: search open %v, sent %v, prompt %q", m.search != nil, m.busy(), m.input.Value())
	}
	if last := m.transcript.blocks[len(m.transcript.blocks)-1]; last.kind != blockUser || last.text != "make check" {
		t.Fatalf("sent %+v", last)
	}
}

func TestCtrlCEndsTheSearchAndClearsThePrompt(t *testing.T) {
	if m := key(search(searchModel(t, "make check"), "make"), ctrlC); m.search != nil || m.input.Value() != "" {
		t.Fatalf("after ctrl+c: search open %v, prompt %q", m.search != nil, m.input.Value())
	}
}

func TestLeftAndRightEndTheSearchAndMoveFromTheMatch(t *testing.T) {
	start := search(searchModel(t, "run make check now"), "make")
	for _, c := range []struct {
		code rune
		col  int
	}{{tea.KeyLeft, len("run ") - 1}, {tea.KeyRight, len("run ") + 1}} {
		m := key(start, tea.KeyPressMsg{Code: c.code})
		if m.search != nil || m.input.Value() != "run make check now" || m.input.Column() != c.col {
			t.Fatalf("after %v: search open %v, prompt %q, column %d, want %d", c.code, m.search != nil, m.input.Value(), m.input.Column(), c.col)
		}
	}
}

// Up and Down end the search and go on through history from the match: the
// prompt sent before it, the ones after it, and the draft past the newest.
func TestUpAndDownGoOnThroughHistoryFromTheMatch(t *testing.T) {
	m := searchModel(t, "go build", "make check", "go test", "make lint")
	m.input.SetValue("draft")
	m = key(search(m, "make"), ctrlR)
	if got := m.input.Value(); got != "make check" {
		t.Fatalf("match = %q", got)
	}
	up, down := tea.KeyPressMsg{Code: tea.KeyUp}, tea.KeyPressMsg{Code: tea.KeyDown}
	if m = key(m, up); m.search != nil || m.input.Value() != "go build" {
		t.Fatalf("after up: search open %v, prompt %q", m.search != nil, m.input.Value())
	}
	for _, want := range []string{"make check", "go test", "make lint", "draft"} {
		if m = key(m, down); m.input.Value() != want {
			t.Fatalf("down reached %q, want %q", m.input.Value(), want)
		}
	}
}

func TestCtrlROnAnEmptyQueryRepeatsTheLastSearch(t *testing.T) {
	m := key(search(searchModel(t, "make check", "go test"), "make"), tea.KeyPressMsg{Code: tea.KeyEscape})
	m.input.Reset()
	if m = key(key(m, ctrlR), ctrlR); m.search.query != "make" || m.input.Value() != "make check" {
		t.Fatalf("query %q, prompt %q", m.search.query, m.input.Value())
	}
}

func TestPasteAddsToTheQuery(t *testing.T) {
	m := key(searchModel(t, "make check", "go test"), ctrlR)
	updated, _ := m.Update(tea.PasteMsg{Content: "make\nch"})
	if m = updated.(Model); m.search.query != "make ch" || m.input.Value() != "make check" {
		t.Fatalf("query %q, prompt %q", m.search.query, m.input.Value())
	}
}
