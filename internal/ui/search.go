package ui

import (
	"strings"

	tea "charm.land/bubbletea/v2"
	"github.com/hizkifw/kon/internal/history"
)

// reverseSearch is the incremental reverse-history search started by Ctrl+R,
// mirroring the shell's (reverse-i-search). Like bash it draws no list: the
// query lives in the status line and the highlighted match fills the prompt, so
// accepting it is a no-op. Repeated Ctrl+R cycles through matches in place
// instead of re-filtering, and cancelling restores the prompt that was being
// typed.
type reverseSearch struct {
	query   string
	draft   string // prompt contents before the search began, restored on cancel
	prior   string // status before the search began, restored when it ends
	matches []string
	index   int
}

// refresh recomputes the matches for the current query. An empty query matches
// nothing so the search shows no result until the user types, like bash. Entries
// are scanned newest first so the most recent match is the default, and a
// prompt submitted repeatedly is listed once so cycling always moves to new
// text instead of appearing stuck.
func (s *reverseSearch) refresh(entries []history.Entry) {
	s.matches = s.matches[:0]
	if s.query == "" {
		return
	}
	needle := strings.ToLower(s.query)
	seen := make(map[string]bool)
	for i := len(entries) - 1; i >= 0; i-- {
		text := entries[i].Text
		if !seen[text] && strings.Contains(strings.ToLower(text), needle) {
			seen[text] = true
			s.matches = append(s.matches, text)
		}
	}
	if s.index >= len(s.matches) {
		s.index = max(0, len(s.matches)-1)
	}
}

// startSearch opens the reverse search over the prompt history.
func (m Model) startSearch() (tea.Model, tea.Cmd) {
	// A popup may be open (e.g. slash-command completion with a preview); the
	// search owns the prompt and status now, so close it and restore the live
	// transcript.
	m.resetMenu()
	m.search = &reverseSearch{draft: m.input.Value(), prior: m.status}
	m.search.refresh(m.history.entries)
	m.syncSearch()
	return m, nil
}

// updateSearch handles keys while the search is active. Plain text extends the
// query, Backspace shortens it, Ctrl+R or Up steps to an older match and Down
// back to a newer one (matching prompt recall, where Up is older), Enter
// accepts, and Esc or Ctrl+C restores the original prompt.
func (m Model) updateSearch(msg tea.KeyPressMsg) (tea.Model, tea.Cmd) {
	s := m.search
	switch msg.String() {
	case "ctrl+r", "up":
		if s.index+1 < len(s.matches) {
			s.index++
		}
	case "down":
		if s.index > 0 {
			s.index--
		}
	case "esc", "ctrl+c":
		return m.cancelSearch(), nil
	case "enter":
		return m.acceptSearch(), nil
	case "backspace":
		if s.query != "" {
			runes := []rune(s.query)
			s.query = string(runes[:len(runes)-1])
			s.index = 0
			s.refresh(m.history.entries)
		}
	default:
		if msg.Text != "" {
			s.query += msg.Text
			s.index = 0
			s.refresh(m.history.entries)
		}
	}
	m.syncSearch()
	return m, nil
}

// syncSearch mirrors the search state into the prompt and the status line: a
// highlighted match fills the prompt so accepting it is a no-op, and the status
// shows the query as bash does. An empty query shows just the (reverse-i-search)
// status with no result; a query with no match reports (failed reverse-i-search)
// and leaves the prompt untouched, both matching the shell.
func (m *Model) syncSearch() {
	s := m.search
	if s == nil {
		return
	}
	switch {
	case s.query == "":
		m.status = "(reverse-i-search)`'"
	case len(s.matches) == 0:
		m.status = "(failed reverse-i-search)`" + s.query + "'"
	default:
		m.input.SetValue(s.matches[s.index])
		m.input.CursorEnd()
		m.status = "(reverse-i-search)`" + s.query + "'"
	}
	m.resize()
}

// acceptSearch commits the highlighted match to the prompt and closes the
// search. The prompt already shows that match, so only the search state is torn
// down.
func (m Model) acceptSearch() Model {
	prior := m.search.prior
	m.search = nil
	m.status = prior
	m.resize()
	m.input.CursorEnd()
	return m
}

// cancelSearch restores the prompt contents captured when the search began.
func (m Model) cancelSearch() Model {
	draft, prior := m.search.draft, m.search.prior
	m.search = nil
	m.input.SetValue(draft)
	m.status = prior
	m.resize()
	m.input.CursorEnd()
	return m
}
