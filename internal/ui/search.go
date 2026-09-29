package ui

import (
	"strings"

	tea "charm.land/bubbletea/v2"
)

// reverseSearch is the incremental history search started by Ctrl+R, which
// behaves like the shell's (reverse-i-search). It draws no list: the query
// lives in the status line and the match fills the prompt, with the cursor
// where the query was found. Ctrl+R steps to an older match and Ctrl+S to a
// newer one. Esc ends the search keeping the match, and Ctrl+G ends it giving
// back the prompt that was being typed. Any other key ends it too and then
// does what it does in the prompt, so Enter sends the match, Left and Right
// move through it, and Up and Down go on through history from it.
type reverseSearch struct {
	query string
	draft string // prompt contents before the search began, given back by Ctrl+G
	// match is the history entry the query was last found in, or -1, and at
	// is the byte where it was found. failed marks a query found nowhere
	// further, which leaves the last match in the prompt as the shell does.
	match, at int
	failed    bool
	// forward is set by Ctrl+S, which searches toward newer entries.
	forward bool
	// typed holds the search as it was before each character of the query,
	// so Backspace goes back to where the shorter query matched.
	typed []searchState
	// newest maps each prompt to its newest entry. Only that entry stands for
	// the prompt, so a prompt sent many times is found once and stepping
	// always moves to different text.
	newest map[string]int
}

// searchState is what Backspace goes back to.
type searchState struct {
	query     string
	match, at int
	failed    bool
}

// startSearch opens the search over the prompt history.
func (m Model) startSearch() (tea.Model, tea.Cmd) {
	// A popup may be open (e.g. slash-command completion with a preview); the
	// search owns the prompt and status now, so close it and restore the live
	// transcript.
	m.resetMenu()
	newest := make(map[string]int, len(m.history.entries))
	for i, entry := range m.history.entries {
		newest[entry.Text] = i
	}
	m.search = &reverseSearch{draft: m.input.Value(), match: -1, newest: newest}
	m.resize()
	return m, nil
}

// searchKey handles a key while the search is open, and reports whether the
// search took it. A key it does not take has ended the search, keeping the
// match, and is left to act on the prompt.
func (m Model) searchKey(msg tea.KeyPressMsg) (Model, bool) {
	s := m.search
	switch key := msg.String(); {
	case key == "ctrl+r", key == "ctrl+s":
		s.forward = key == "ctrl+s"
		if s.query == "" {
			// An empty query searches again for the last one.
			s.query = m.lastSearch
			m.find(true)
		} else {
			m.find(false)
		}
	case key == "backspace":
		if n := len(s.typed); n > 0 {
			last := s.typed[n-1]
			s.typed = s.typed[:n-1]
			s.query, s.match, s.at, s.failed = last.query, last.match, last.at, last.failed
			m.showMatch()
		}
	case key == "ctrl+g":
		m.input.SetValue(s.draft)
		m.input.CursorEnd()
		m = m.endSearch()
	case key == "esc":
		m = m.endSearch()
	case msg.Text != "" && msg.Mod&(tea.ModCtrl|tea.ModAlt) == 0:
		m.extendQuery(msg.Text)
	default:
		m = m.endSearch()
		return m, false
	}
	return m, true
}

// pasteSearch adds pasted text to the query, on one line.
func (m Model) pasteSearch(msg tea.PasteMsg) Model {
	m.extendQuery(strings.Join(strings.Fields(msg.Content), " "))
	return m
}

// extendQuery adds text to the query and searches for it from the current
// match, which is kept while it still holds the longer query.
func (m *Model) extendQuery(text string) {
	s := m.search
	if text == "" {
		return
	}
	s.typed = append(s.typed, searchState{s.query, s.match, s.at, s.failed})
	s.query += text
	m.find(true)
}

// find searches history for the query, stepping from the current match
// toward older entries, or newer ones for Ctrl+S. With here set the current
// match itself is tried first. A query found nowhere further is marked
// failed and leaves the last match where it is.
func (m *Model) find(here bool) {
	s := m.search
	entries := m.history.entries
	if s.query == "" {
		m.showMatch()
		return
	}
	step, i := -1, s.match
	if s.forward {
		step = 1
	}
	switch {
	case i < 0 && s.forward:
		i = len(entries)
	case i < 0:
		i = len(entries) - 1
	case !here:
		i += step
	}
	s.failed = true
	for ; i >= 0 && i < len(entries); i += step {
		if s.newest[entries[i].Text] != i {
			continue
		}
		if at := indexFold(entries[i].Text, s.query); at >= 0 {
			s.match, s.at, s.failed = i, at, false
			break
		}
	}
	m.showMatch()
}

// showMatch fills the prompt with the match and puts the cursor where the
// query was found in it, or shows the draft until there is a match.
func (m *Model) showMatch() {
	s := m.search
	if s.match < 0 {
		m.input.SetValue(s.draft)
		m.input.CursorEnd()
		m.resize()
		return
	}
	text := m.history.entries[s.match].Text
	m.input.SetValue(text)
	m.setCursorOffset(s.at)
	m.resize()
}

// endSearch closes the search with the match in the prompt. History recall
// goes on from the match, as it does in the shell, so Up and Down move to
// the prompts sent before and after it, and back to the draft past the end.
func (m Model) endSearch() Model {
	s := m.search
	m.search = nil
	m.lastSearch = s.query
	if s.match >= 0 {
		m.history.position, m.history.draft = s.match, s.draft
	}
	m.resize()
	return m
}

// prompt shows the query in the status line as the shell does, naming the
// direction and whether the query was found.
func (s *reverseSearch) prompt() string {
	name := "reverse-i-search"
	if s.forward {
		name = "i-search"
	}
	if s.failed {
		name = "failed " + name
	}
	return "(" + name + ")`" + s.query + "'"
}

// indexFold returns where substr first appears in s, ignoring case, or -1.
func indexFold(s, substr string) int {
	for i := range s {
		if i+len(substr) <= len(s) && strings.EqualFold(s[i:i+len(substr)], substr) {
			return i
		}
	}
	return -1
}
