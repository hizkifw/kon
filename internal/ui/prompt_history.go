package ui

import "github.com/hizkifw/kon/internal/history"

type promptHistory struct {
	store    *history.Store
	entries  []history.Entry
	position int
	draft    string
}

func newPromptHistory(store *history.Store, entries []history.Entry) promptHistory {
	return promptHistory{store: store, entries: entries, position: len(entries)}
}

func (h *promptHistory) append(cwd, text string) error {
	if err := h.store.Append(cwd, text); err != nil {
		return err
	}
	h.entries = append(h.entries, history.Entry{CWD: cwd, Text: text})
	h.position = len(h.entries)
	h.draft = ""
	return nil
}

func (h *promptHistory) recall(current string, direction int) (string, bool) {
	if len(h.entries) == 0 {
		return "", false
	}
	if direction < 0 {
		if h.position == len(h.entries) {
			h.draft = current
		}
		if h.position == 0 {
			return "", false
		}
		h.position--
		return h.entries[h.position].Text, true
	}
	if h.position >= len(h.entries) {
		return "", false
	}
	h.position++
	if h.position == len(h.entries) {
		return h.draft, true
	}
	return h.entries[h.position].Text, true
}

func (h *promptHistory) resetPosition() {
	h.position = len(h.entries)
	h.draft = ""
}
