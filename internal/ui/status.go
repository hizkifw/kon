package ui

import (
	"strings"
	"time"

	tea "charm.land/bubbletea/v2"
)

// The status line tells two things apart: the mode kon is in, which it shows
// for as long as the mode lasts, and the last thing that happened, the
// message, which stays until something replaces it or, for a passing notice,
// until its time is up. Keeping them apart means no mode has to save the
// message it covers, and no message can hide a mode.

// mode names the mode kon is in: a login in progress, a session followed
// read-only, or a launch with nothing configured, which explains itself in
// the transcript. It is "" otherwise.
func (m Model) mode() string {
	switch {
	case m.login != nil:
		return m.login.question()
	case m.follow != nil:
		return m.followMode
	case !m.configured:
		return "needs configuration"
	}
	return ""
}

// statusText is what the status line says after its fixed fields: the mode,
// then the message. A reverse search takes the place of both, as the shell's
// prompt does, and they come back when it ends.
func (m Model) statusText() string {
	if m.search != nil {
		return m.search.prompt()
	}
	var parts []string
	for _, part := range []string{m.mode(), m.message} {
		// The status line is one row: a multi-line message, such as a
		// provider's error body, would otherwise push the prompt down.
		if part = oneLine(part); part != "" {
			parts = append(parts, part)
		}
	}
	return strings.Join(parts, " · ")
}

// flashTimeout is how long a passing notice, such as a copy's, stays in the
// status line.
const flashTimeout = 3 * time.Second

type flashDoneMsg struct{ epoch int }

// flash shows a passing notice as the message and clears it after
// flashTimeout.
func (m *Model) flash(text string) tea.Cmd {
	m.message, m.flashed = text, text
	m.flashEpoch++
	epoch := m.flashEpoch
	return tea.Tick(flashTimeout, func(time.Time) tea.Msg { return flashDoneMsg{epoch} })
}

// flashDone takes a notice down when its time is up. A later notice has its
// own time, and a message set since then is left alone.
func (m Model) flashDone(msg flashDoneMsg) (tea.Model, tea.Cmd) {
	if msg.epoch == m.flashEpoch && m.message == m.flashed {
		m.message = ""
	}
	return m, nil
}
