package ui

import (
	"time"

	tea "charm.land/bubbletea/v2"
)

// flashTimeout is how long a passing notice, such as a copy's, stays in the
// status line.
const flashTimeout = 3 * time.Second

// statusFlash is a passing notice in the status line and the status it
// covers, which comes back when the notice goes.
type statusFlash struct {
	text, prior string
	epoch       int
}

type flashDoneMsg struct{ epoch int }

// flash shows a passing notice in the status line and clears it after
// flashTimeout. A notice that follows another covers the same status the first
// one did.
func (m *Model) flash(text string) tea.Cmd {
	prior := m.status
	if m.flashing != nil && m.status == m.flashing.text {
		prior = m.flashing.prior
	}
	m.flashEpoch++
	m.flashing = &statusFlash{text: text, prior: prior, epoch: m.flashEpoch}
	m.status = text
	epoch := m.flashEpoch
	return tea.Tick(flashTimeout, func(time.Time) tea.Msg { return flashDoneMsg{epoch} })
}

// flashDone takes a notice down when its time is up. A later notice has its
// own time, and a status set since then is left alone.
func (m Model) flashDone(msg flashDoneMsg) (tea.Model, tea.Cmd) {
	if f := m.flashing; f != nil && f.epoch == msg.epoch {
		if m.status == f.text {
			m.status = f.prior
		}
		m.flashing = nil
	}
	return m, nil
}
