package ui

import (
	"image/color"
	"strings"
	"time"

	tea "charm.land/bubbletea/v2"
	"github.com/hizkifw/kon/core/tokens"
)

// interrupts tracks Esc presses against the run in flight, so the harness can
// escalate: the first cancels the run (interrupting a running command), the
// second kills it.
type interrupts struct {
	presses int
	// epoch is the notice epoch of the warning a first Esc showed before
	// interrupting (see interruptArmed); 0 when none was shown.
	epoch int
}

// flashState is the last passing notice shown as the message. epoch counts
// notices, so each clears only itself.
type flashState struct {
	text  string
	epoch int
}

// streamCount counts the stream chunks received since the last usage report,
// each taken as one token, so the status bar moves while a response streams.
// A chunk usually holds more than one token, so the estimate undercounts
// until the report replaces it.
type streamCount struct {
	all tokens.Count
	// context counts only the chunks that extend the context: a compaction
	// summary replaces the context rather than adding to it.
	context tokens.Count
}

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
	case m.follow.replay != nil:
		return m.follow.mode
	case !m.configured:
		return "needs configuration"
	}
	return ""
}

// tone is how the message reads at a glance: a plain report, something that
// went right, something to look at before continuing, or something that
// failed or was lost.
type tone int

const (
	toneInfo tone = iota
	toneSuccess
	toneWarn
	toneDanger
)

func (t tone) color() color.Color {
	switch t {
	case toneSuccess:
		return colorOK
	case toneWarn:
		return colorWarn
	case toneDanger:
		return colorFail
	}
	return colorFaint
}

// say sets the message in a tone.
func (m *Model) say(t tone, text string) {
	m.message, m.tone, m.toned = text, t, text
}

// messageTone is the tone of the message on the status line. A tone holds
// only while the message is still the text it was said with, so a plain
// assignment to message never inherits the last message's color.
func (m Model) messageTone() tone {
	if m.message != m.toned {
		return toneInfo
	}
	return m.tone
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
func (m *Model) flash(t tone, text string) tea.Cmd {
	m.say(t, text)
	m.flashed.text = text
	m.flashed.epoch++
	epoch := m.flashed.epoch
	return tea.Tick(flashTimeout, func(time.Time) tea.Msg { return flashDoneMsg{epoch} })
}

// flashDone takes a notice down when its time is up. A later notice has its
// own time, and a message set since then is left alone.
func (m Model) flashDone(msg flashDoneMsg) (tea.Model, tea.Cmd) {
	if msg.epoch == m.flashed.epoch && m.message == m.flashed.text {
		m.message = ""
	}
	return m, nil
}

// interruptArmed reports whether a first Esc's warning is still on the status
// line, so the next Esc interrupts. Once the warning is gone, by its time
// running out or by another message replacing it, Esc warns again.
func (m Model) interruptArmed() bool {
	return m.interrupt.epoch != 0 && m.interrupt.epoch == m.flashed.epoch && m.message == m.flashed.text
}
