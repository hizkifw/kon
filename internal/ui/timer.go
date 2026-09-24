package ui

import (
	"fmt"
	"strings"
	"time"

	tea "charm.land/bubbletea/v2"
)

// timerTickMsg advances the work timer's display. The timer shows whole
// seconds, so one tick per second keeps it current. epoch identifies the turn
// the tick belongs to, so a tick left in flight when a run ended cannot start a
// second chain once the next turn begins.
type timerTickMsg struct{ epoch int }

// timerTick schedules the next timer repaint for the given turn.
func timerTick(epoch int) tea.Cmd {
	return tea.Tick(time.Second, func(time.Time) tea.Msg { return timerTickMsg{epoch: epoch} })
}

// The turn marker is a dot that reads as a status light: outline and filled
// alternate once a second while a turn is running, so the transcript shows
// motion even when the seconds digit has not changed, and a finished turn stays
// filled. Both glyphs are one cell wide, so the marker never shifts the text.
const (
	markOutline = "○"
	markFilled  = "●"
)

// workingLabel is the running form of the turn marker. The dot flips on the
// elapsed second's parity, so it is a pure function of the clock and cannot
// drift out of step with the displayed total.
func workingLabel(elapsed time.Duration) string {
	mark := markFilled
	if int(elapsed/time.Second)%2 == 1 {
		mark = markOutline
	}
	return mark + " Working… " + formatDuration(elapsed)
}

// workedLabel is the finished form of the turn marker: the dot stays filled and
// the total is frozen. Live and replayed turns both build from here so they
// render identically.
func workedLabel(elapsed time.Duration) string {
	return markFilled + " Worked for " + formatDuration(elapsed)
}

// stoppedLabel replaces the total for a replayed turn whose process died before
// it finished, so no duration was recorded. The outline dot sets it apart from a
// completed turn's filled one.
const stoppedLabel = markOutline + " Stopped abruptly"

// turnTimer times one user turn. It starts when the prompt is submitted and
// stops when the run ends, so it spans every internal model call and tool step
// of that turn rather than one provider round trip.
type turnTimer struct {
	start time.Time
	end   time.Time // zero while the turn is running
	// epoch identifies this turn's tick chain; see timerTickMsg.
	epoch int
}

func (t *turnTimer) running() bool { return t.end.IsZero() }

// elapsed is the turn's duration at now, or its frozen total once it stopped.
func (t *turnTimer) elapsed(now time.Time) time.Duration {
	if t.running() {
		return now.Sub(t.start)
	}
	return t.end.Sub(t.start)
}

// startTimer begins timing a new turn and shows its first frame immediately, so
// the indicator appears with the prompt instead of a second later.
func (m *Model) startTimer() {
	now := time.Now()
	m.timerEpoch++
	m.timer = &turnTimer{start: now, epoch: m.timerEpoch}
	m.syncTimer(now)
}

// syncTimer repaints the running indicator from the current clock.
func (m *Model) syncTimer(now time.Time) {
	if m.timer == nil {
		m.transcript.liveTimer = ""
		return
	}
	m.transcript.liveTimer = workingLabel(m.timer.elapsed(now))
}

// finishTimer freezes the running indicator into a stable transcript block so
// it stays in place as history, while the next turn gets a fresh indicator.
func (m *Model) finishTimer() {
	if m.timer == nil {
		return
	}
	m.timer.end = time.Now()
	m.transcript.liveTimer = ""
	m.transcript.add(block{kind: blockElapsed, text: workedLabel(m.timer.elapsed(m.timer.end))})
	m.timer = nil
}

// formatDuration renders a duration at second precision, dropping zero-valued
// components so it reads naturally: "30s", "2m 30s", "1h 20m 50s".
func formatDuration(d time.Duration) string {
	seconds := int(d / time.Second)
	if seconds < 0 {
		seconds = 0
	}
	h, m, s := seconds/3600, seconds%3600/60, seconds%60
	parts := make([]string, 0, 3)
	if h > 0 {
		parts = append(parts, fmt.Sprintf("%dh", h))
	}
	if m > 0 {
		parts = append(parts, fmt.Sprintf("%dm", m))
	}
	if s > 0 || len(parts) == 0 {
		parts = append(parts, fmt.Sprintf("%ds", s))
	}
	return strings.Join(parts, " ")
}
