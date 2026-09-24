package ui

import (
	"context"
	"errors"
	"strings"
	"testing"
	"time"

	"charm.land/lipgloss/v2"
	"github.com/hizkifw/kon/internal/agent"
	"github.com/hizkifw/kon/internal/session"
)

// TestTimerEndToEndAcrossTwoTurns replays the requested behaviour: a first turn
// shows a running indicator, freezes to a "Worked for …" total when it ends, and
// the next turn shows a fresh indicator below that total rather than replacing
// it.
func TestTimerEndToEndAcrossTwoTurns(t *testing.T) {
	m := newTestModel(t)
	m.input.SetValue("do x y z")
	updated, _ := m.submit()
	m = updated.(Model)
	if m.timer == nil {
		t.Fatal("submit did not start a timer")
	}
	m.transcript.add(block{kind: blockAssistant, text: "ok here's xyz done"})
	m.timer.start = time.Now().Add(-5*time.Minute - 20*time.Second)
	updated, _ = m.Update(runDoneMsg{})
	m = updated.(Model)
	first := plain(strings.Join(m.transcript.linesFor(80), "\n"))
	if !strings.Contains(first, "ok here's xyz done") || !strings.Contains(first, "Worked for 5m 20s") {
		t.Fatalf("first turn transcript:\n%s", first)
	}
	if strings.Contains(first, "Working…") {
		t.Fatalf("running indicator survived the finished first turn:\n%s", first)
	}

	m.input.SetValue("next do a b c")
	updated, _ = m.submit()
	m = updated.(Model)
	m.transcript.add(block{kind: blockAssistant, text: "ok let me do a b c"})
	m.syncTimer(m.timer.start.Add(20 * time.Second))
	second := plain(strings.Join(m.transcript.linesFor(80), "\n"))
	for _, want := range []string{"do x y z", "● Worked for 5m 20s", "next do a b c", "ok let me do a b c", "Working… 20s"} {
		if !strings.Contains(second, want) {
			t.Fatalf("second turn transcript missing %q:\n%s", want, second)
		}
	}
	// The frozen total must stay above the new turn's content, and the running
	// indicator must trail everything.
	if strings.Index(second, "● Worked for 5m 20s") > strings.Index(second, "next do a b c") {
		t.Fatalf("frozen total moved below the new turn:\n%s", second)
	}
	if !strings.HasSuffix(second, "Working… 20s") {
		t.Fatalf("running indicator is not the transcript tail:\n%s", second)
	}
}

// replayedMarkers returns the text of every turn marker a replay produced, in
// order.
func replayedMarkers(model Model) []string {
	var out []string
	for _, b := range model.transcript.blocks {
		if b.kind == blockElapsed {
			out = append(out, b.text)
		}
	}
	return out
}

func replayMessage(role session.Role, text string) session.Entry {
	return session.Entry{Type: session.EntryTypeMessage, Message: &session.Message{Role: role, Parts: []session.Part{{Type: session.PartText, Text: text}}}}
}

func turnStart() session.Entry { return session.Entry{Type: session.EntryTypeTurnStart} }

func turnEnd(d time.Duration) session.Entry {
	return session.Entry{Type: session.EntryTypeTurnEnd, DurationMS: d.Milliseconds()}
}

// TestReplayShowsRecordedTurnDurations checks that a resumed session shows each
// turn's recorded "Worked for …" total after its own reply and before the next
// prompt, matching the live transcript's ordering.
func TestReplayShowsRecordedTurnDurations(t *testing.T) {
	model := newTestModel(t)
	model.applyHistory([]session.Entry{
		turnStart(),
		replayMessage(session.RoleUser, "do x y z"),
		replayMessage(session.RoleAssistant, "ok here's xyz done"),
		turnEnd(5*time.Minute + 20*time.Second),
		turnStart(),
		replayMessage(session.RoleUser, "next do a b c"),
		replayMessage(session.RoleAssistant, "done again"),
		turnEnd(30 * time.Second),
	})
	if got := replayedMarkers(model); len(got) != 2 || got[0] != "● Worked for 5m 20s" || got[1] != "● Worked for 30s" {
		t.Fatalf("replayed durations = %v", got)
	}
	got := plain(strings.Join(model.transcript.linesFor(80), "\n"))
	first := strings.Index(got, "Worked for 5m 20s")
	if first < strings.Index(got, "ok here's xyz done") || first > strings.Index(got, "next do a b c") {
		t.Fatalf("first total is misplaced:\n%s", got)
	}
	if last := strings.Index(got, "Worked for 30s"); last < strings.Index(got, "done again") {
		t.Fatalf("last total is misplaced:\n%s", got)
	}
}

// TestReplayMarksUnfinishedTurns checks that a turn whose process died before
// it recorded an end is marked as stopped, both when a later turn follows it
// and when it is the last thing in the history.
func TestReplayMarksUnfinishedTurns(t *testing.T) {
	model := newTestModel(t)
	model.applyHistory([]session.Entry{
		turnStart(),
		replayMessage(session.RoleUser, "one"),
		replayMessage(session.RoleAssistant, "partial"),
		turnStart(),
		replayMessage(session.RoleUser, "two"),
		replayMessage(session.RoleAssistant, "done"),
		turnEnd(4 * time.Second),
		turnStart(),
		replayMessage(session.RoleUser, "three"),
	})
	want := []string{stoppedLabel, "● Worked for 4s", stoppedLabel}
	if got := replayedMarkers(model); strings.Join(got, "|") != strings.Join(want, "|") {
		t.Fatalf("markers = %v, want %v", got, want)
	}
	got := plain(strings.Join(model.transcript.linesFor(80), "\n"))
	if stopped := strings.Index(got, "Stopped abruptly"); stopped < strings.Index(got, "partial") || stopped > strings.Index(got, "two") {
		t.Fatalf("stopped marker is misplaced:\n%s", got)
	}
}

// TestReplayWithoutTurnMarkersShowsNothing guards sessions written before turns
// were recorded: with no markers there is no duration to show, and no turn may
// be mistaken for an unfinished one.
func TestReplayWithoutTurnMarkersShowsNothing(t *testing.T) {
	model := newTestModel(t)
	model.applyHistory([]session.Entry{
		replayMessage(session.RoleUser, "one"),
		replayMessage(session.RoleAssistant, "two"),
		replayMessage(session.RoleUser, "three"),
	})
	if got := replayedMarkers(model); len(got) != 0 {
		t.Fatalf("unexpected markers %v", got)
	}
}

// TestReplayShowsEndWithoutStart covers a preview window that begins at a
// turn's user message and so cuts off its start: the end still carries the
// whole duration.
func TestReplayShowsEndWithoutStart(t *testing.T) {
	model := newTestModel(t)
	model.applyHistory([]session.Entry{
		replayMessage(session.RoleUser, "one"),
		replayMessage(session.RoleAssistant, "two"),
		turnEnd(12 * time.Second),
	})
	if got := replayedMarkers(model); len(got) != 1 || got[0] != "● Worked for 12s" {
		t.Fatalf("markers = %v", got)
	}
}

// TestTurnMarkerFlipsThenFreezes checks the status dot: it alternates outline
// and filled once a second while running, and the finished form is always
// filled.
func TestTurnMarkerFlipsThenFreezes(t *testing.T) {
	for _, c := range []struct {
		elapsed time.Duration
		want    string
	}{
		{0, "● Working… 0s"},
		{1 * time.Second, "○ Working… 1s"},
		{2 * time.Second, "● Working… 2s"},
		{3 * time.Second, "○ Working… 3s"},
		{10 * time.Second, "● Working… 10s"},
		{11 * time.Second, "○ Working… 11s"},
	} {
		if got := workingLabel(c.elapsed); got != c.want {
			t.Errorf("workingLabel(%v) = %q, want %q", c.elapsed, got, c.want)
		}
	}
	for _, d := range []time.Duration{0, time.Second, 2 * time.Second, 3 * time.Second} {
		got := workedLabel(d)
		if !strings.HasPrefix(got, "● ") {
			t.Errorf("workedLabel(%v) = %q, want a filled mark", d, got)
		}
		if strings.Contains(got, "○") {
			t.Errorf("workedLabel(%v) = %q, leaked the running mark", d, got)
		}
	}
}

// TestTurnMarkerIsInsetOneCell checks the marker's left margin, matching the
// message slabs above it. The running indicator and the frozen total must both
// carry it, and a wrapped continuation must keep it.
func TestTurnMarkerIsInsetOneCell(t *testing.T) {
	var tr transcript
	tr.cwd = "/tmp"
	tr.add(block{kind: blockUser, text: "hi"})
	tr.liveTimer = workingLabel(5 * time.Second)
	lines := tr.linesFor(80)
	last := lines[len(lines)-1]
	if !strings.HasPrefix(plain(last), " ○ Working…") {
		t.Fatalf("running marker is not inset: %q", plain(last))
	}
	tr.liveTimer = ""
	tr.add(block{kind: blockElapsed, text: workedLabel(5 * time.Second)})
	lines = tr.linesFor(80)
	last = lines[len(lines)-1]
	if !strings.HasPrefix(plain(last), " ● Worked for") {
		t.Fatalf("frozen marker is not inset: %q", plain(last))
	}
	// A narrow width forces the marker to wrap; every line still starts with
	// the inset and none overflows.
	for _, line := range tr.linesFor(6) {
		if w := lipgloss.Width(line); w > 6 {
			t.Fatalf("line overflows at width 6: %q (%d)", plain(line), w)
		}
		if plain(line) != "" && !strings.HasPrefix(plain(line), " ") {
			t.Fatalf("wrapped line lost its inset: %q", plain(line))
		}
	}
}

// TestInterruptedTurnFreezesBelowItsAnswer guards the ordering of the interrupt
// path: when a run is cancelled its partial answer is finalized from the live
// stream, and the frozen total must land below that answer rather than above it.
func TestInterruptedTurnFreezesBelowItsAnswer(t *testing.T) {
	m := newTestModel(t)
	m.input.SetValue("hello")
	updated, _ := m.submit()
	m = updated.(Model)
	updated, _ = m.Update(runEventMsg{event: agent.Event{Kind: agent.EventText, Text: "partial answer in progress"}})
	m = updated.(Model)
	updated, _ = m.Update(runDoneMsg{err: context.Canceled})
	m = updated.(Model)
	got := plain(strings.Join(m.transcript.linesFor(80), "\n"))
	answer, worked := strings.Index(got, "partial answer"), strings.Index(got, "Worked for")
	if answer < 0 || worked < 0 {
		t.Fatalf("missing content (answer=%d worked=%d):\n%s", answer, worked, got)
	}
	if worked < answer {
		t.Fatalf("frozen total rendered above the interrupted answer:\n%s", got)
	}
	if m.timer != nil || m.transcript.liveTimer != "" {
		t.Fatalf("interrupted turn left a running timer: %+v live=%q", m.timer, m.transcript.liveTimer)
	}
}

// TestFailedTurnFreezesAfterItsError guards the same ordering on the failure
// path: the error block is appended before the total, so the turn's last line is
// always its elapsed marker.
func TestFailedTurnFreezesAfterItsError(t *testing.T) {
	m := newTestModel(t)
	m.input.SetValue("hello")
	updated, _ := m.submit()
	m = updated.(Model)
	updated, _ = m.Update(runDoneMsg{err: errors.New("boom")})
	m = updated.(Model)
	got := plain(strings.Join(m.transcript.linesFor(80), "\n"))
	errAt, worked := strings.Index(got, "boom"), strings.Index(got, "Worked for")
	if errAt < 0 || worked < 0 {
		t.Fatalf("missing content (err=%d worked=%d):\n%s", errAt, worked, got)
	}
	if worked < errAt {
		t.Fatalf("frozen total rendered above the error:\n%s", got)
	}
	if !strings.HasSuffix(strings.TrimRight(got, "\n"), "0s") {
		t.Fatalf("frozen total is not the turn's last line:\n%s", got)
	}
}

func TestFormatDuration(t *testing.T) {
	cases := []struct {
		in   time.Duration
		want string
	}{
		{0, "0s"},
		{500 * time.Millisecond, "0s"},
		{time.Second, "1s"},
		{30 * time.Second, "30s"},
		{59 * time.Second, "59s"},
		{time.Minute, "1m"},
		{2*time.Minute + 30*time.Second, "2m 30s"},
		{59*time.Minute + 59*time.Second, "59m 59s"},
		{time.Hour, "1h"},
		{time.Hour + 20*time.Minute + 50*time.Second, "1h 20m 50s"},
		{time.Hour + 20*time.Minute, "1h 20m"},
		{2 * time.Hour, "2h"},
		// Sub-second remainders truncate rather than rounding up, so the
		// displayed total never counts a second that has not elapsed.
		{time.Second + 999*time.Millisecond, "1s"},
		{-time.Second, "0s"},
	}
	for _, c := range cases {
		if got := formatDuration(c.in); got != c.want {
			t.Errorf("formatDuration(%v) = %q, want %q", c.in, got, c.want)
		}
	}
}

// TestRunningTimerRendersInTranscript checks that the running indicator renders
// at the tail of the transcript without disturbing the message above it, and
// that it tracks the clock.
func TestRunningTimerRendersInTranscript(t *testing.T) {
	var tr transcript
	tr.cwd = "/tmp"
	tr.add(block{kind: blockUser, text: "do x y z"})
	tr.add(block{kind: blockAssistant, text: "on it"})
	// The dot alternates on the elapsed second's parity: odd seconds show the
	// outline, even seconds the filled mark.
	tr.liveTimer = workingLabel(5 * time.Second)
	got := plain(tr.render(60))
	if want := "on it\n\n ○ Working… 5s"; !strings.HasSuffix(got, want) {
		t.Fatalf("running timer not at transcript tail:\n%s", got)
	}
	tr.liveTimer = workingLabel(6 * time.Second)
	if got := plain(tr.render(60)); !strings.HasSuffix(got, "● Working… 6s") {
		t.Fatalf("timer did not advance or flip:\n%s", got)
	}
	// Clearing the timer (the run finished) must leave the transcript without a
	// stale indicator.
	tr.liveTimer = ""
	if got := plain(tr.render(60)); strings.Contains(got, "Working") {
		t.Fatalf("stale timer survived:\n%s", got)
	}
}

// TestTimerLinesMatchFullRender is the cache-equivalence guard for the timer:
// after every frame the cached display lines must equal a from-scratch render,
// whether the timer ticks, the stream grows, or the timer clears.
func TestTimerLinesMatchFullRender(t *testing.T) {
	for _, width := range []int{12, 40, 80} {
		var tr transcript
		tr.cwd = "/tmp"
		check := func(step string) {
			t.Helper()
			got := strings.Join(tr.linesFor(width), "\n")
			fresh := transcript{
				cwd:       "/tmp",
				blocks:    append([]block(nil), tr.blocks...),
				stream:    append([]byte(nil), tr.stream...),
				thinking:  tr.thinking,
				liveTimer: tr.liveTimer,
			}
			if want := fresh.render(width); got != want {
				t.Fatalf("width=%d %s: cached lines diverged\n got: %q\nwant: %q", width, step, plain(got), plain(want))
			}
		}
		tr.add(block{kind: blockUser, text: "hello"})
		check("user")
		tr.liveTimer = workingLabel(1 * time.Second)
		check("timer start")
		tr.appendStream("streaming text")
		check("stream with timer")
		tr.liveTimer = workingLabel(2 * time.Second)
		check("timer tick")
		tr.finishStream()
		check("stream finish with timer")
		tr.liveTimer = ""
		check("timer clear")
		tr.add(block{kind: blockElapsed, text: workedLabel(2 * time.Second)})
		check("elapsed block")
	}
}

// TestFinishedTimerFreezesIntoElapsedBlock checks the end-to-end lifecycle: a
// started timer renders live, and finishing it leaves a stable "Worked for …"
// block in history that a later turn's indicator appears below.
func TestFinishedTimerFreezesIntoElapsedBlock(t *testing.T) {
	m := newTestModel(t)
	m.startTimer()
	if m.timer == nil || !strings.Contains(m.transcript.liveTimer, "Working…") {
		t.Fatalf("timer did not start: %+v live=%q", m.timer, m.transcript.liveTimer)
	}
	m.timer.start = time.Now().Add(-5 * time.Minute)
	m.finishTimer()
	if m.timer != nil {
		t.Fatalf("timer still running after finish: %+v", m.timer)
	}
	if m.transcript.liveTimer != "" {
		t.Fatalf("live timer survived finish: %q", m.transcript.liveTimer)
	}
	// The frozen total becomes a stable block, and a new turn's running
	// indicator must render below it rather than replacing it.
	m.startTimer()
	got := plain(strings.Join(m.transcript.linesFor(80), "\n"))
	worked, working := strings.Index(got, "Worked for 5m"), strings.Index(got, "Working…")
	if !strings.Contains(got, "Worked for 5m") || !strings.Contains(got, "Working…") {
		t.Fatalf("elapsed block or new timer missing:\n%s", got)
	}
	if worked > working {
		t.Fatalf("new timer rendered above the frozen total:\n%s", got)
	}
}

// TestTimerStopsTickingWhenRunEnds guards the tick loop from restarting after a
// run has already finished, which would keep the app repainting forever, and
// from a stale chain surviving into the next turn.
func TestTimerStopsTickingWhenRunEnds(t *testing.T) {
	m := newTestModel(t)
	if _, cmd := m.Update(timerTickMsg{}); cmd != nil {
		t.Fatal("a tick with no running timer rescheduled itself")
	}
	m.startTimer()
	epoch := m.timer.epoch
	if _, cmd := m.Update(timerTickMsg{epoch: epoch}); cmd == nil {
		t.Fatal("a tick for the running turn did not reschedule")
	}
	m.finishTimer()
	if _, cmd := m.Update(timerTickMsg{epoch: epoch}); cmd != nil {
		t.Fatal("a stale tick rescheduled after the run ended")
	}
}
