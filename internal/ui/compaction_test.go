package ui

import (
	"errors"
	"strings"
	"testing"

	"kon.kitsu.red/core/agent"
)

// summaryText is long enough to wrap and has the headings a real summary has.
const summaryText = "## Goal\n\nMake the build faster by caching the dependency graph between runs.\n\n## Progress\n\n- Profiled the loader\n- Found the repeated parse"

func compactionEvents(m *Model) {
	m.applyAgentEvent(agent.Event{Kind: agent.EventCompacting, Tokens: 12_300, Estimated: true})
	for _, chunk := range strings.SplitAfter(summaryText, " ") {
		m.applyAgentEvent(agent.Event{Kind: agent.EventCompactionText, Text: chunk})
	}
}

func TestAutoCompactionStreamsInsideTheTurn(t *testing.T) {
	m := newTestModel(t)
	m.transcript.add(block{kind: blockUser, text: "speed it up"})
	fakeTurn(&m)
	compactionEvents(&m)
	if !strings.Contains(m.transcript.liveTimer, "Compacting…") {
		t.Fatalf("marker while compacting = %q", m.transcript.liveTimer)
	}
	streaming := plain(strings.Join(m.transcript.linesFor(80), "\n"))
	if !strings.Contains(streaming, "Profiled the loader") {
		t.Fatalf("summary not streaming:\n%s", streaming)
	}
	m.applyAgentEvent(agent.Event{Kind: agent.EventCompacted, Text: summaryText, Tokens: 12_300, Estimated: true})
	if !strings.Contains(m.transcript.liveTimer, "Working…") {
		t.Fatalf("marker after compacting = %q", m.transcript.liveTimer)
	}
	done := plain(strings.Join(m.transcript.linesFor(80), "\n"))
	// The summary stays where it streamed, whole; the running marker under it
	// is now the compaction's own, and the turn's marker follows.
	summaryEnd := strings.Index(streaming, "○ Compacting")
	if summaryEnd < 0 {
		summaryEnd = strings.Index(streaming, "● Compacting")
	}
	if summaryEnd < 0 || !strings.HasPrefix(done, streaming[:summaryEnd]) {
		t.Fatalf("summary moved when it was kept\nstreaming:\n%s\ndone:\n%s", streaming, done)
	}
	marker, turn := strings.Index(done, "● Compacted ~12.3k tokens"), strings.Index(done, "Working…")
	if marker < summaryEnd || turn < marker {
		t.Fatalf("markers out of order:\n%s", done)
	}
}

func TestRestartedSummaryDropsWhatStreamedFirst(t *testing.T) {
	m := newTestModel(t)
	m.applyAgentEvent(agent.Event{Kind: agent.EventCompacting})
	m.applyAgentEvent(agent.Event{Kind: agent.EventCompactionText, Text: "first attempt"})
	m.applyAgentEvent(agent.Event{Kind: agent.EventCompacting})
	m.applyAgentEvent(agent.Event{Kind: agent.EventCompactionText, Text: "second attempt"})
	m.applyAgentEvent(agent.Event{Kind: agent.EventCompacted, Text: "second attempt", Tokens: 100})
	got := plain(strings.Join(m.transcript.linesFor(80), "\n"))
	if strings.Contains(got, "first attempt") || strings.Count(got, "second attempt") != 1 {
		t.Fatalf("transcript:\n%s", got)
	}
}

func TestFailedCompactKeepsWhatStreamedAndSaysSo(t *testing.T) {
	m := newTestModel(t)
	m.input.SetValue("/compact")
	updated, _ := m.submit()
	m = updated.(Model)
	compactionEvents(&m)
	m, _ = update(m, turnDone(m, errors.New("provider died\nstatus 502")))
	got := plain(strings.Join(m.transcript.linesFor(80), "\n"))
	stopped, failure := strings.Index(got, "○ Compaction stopped"), strings.Index(got, "provider died")
	if !strings.Contains(got, "Profiled the loader") || stopped < 0 || failure < stopped {
		t.Fatalf("transcript:\n%s", got)
	}
	// A /compact leaves its compaction block as its record, not a turn total.
	if strings.Contains(got, "Worked for") || m.transcript.liveTimer != "" {
		t.Fatalf("a /compact left a turn marker:\n%s", got)
	}
}
