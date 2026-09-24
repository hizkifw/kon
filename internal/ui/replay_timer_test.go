package ui

import (
	"strings"
	"testing"
	"time"

	"github.com/hizkifw/kon/internal/session"
)

// TestReplayMatchesLiveAfterManualCompact round-trips a real session store
// through the case that timestamp inference got wrong: a manual compaction long
// after a finished turn must not stretch that turn's total, and the total must
// sit above the compaction marker exactly as it did live.
func TestReplayMatchesLiveAfterManualCompact(t *testing.T) {
	store, err := session.New(t.TempDir(), "/tmp", "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	defer store.Close()
	if _, err := store.AppendTurnStart(); err != nil {
		t.Fatal(err)
	}
	userID, err := store.AppendMessage(session.TextMessage(session.RoleUser, "do the thing"))
	if err != nil {
		t.Fatal(err)
	}
	if _, err := store.AppendMessage(session.TextMessage(session.RoleAssistant, "done")); err != nil {
		t.Fatal(err)
	}
	if _, err := store.AppendTurnEnd(2*time.Minute + 30*time.Second); err != nil {
		t.Fatal(err)
	}
	if _, err := store.AppendCompaction("summary", userID, 1200, false, nil); err != nil {
		t.Fatal(err)
	}
	entries := store.ActivePath()

	m := newTestModel(t)
	m.transcript.add(block{kind: blockUser, text: "do the thing"})
	m.transcript.add(block{kind: blockAssistant, text: "done"})
	m.transcript.add(block{kind: blockElapsed, text: workedLabel(2*time.Minute + 30*time.Second)})
	m.transcript.add(block{kind: blockContext, text: compactedLabel(1200, false)})
	live := plain(strings.Join(m.transcript.linesFor(80), "\n"))

	replay := newTestModel(t)
	replay.applyHistory(entries)
	got := plain(strings.Join(replay.transcript.linesFor(80), "\n"))

	if !strings.Contains(got, "Worked for 2m 30s") {
		t.Fatalf("replayed total missing:\n%s", got)
	}
	if got != live {
		t.Fatalf("replayed transcript differs from live\n got: %q\nlive: %q", got, live)
	}
}
