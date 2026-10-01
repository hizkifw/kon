package ui

import (
	"errors"
	"time"

	tea "charm.land/bubbletea/v2"
	"github.com/hizkifw/kon/core/session"
	"github.com/hizkifw/kon/internal/app"
)

// followInterval is how often a followed session is read for what its writer
// appended. The writer persists whole messages, so a follower sees each one as
// it completes rather than as it streams.
const followInterval = 500 * time.Millisecond

type followTickMsg struct{ epoch int }

type followedMsg struct {
	epoch    int
	followed app.Followed
	err      error
}

func followTick(epoch int) tea.Cmd {
	return tea.Tick(followInterval, func(time.Time) tea.Msg { return followTickMsg{epoch: epoch} })
}

// loadSession replays the runtime's session into the transcript. A session
// another kon has open is replayed without closing its last turn, which may
// still be running there, and is then followed; the returned command starts
// that. Every load ends any earlier follow, whose ticks carry a stale epoch.
func (m *Model) loadSession() tea.Cmd {
	m.follow = nil
	m.followEpoch++
	history := m.runtime.SessionHistory()
	m.spent = session.TotalUsage(history).Cost
	if !m.runtime.State().Following() {
		m.applyHistory(history)
		return nil
	}
	m.follow = &replayState{}
	m.replay(&m.transcript, m.follow, history)
	m.setFollowMode(false)
	return followTick(m.followEpoch)
}

// pollFollowed reads the followed session off the update loop.
func (m Model) pollFollowed(epoch int) tea.Cmd {
	runtime := m.runtime
	return func() tea.Msg {
		followed, err := runtime.Follow()
		return followedMsg{epoch: epoch, followed: followed, err: err}
	}
}

// applyFollowed adds what the writer appended and schedules the next read. An
// answer for a session this kon has since left is dropped.
func (m *Model) applyFollowed(msg followedMsg) tea.Cmd {
	if m.follow == nil || msg.epoch != m.followEpoch || msg.followed.Session != m.runtime.SessionID() {
		return nil
	}
	if len(msg.followed.Entries) > 0 {
		m.replay(&m.transcript, m.follow, msg.followed.Entries)
		m.spent += session.TotalUsage(msg.followed.Entries).Cost
		m.refreshTranscript(true)
	}
	switch {
	case errors.Is(msg.err, session.ErrRemoved):
		m.followMode = "read-only: session was removed"
		return nil
	case msg.err != nil:
		m.say(toneDanger, "error: "+msg.err.Error())
		return nil
	}
	m.setFollowMode(msg.followed.Free)
	return followTick(m.followEpoch)
}

// setFollowMode names what the followed session is doing: whether its writer
// is working, or has let go so a prompt here would take it over.
func (m *Model) setFollowMode(free bool) {
	m.followMode = "read-only: open in another session"
	switch {
	case free:
		m.followMode = "read-only: session is free, send a prompt to continue here"
	case m.follow.open:
		m.followMode += " · working"
	}
}

// takeOver makes this kon the writer of a followed session before a prompt or
// compaction writes to it. It reports whether the session is now writable.
func (m *Model) takeOver() bool {
	if m.follow == nil {
		return true
	}
	missed, err := m.runtime.TakeOver()
	if errors.Is(err, session.ErrInUse) {
		m.say(toneWarn, "read-only: still open in another session")
		return false
	}
	if err != nil {
		m.say(toneDanger, "error: "+err.Error())
		return false
	}
	m.replay(&m.transcript, m.follow, missed)
	m.spent += session.TotalUsage(missed).Cost
	// The writer has let go, so a turn it left open was never finished.
	m.follow.finish(&m.transcript)
	m.follow = nil
	m.followEpoch++
	m.syncRuntimeState()
	m.seedContextUsage()
	return true
}
