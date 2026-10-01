package ui

import (
	"fmt"
	"time"

	tea "charm.land/bubbletea/v2"
)

// spending is what kon has spent in US dollars, as the status line shows it.
type spending struct {
	// own is what the live session's own responses have cost, side what
	// /btw answers have, and subagents what its subagents have, as of the
	// last read of their sessions.
	own, side, subagents float64
	// polling marks a read of the subagents in flight, and epoch counts
	// sessions opened so a read for an earlier one is dropped.
	polling bool
	epoch   int
}

// spendInterval is how often subagent sessions are read again while a
// subagent may be working.
const spendInterval = time.Second

// spendMsg carries what subagents have spent. Reading their sessions is file
// work, so it happens off the UI goroutine. epoch drops a read made for a
// session the UI has since left, and polled marks the poll's own read.
type spendMsg struct {
	cost   float64
	epoch  int
	polled bool
}

// pollSpend schedules the next read of subagent spend while a subagent could
// be working: during a run, whose shell may be running one in the foreground,
// while background jobs run, or while following a session another kon is
// writing, whose runs and jobs cannot be seen from here. It keeps a single
// poll in flight and returns nil once there is nothing to watch. A run starts
// it with its first event; a subagent can only start from a tool call, which
// has one.
func (m *Model) pollSpend() tea.Cmd {
	if m.spend.polling || (!m.busy() && m.jobs.running == 0 && m.follow.replay == nil) {
		return nil
	}
	m.spend.polling = true
	return tea.Tick(spendInterval, m.readSpend(true))
}

// loadSpend reads subagent spend once, right away: for a session just
// opened, or when a subagent may have just written its last response.
func (m *Model) loadSpend() tea.Cmd {
	read := m.readSpend(false)
	return func() tea.Msg { return read(time.Now()) }
}

// readSpend reads subagent spend for the session shown now. It captures the
// runtime rather than the model, so it can run on another goroutine.
func (m *Model) readSpend(polled bool) func(time.Time) tea.Msg {
	runtime, epoch := m.runtime, m.spend.epoch
	return func(time.Time) tea.Msg {
		return spendMsg{cost: runtime.SubagentUsage().Cost, epoch: epoch, polled: polled}
	}
}

// applySpend adopts a read of subagent spend and schedules the next one.
func (m *Model) applySpend(msg spendMsg) tea.Cmd {
	if msg.polled {
		m.spend.polling = false
	}
	if msg.epoch == m.spend.epoch {
		m.spend.subagents = msg.cost
	}
	m.jobs.running = m.runtime.RunningJobs()
	return m.pollSpend()
}

// resetSpend starts counting subagent spend again for a session just opened.
func (m *Model) resetSpend() tea.Cmd {
	m.spend.side = 0
	m.spend.epoch++
	m.spend.subagents = 0
	return m.loadSpend()
}

// formatCost renders dollars to the cent. A smaller total shows as under a
// cent, so a cheap session never reads as free.
func formatCost(usd float64) string {
	if usd < 0.01 {
		return "<$0.01"
	}
	return fmt.Sprintf("$%.2f", usd)
}
