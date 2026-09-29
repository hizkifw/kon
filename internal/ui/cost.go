package ui

import (
	"fmt"
	"time"

	tea "charm.land/bubbletea/v2"
)

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
	if m.spendPolling || (!m.busy() && m.jobs == 0 && m.follow == nil) {
		return nil
	}
	m.spendPolling = true
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
	runtime, epoch := m.runtime, m.spendEpoch
	return func(time.Time) tea.Msg {
		return spendMsg{cost: runtime.SubagentUsage().Cost, epoch: epoch, polled: polled}
	}
}

// applySpend adopts a read of subagent spend and schedules the next one.
func (m *Model) applySpend(msg spendMsg) tea.Cmd {
	if msg.polled {
		m.spendPolling = false
	}
	if msg.epoch == m.spendEpoch {
		m.subagentSpent = msg.cost
	}
	m.jobs = m.runtime.RunningJobs()
	return m.pollSpend()
}

// resetSpend starts counting subagent spend again for a session just opened.
func (m *Model) resetSpend() tea.Cmd {
	m.sideSpent = 0
	m.spendEpoch++
	m.subagentSpent = 0
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
