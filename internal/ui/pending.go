package ui

import (
	"context"
	"errors"
	"fmt"
	"slices"
	"strconv"
	"strings"

	tea "charm.land/bubbletea/v2"
	"charm.land/lipgloss/v2"

	"github.com/hizkifw/kon/internal/agent"
)

// maxPendingRows caps the strip of pending messages above the status line; the
// last row summarizes the rest once there are more.
const maxPendingRows = 3

// steer hands text to the running agent, which reads it before its next
// request. Several steers stack and arrive together.
func (m Model) steer(text string) (tea.Model, tea.Cmd) {
	if err := m.history.append(m.cwd, text); err != nil {
		m.status = "error: " + err.Error()
		return m, nil
	}
	m.inbox.Push(text)
	m.input.Reset()
	m.syncSteering()
	return m, nil
}

// canQueue reports whether Tab should queue the prompt rather than complete
// it. Only prose queues: a slash command is completed or run, never queued.
// Idle with an empty queue there is nothing to wait behind, so Enter sends.
func (m Model) canQueue() bool {
	text := strings.TrimSpace(m.input.Value())
	return text != "" && !strings.HasPrefix(text, "/") && (m.busy || len(m.queued) > 0)
}

// enqueue holds the prompt until the runs ahead of it have finished.
func (m Model) enqueue() (tea.Model, tea.Cmd) {
	text := strings.TrimSpace(m.input.Value())
	if err := m.history.append(m.cwd, text); err != nil {
		m.status = "error: " + err.Error()
		return m, nil
	}
	m.queued = append(slices.Clone(m.queued), text)
	m.input.Reset()
	m.resize()
	return m, nil
}

// sendQueued starts a run for the oldest queued prompt.
func (m Model) sendQueued() (tea.Model, tea.Cmd) {
	if !m.canSend() {
		return m, nil
	}
	text := m.queued[0]
	m.queued = slices.Clone(m.queued[1:])
	return m.send(text)
}

// dispatchPending decides what runs after a run ends. Steering the runner
// never read goes first, as its own run: after an interrupt that is the point
// of pressing Esc with a steer pending. The queue advances only after a clean
// finish; an interrupt or failure holds it until Enter on an empty prompt, and
// a failure moves its unread steering to the head of the queue rather than
// sending it into the same error.
func (m Model) dispatchPending(err error) (tea.Model, tea.Cmd) {
	clean := err == nil || errors.Is(err, agent.ErrNothingToCompact)
	leftover := m.inbox.Take()
	m.syncSteering()
	if len(leftover) > 0 {
		text := strings.Join(leftover, "\n\n")
		if (clean || errors.Is(err, context.Canceled)) && m.canSend() {
			return m.send(text)
		}
		m.queued = append([]string{text}, m.queued...)
		m.resize()
		return m, nil
	}
	if len(m.queued) == 0 {
		return m, nil
	}
	if clean {
		return m.sendQueued()
	}
	if errors.Is(err, context.Canceled) {
		m.status = "interrupted · queue held, Enter sends the next"
	}
	return m, nil
}

// noticeMsg carries a notice from kon, such as a background job exiting, that
// the agent should hear.
type noticeMsg struct{ text string }

// waitNotice waits for the runtime's next notice. It is re-armed after each
// one, and stops once the runtime closes the channel.
func waitNotice(notices <-chan string) tea.Cmd {
	return func() tea.Msg {
		text, ok := <-notices
		if !ok {
			return nil
		}
		return noticeMsg{text: text}
	}
}

// deliverNotice passes a notice to the agent: at its next request while a run
// is in flight, or by starting a run when idle, so a job finishing after the
// agent's turn ended still gets its attention.
func (m Model) deliverNotice(text string) (tea.Model, tea.Cmd) {
	m.jobs = m.runtime.RunningJobs()
	if m.busy || !m.canSend() {
		m.inbox.PushNotice(text)
		return m, nil
	}
	return m.send(text)
}

// syncSteering refreshes the mirror of the inbox and re-lays-out the frame,
// since the pending strip may have changed height.
func (m *Model) syncSteering() {
	m.steering = m.inbox.Pending()
	m.resize()
}

// pendingRows lists pending messages in delivery order: steering, which the
// agent reads first, then the queue.
func (m Model) pendingRows() []string {
	label := lipgloss.NewStyle().Foreground(colorAccent)
	text := lipgloss.NewStyle().Foreground(colorFaint)
	rows := make([]string, 0, len(m.steering)+len(m.queued))
	for _, message := range m.steering {
		rows = append(rows, label.Render(" ↳ steer ")+text.Render(" "+oneLine(message)))
	}
	for _, message := range m.queued {
		rows = append(rows, label.Render(" ⏵ queue ")+text.Render(" "+oneLine(message)))
	}
	if len(rows) > maxPendingRows {
		more := len(rows) - maxPendingRows + 1
		rows = append(rows[:maxPendingRows-1], text.Render(fmt.Sprintf(" +%d more · /queue to edit", more)))
	}
	return rows
}

// pendingView renders the pending strip, or "" when nothing is pending.
func (m Model) pendingView() string {
	rows := m.pendingRows()
	for i, row := range rows {
		rows[i] = fitLine(row, m.width)
	}
	return strings.Join(rows, "\n")
}

func (m Model) pendingHeight() int { return min(len(m.steering)+len(m.queued), maxPendingRows) }

// oneLine flattens a message to a single line for the strip and the picker.
func oneLine(text string) string { return strings.Join(strings.Fields(sanitize(text)), " ") }

// placeholder names what Enter and Tab do in the current state, where the
// prompt is empty and so has room to say it.
func (m Model) placeholder() string {
	switch {
	case m.busy:
		return "Ask kon… · ⇥ queue · ⏎ steer"
	case len(m.queued) > 0:
		return "Ask kon… · ⏎ send next queued"
	}
	return "Ask kon…"
}

// completePending offers each pending message by its position, followed by
// clear.
func completePending(m Model, prefix string) []menuItem {
	var items []menuItem
	add := func(value, label, description string) {
		if strings.HasPrefix(value, prefix) {
			items = append(items, menuItem{Value: value, Label: label, Description: description})
		}
	}
	n := 0
	for _, message := range m.steering {
		n++
		add(strconv.Itoa(n), strconv.Itoa(n)+" steer", oneLine(message))
	}
	for _, message := range m.queued {
		n++
		add(strconv.Itoa(n), strconv.Itoa(n)+" queue", oneLine(message))
	}
	if n > 0 {
		add("clear", "", "drop every pending message")
	}
	return items
}

// managePending runs /queue. Bare, it opens the picker; with a position it
// pulls that message back into the prompt, which cancels it and lets it be
// edited and sent again with either key; "clear" drops everything.
func (m Model) managePending(args []string) (tea.Model, tea.Cmd) {
	if len(args) == 0 {
		if len(m.steering)+len(m.queued) == 0 {
			m.input.Reset()
			m.status = "nothing pending"
			return m, nil
		}
		m.input.SetValue("/queue ")
		m.input.CursorEnd()
		m.refreshInput()
		return m, nil
	}
	if args[0] == "clear" {
		dropped := m.inbox.WithdrawAll() + len(m.queued)
		m.queued = nil
		m.input.Reset()
		m.syncSteering()
		m.status = fmt.Sprintf("dropped %d pending", dropped)
		return m, nil
	}
	n, err := strconv.Atoi(args[0])
	if err != nil || n < 1 || n > len(m.steering)+len(m.queued) {
		m.status = "no pending message " + args[0]
		return m, nil
	}
	var text string
	if i := n - 1; i < len(m.steering) {
		removed, ok := m.inbox.Remove(i)
		if !ok {
			m.input.Reset()
			m.syncSteering()
			m.status = "already sent to the agent"
			return m, nil
		}
		text = removed
		m.syncSteering()
	} else {
		i -= len(m.steering)
		text = m.queued[i]
		m.queued = slices.Delete(slices.Clone(m.queued), i, i+1)
	}
	m.input.SetValue(text)
	m.input.CursorEnd()
	m.refreshInput()
	m.status = "pulled back for editing"
	return m, nil
}
