package ui

import (
	"context"
	"errors"
	"strings"
	"time"

	tea "charm.land/bubbletea/v2"
	"github.com/hizkifw/kon/internal/agent"
	"github.com/hizkifw/kon/internal/app"
)

// sideChat owns a temporary view; the main transcript keeps receiving its
// events while this one is visible.
type sideChat struct {
	transcript transcript
	cancel     context.CancelFunc
	events     <-chan sideEventMsg
	position   previewReturn
	done       bool
}

type sideEventMsg struct {
	epoch int
	event agent.Event
	done  bool
	err   error
}

const sideToolsNotice = "Nothing was executed. /btw has no tools. Ask in the main conversation to use tools."

func (m Model) startSideChat(question string) (tea.Model, tea.Cmd) {
	state := m.runtime.State()
	if state.Phase != app.PhaseReady && state.Phase != app.PhaseRunning {
		m.message = "configure a model and open a writable session before using /btw"
		return m, nil
	}
	m.message = ""
	m.closePreview()
	m.sideEpoch++
	epoch := m.sideEpoch
	ctx, cancel := context.WithCancel(m.ctx)
	events := make(chan sideEventMsg)
	m.side = &sideChat{
		transcript: transcript{cwd: m.cwd}, cancel: cancel, events: events,
		position: previewReturn{offset: m.viewport.YOffset(), atBottom: m.viewport.AtBottom()},
	}
	m.side.transcript.add(block{kind: blockContext, text: "/btw · temporary answer · no tools"})
	m.side.transcript.add(block{kind: blockUser, text: sanitize(question)})
	m.side.transcript.liveTimer = "Answering…"
	m.input.Reset()
	m.resetMenu()
	m.refreshTranscript(false)
	m.viewport.GotoBottom()
	runtime := m.runtime
	go func() {
		defer close(events)
		defer cancel()
		err := runtime.SideChat(ctx, question, func(event agent.Event) {
			select {
			case events <- sideEventMsg{epoch: epoch, event: event}:
			case <-ctx.Done():
			}
		})
		select {
		case events <- sideEventMsg{epoch: epoch, done: true, err: err}:
		case <-ctx.Done():
		}
	}()
	return m, waitSideEvent(events)
}

func waitSideEvent(events <-chan sideEventMsg) tea.Cmd {
	return func() tea.Msg {
		msg, ok := <-events
		if !ok {
			return nil
		}
		return msg
	}
}

func (m Model) updateSideChat(msg sideEventMsg) (tea.Model, tea.Cmd) {
	if m.side == nil || msg.epoch != m.sideEpoch {
		return m, nil
	}
	if msg.done {
		m.side.done = true
		m.side.transcript.finishStream()
		m.side.transcript.liveTimer = ""
		if errors.Is(msg.err, app.ErrSideChatTools) || sideToolCallText(m.side.transcript.lastReply()) {
			m.side.transcript.add(block{kind: blockError, text: sideToolsNotice})
		}
		if msg.err != nil && !errors.Is(msg.err, context.Canceled) && !errors.Is(msg.err, app.ErrSideChatTools) {
			m.side.transcript.add(block{kind: blockError, text: sanitize(msg.err.Error())})
		}
		m.refreshTranscript(true)
		return m, nil
	}
	switch msg.event.Kind {
	case agent.EventText:
		m.side.transcript.appendStream(sanitize(msg.event.Text))
	case agent.EventUsage:
		m.sideSpent += msg.event.Cost
	}
	var flush tea.Cmd
	if !m.flushPending {
		m.flushPending = true
		flush = tea.Tick(streamFrameInterval, func(time.Time) tea.Msg { return flushTranscriptMsg{} })
	}
	return m, tea.Batch(waitSideEvent(m.side.events), flush)
}

// Some models print tool markup as ordinary text despite tool_choice none.
// This heuristic only adds an explanation; the answer is never
// interpreted or executed, and quoted examples remain visible as written.
func sideToolCallText(text string) bool {
	text = strings.ToLower(text)
	for _, tag := range []string{"dots_function_call", "tool_call", "tool_calls", "function_call", "function_calls", "invoke"} {
		for _, end := range []string{">", " ", "\t", "\n"} {
			if strings.Contains(text, "<"+tag+end) {
				return true
			}
		}
	}
	return false
}

func (m *Model) closeSideChat() {
	if m.side == nil {
		return
	}
	m.side.cancel()
	position := m.side.position
	m.side = nil
	m.refreshTranscript(false)
	if position.atBottom {
		m.viewport.GotoBottom()
	} else {
		m.viewport.SetYOffset(position.offset)
	}
}

func (m Model) sideKey(key string) (tea.Model, tea.Cmd) {
	switch key {
	case "esc", "ctrl+c":
		m.closeSideChat()
	case "enter":
		if m.side.done {
			m.closeSideChat()
		}
	case "ctrl+d":
		m.closeSideChat()
		if m.runCancel != nil {
			m.runCancel()
		}
		return m, tea.Quit
	case "pgup", "up":
		m.viewport.PageUp()
	case "pgdown", "down":
		m.viewport.PageDown()
	}
	return m, nil
}
