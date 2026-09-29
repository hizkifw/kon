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

// sideChat is a side answer streaming into its own drawer; the main
// transcript keeps receiving its events underneath.
type sideChat struct {
	transcript transcript
	cancel     context.CancelFunc
	events     <-chan sideEventMsg
	done       bool
	// start times the answer for its marker, and verb names what the model
	// is doing: Asking until it responds, then Thinking or Answering by the
	// kind of text last streamed.
	start time.Time
	verb  string
}

// sideTickMsg advances the side answer's marker; epoch drops a tick from a
// side chat that has since closed.
type sideTickMsg struct{ epoch int }

func sideTick(epoch int) tea.Cmd {
	return tea.Tick(time.Second, func(time.Time) tea.Msg { return sideTickMsg{epoch: epoch} })
}

type sideEventMsg struct {
	epoch int
	event agent.Event
	done  bool
	err   error
}

const sideToolsNotice = "Nothing was executed. /btw has no tools. Ask in the main conversation to use tools."

func (m Model) startSideChat(question string) (tea.Model, tea.Cmd) {
	// A side question needs a writable session like any prompt, so a followed
	// session its writer has let go is taken over first.
	if !m.takeOver() {
		return m, nil
	}
	state := m.runtime.State()
	if state.Phase != app.PhaseReady && state.Phase != app.PhaseRunning {
		m.say(toneWarn, "configure a model and open a writable session before using /btw")
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
		start: time.Now(), verb: "Asking",
	}
	m.side.transcript.add(block{kind: blockUser, text: sanitize(question)})
	m.side.syncTimer(time.Now())
	m.input.Reset()
	m.resetMenu()
	m.openDrawer(&drawer{title: "/btw", transcript: &m.side.transcript, onClose: closeSideChat})
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
	return m, tea.Batch(waitSideEvent(events), sideTick(epoch))
}

// syncTimer repaints the side answer's running marker from the clock.
func (s *sideChat) syncTimer(now time.Time) {
	s.transcript.liveTimer = runningLabel(s.verb, now.Sub(s.start))
}

// setVerb changes the marker's verb at once rather than on the next tick.
func (s *sideChat) setVerb(verb string) {
	if s.verb != verb {
		s.verb = verb
		s.syncTimer(time.Now())
	}
}

func (m Model) tickSideChat(msg sideTickMsg) (tea.Model, tea.Cmd) {
	if m.side == nil || m.side.done || msg.epoch != m.sideEpoch {
		return m, nil
	}
	m.side.syncTimer(time.Now())
	m.refreshTranscript(true)
	return m, sideTick(msg.epoch)
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
		} else {
			m.side.transcript.add(block{kind: blockElapsed, text: markFilled + " Answered in " + formatDuration(time.Since(m.side.start))})
		}
		m.refreshTranscript(true)
		return m, nil
	}
	switch msg.event.Kind {
	case agent.EventText:
		m.side.transcript.appendStream(sanitize(msg.event.Text))
		m.side.setVerb("Answering")
	case agent.EventThinking:
		m.side.setVerb("Thinking")
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

// closeSideChat is the side answer's drawer closing: an unfinished answer is
// cancelled, and its late events are dropped once m.side is gone.
func closeSideChat(m *Model) {
	if m.side == nil {
		return
	}
	m.side.cancel()
	m.side = nil
}
