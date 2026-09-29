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
	// run is the answer while it streams, nil once it has ended. Its marker
	// says Asking until the model responds, then Thinking or Answering by
	// the kind of text last streamed.
	run *run
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
	runtime := m.runtime
	r, cmd := m.startRun("Asking", func(ctx context.Context, emit func(agent.Event)) error {
		return runtime.SideChat(ctx, question, emit)
	})
	m.side = &sideChat{transcript: transcript{cwd: m.cwd}, run: r}
	m.side.transcript.add(block{kind: blockUser, text: sanitize(question)})
	r.paint(&m.side.transcript, time.Now())
	m.input.Reset()
	m.resetMenu()
	m.openDrawer(&drawer{title: "/btw", transcript: &m.side.transcript, onClose: closeSideChat})
	return m, cmd
}

func (m Model) applySideEvent(event agent.Event) (tea.Model, tea.Cmd) {
	t := &m.side.transcript
	m.side.run.track(t, event)
	switch event.Kind {
	case agent.EventText:
		t.appendStream(sanitize(event.Text))
		m.side.run.setVerb(t, "Answering")
	case agent.EventThinking:
		m.side.run.setVerb(t, "Thinking")
	case agent.EventUsage:
		m.sideSpent += event.Cost
	}
	return m, tea.Batch(m.side.run.wait(), m.scheduleFlush())
}

func (m Model) finishSideChat(err error) (tea.Model, tea.Cmd) {
	t := &m.side.transcript
	elapsed := time.Since(m.side.run.start)
	m.side.run = nil
	t.finishStream()
	t.liveTimer = ""
	if errors.Is(err, app.ErrSideChatTools) || sideToolCallText(t.lastReply()) {
		t.add(block{kind: blockError, text: sideToolsNotice})
	}
	if err != nil && !errors.Is(err, context.Canceled) && !errors.Is(err, app.ErrSideChatTools) {
		t.add(block{kind: blockError, text: sanitize(err.Error())})
	} else {
		t.add(block{kind: blockElapsed, text: markFilled + " Answered in " + formatDuration(elapsed)})
	}
	m.refreshTranscript(true)
	return m, nil
}

// Some models print tool markup as ordinary text when told tools are unavailable.
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
	if m.side.run != nil {
		m.side.run.cancel()
	}
	m.side = nil
}
