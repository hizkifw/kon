package ui

import (
	"context"
	"errors"
	"strings"
	"testing"
	"time"

	tea "charm.land/bubbletea/v2"
	"github.com/hizkifw/kon/internal/agent"
	"github.com/hizkifw/kon/internal/app"
	"github.com/hizkifw/kon/internal/session"
)

type sideRuntime struct {
	Runtime
	run func(context.Context, string, func(agent.Event)) error
}

func (r sideRuntime) SideChat(ctx context.Context, question string, emit func(agent.Event)) error {
	return r.run(ctx, question, emit)
}

// openSide opens an empty side answer's drawer with no request behind it.
func openSide(m *Model) {
	m.runEpoch++
	m.side = &sideChat{run: &run{epoch: m.runEpoch, cancel: func() {}, start: time.Now(), verb: "Asking"}}
	m.openDrawer(&drawer{title: "/btw", transcript: &m.side.transcript, onClose: closeSideChat})
}

func TestBTWParsesFreeFormQuestion(t *testing.T) {
	r := defaultRegistry()
	question := "why  this code?\n  What about `a / b`?"
	parsed, err := r.parse("/btw " + question)
	if err != nil || len(parsed.args) != 1 || parsed.args[0] != question {
		t.Fatalf("parsed = %#v, err = %v", parsed.args, err)
	}
	for _, input := range []string{"/btw", "/btw \n\t"} {
		if _, err := r.parse(input); err == nil || err.Error() != "usage: /btw <question>" {
			t.Fatalf("parse(%q) = %v", input, err)
		}
	}
	items := r.completion(newTestModel(t), "/bt")
	if len(items) != 1 || items[0].Value != "/btw" {
		t.Fatalf("completion = %#v", items)
	}
}

func TestBTWStreamsApartFromMainRun(t *testing.T) {
	m := busyModel(t)
	m.contextTokens = 42
	m.transcript.add(block{kind: blockUser, text: "main task"})
	m.runtime = sideRuntime{Runtime: m.runtime, run: func(ctx context.Context, question string, emit func(agent.Event)) error {
		if question != "why this code?" {
			return errors.New("wrong question: " + question)
		}
		emit(agent.Event{Kind: agent.EventText, Text: "side answer"})
		emit(agent.Event{Kind: agent.EventUsage, Cost: 0.01})
		return nil
	}}
	m.input.SetValue("/btw")
	updated, _ := m.submit()
	m = updated.(Model)
	if m.message != "usage: /btw <question>" {
		t.Fatalf("missing usage hint: %q", m.message)
	}
	m.input.SetValue("/btw why this code?")
	updated, _ = m.submit()
	m = updated.(Model)
	t.Cleanup(m.side.run.cancel)
	if m.message != "" {
		t.Fatalf("valid side question left stale status: %q", m.message)
	}
	if !m.busy() || len(m.inbox.Pending()) != 0 || len(m.queued) != 0 {
		t.Fatal("side question changed the main run or pending messages")
	}
	if got := plain(m.View().Content); !strings.Contains(got, "Asking…") {
		t.Fatalf("side view before the answer = %s", got)
	}
	for {
		select {
		case msg := <-m.side.run.events:
			updated, _ = m.Update(msg)
			m = updated.(Model)
			if msg.done {
				goto finished
			}
		case <-time.After(3 * time.Second):
			t.Fatal("side stream did not finish")
		}
	}
finished:
	m.applyAgentEvent(agent.Event{Kind: agent.EventText, Text: "main answer"})
	m.applyAgentEvent(agent.Event{Kind: agent.EventAssistantDone})
	m.refreshTranscript(true)
	if got := plain(m.View().Content); !strings.Contains(got, "side answer") || strings.Contains(got, "main answer") ||
		!strings.Contains(got, "Answered in") {
		t.Fatalf("side view = %s", got)
	}
	if m.contextTokens != 42 || m.sideSpent != 0.01 || m.spent != 0 {
		t.Fatalf("usage leaked: context=%v, side=%v, main=%v", m.contextTokens, m.sideSpent, m.spent)
	}
	updated, _ = m.Update(tea.KeyPressMsg{Code: tea.KeyEscape})
	m = updated.(Model)
	if strings.Contains(m.statusText(), "usage:") {
		t.Fatalf("dismiss restored stale usage hint: %q", m.statusText())
	}
	if got := plain(m.View().Content); !strings.Contains(got, "main answer") || strings.Contains(got, "side answer") {
		t.Fatalf("restored view = %s", got)
	}
	if m.transcript.lastReply() != "main answer" || len(m.history.entries) != 1 || m.history.entries[0].Text != "/btw why this code?" {
		t.Fatal("side chat polluted transcript or recorded more than its command")
	}
}

func TestBTWDismissCancelsOnlySideAndDropsLateEvents(t *testing.T) {
	m := busyModel(t)
	mainCancelled := false
	m.turn.cancel = func() { mainCancelled = true }
	stopped := make(chan struct{})
	m.runtime = sideRuntime{Runtime: m.runtime, run: func(ctx context.Context, _ string, _ func(agent.Event)) error {
		<-ctx.Done()
		close(stopped)
		return ctx.Err()
	}}
	updated, _ := m.startSideChat("question")
	m = updated.(Model)
	epoch := m.side.run.epoch
	updated, _ = m.Update(tea.KeyPressMsg{Code: tea.KeyEnter})
	m = updated.(Model)
	if m.side == nil || len(m.drawers) != 1 {
		t.Fatal("Enter dismissed the side answer")
	}
	updated, _ = m.Update(tea.KeyPressMsg{Code: tea.KeyEscape})
	m = updated.(Model)
	select {
	case <-stopped:
	case <-time.After(3 * time.Second):
		t.Fatal("dismiss did not cancel side request")
	}
	if mainCancelled || !m.busy() || m.side != nil || len(m.drawers) != 0 {
		t.Fatal("dismiss affected the main run")
	}
	updated, cmd := m.Update(runMsg{epoch: epoch, event: agent.Event{Kind: agent.EventText, Text: "late"}})
	m = updated.(Model)
	if cmd != nil || len(m.transcript.blocks) != 0 {
		t.Fatal("late event changed main transcript")
	}
}

func TestBTWErrorAndModalInput(t *testing.T) {
	m := newTestModel(t)
	openSide(&m)
	updated, _ := m.Update(tea.PasteMsg{Content: "do not steer"})
	m = updated.(Model)
	updated, _ = m.Update(tea.KeyPressMsg{Code: 'x', Text: "x"})
	m = updated.(Model)
	if m.input.Value() != "" || m.View().Cursor != nil {
		t.Fatal("modal accepted hidden input")
	}
	updated, _ = m.Update(runMsg{epoch: 1, done: true, err: errors.New("provider failed")})
	m = updated.(Model)
	if got := plain(m.View().Content); !strings.Contains(got, "provider failed") {
		t.Fatalf("missing error: %s", got)
	}
	if len(m.transcript.blocks) != 0 {
		t.Fatal("side error leaked into main transcript")
	}
}

func TestBTWRejectsUnavailableModel(t *testing.T) {
	for _, phase := range []app.Phase{app.PhaseClosed, app.PhaseFollowing, app.PhaseNeedsConfiguration} {
		m := newTestModel(t)
		m.runtime.(*fakeRuntime).state.Phase = phase
		m.input.SetValue("/btw question")
		updated, cmd := m.submit()
		m = updated.(Model)
		if cmd != nil || m.side != nil || m.input.Value() == "" || m.message == "" {
			t.Fatalf("phase %s did not preserve rejected question", phase)
		}
	}
}

func TestBTWExplainsUnexecutedToolCalls(t *testing.T) {
	for _, tc := range []struct {
		name   string
		chunks []string
		err    error
		notice bool
	}{
		{name: "screenshot", chunks: []string{"<dots_func", "tion_call> <dots_function_call> <built-in> <parameter name=\"command\" string=\"true\">date</parameter> </built-in>"}, notice: true},
		{name: "xml", chunks: []string{"<function_calls><invoke name=\"shell\">date</invoke></function_calls>"}, notice: true},
		{name: "tag with attributes", chunks: []string{"<tool_call name=\"shell\">date</tool_call>"}, notice: true},
		{name: "structured", err: app.ErrSideChatTools, notice: true},
		{name: "both", chunks: []string{"<tool_call>date</tool_call>"}, err: app.ErrSideChatTools, notice: true},
		{name: "partial failure", chunks: []string{"<dots_function_call>date"}, err: errors.New("connection lost"), notice: true},
		{name: "command example", chunks: []string{"You can run `date` in your terminal."}},
		{name: "limitation", chunks: []string{"I cannot check the clock here. Ask in the main conversation."}},
		{name: "similar tag", chunks: []string{"<tool_callback>example</tool_callback>"}},
	} {
		t.Run(tc.name, func(t *testing.T) {
			m := newTestModel(t)
			openSide(&m)
			for _, chunk := range tc.chunks {
				updated, _ := m.Update(runMsg{epoch: 1, event: agent.Event{Kind: agent.EventText, Text: chunk}})
				m = updated.(Model)
			}
			updated, _ := m.Update(runMsg{epoch: 1, done: true, err: tc.err})
			m = updated.(Model)
			count := strings.Count(plain(m.View().Content), "Nothing was executed.")
			if tc.notice && count != 1 || !tc.notice && count != 0 {
				t.Fatalf("notice count = %d, want notice %v: %s", count, tc.notice, plain(m.View().Content))
			}
			if m.side.transcript.lastReply() != strings.Join(tc.chunks, "") {
				t.Fatal("explanation changed the model's answer")
			}
			if len(m.transcript.blocks) != 0 || m.runtime.(*fakeRuntime).runs.Load() != 0 {
				t.Fatal("side tool attempt reached the main conversation")
			}
			m.closeDrawer()
			if strings.Contains(plain(m.View().Content), "Nothing was executed.") {
				t.Fatal("side notice remained after dismissal")
			}
		})
	}
}

func TestBTWTakesOverAFreeSession(t *testing.T) {
	m, runtime := newFollowingModel(t)
	asked := make(chan string, 1)
	m.runtime = sideRuntime{Runtime: runtime, run: func(_ context.Context, question string, _ func(agent.Event)) error {
		asked <- question
		return nil
	}}
	updated, _ := m.startSideChat("question")
	m = updated.(Model)
	if m.follow != nil || m.side == nil || m.message != "" {
		t.Fatalf("following = %v, side = %v, status = %q", m.follow != nil, m.side != nil, m.message)
	}
	t.Cleanup(m.side.run.cancel)
	select {
	case <-asked:
	case <-time.After(3 * time.Second):
		t.Fatal("side question was not asked after taking over")
	}
}

func TestBTWWhileHeldStaysReadOnly(t *testing.T) {
	m, runtime := newFollowingModel(t)
	runtime.takeOverErr = session.ErrInUse
	updated, _ := m.startSideChat("question")
	m = updated.(Model)
	if m.message != "read-only: still open in another session" || m.follow == nil || m.side != nil {
		t.Fatalf("status = %q, following = %v, side = %v", m.message, m.follow != nil, m.side != nil)
	}
}

func TestBTWDragCopiesTheSideAnswer(t *testing.T) {
	m := transcriptModel(t, block{kind: blockAssistant, text: "main reply"})
	m.runtime = sideRuntime{Runtime: m.runtime, run: func(ctx context.Context, _ string, _ func(agent.Event)) error {
		<-ctx.Done()
		return nil
	}}
	updated, _ := m.startSideChat("question")
	m = updated.(Model)
	t.Cleanup(m.side.run.cancel)
	for _, msg := range []runMsg{
		{epoch: m.side.run.epoch, event: agent.Event{Kind: agent.EventText, Text: "the **side** reply"}},
		{epoch: m.side.run.epoch, done: true},
	} {
		updated, _ = m.Update(msg)
		m = updated.(Model)
	}
	x, y := cellOf(t, m, "the side")
	m, cmd := drag(m, x, y, x+len("the side reply")-1, y)
	if got := selectedText(t, cmd); got != "the **side** reply" {
		t.Fatalf("copied %q", got)
	}
	if m.side == nil || m.transcript.selection != nil {
		t.Fatal("selecting in the side view closed it or touched the main transcript")
	}
}

func TestBTWMarkerFollowsWhatTheModelIsDoing(t *testing.T) {
	m := busyModel(t)
	m.runtime = sideRuntime{Runtime: m.runtime, run: func(ctx context.Context, _ string, _ func(agent.Event)) error {
		<-ctx.Done()
		return nil
	}}
	updated, _ := m.startSideChat("question")
	m = updated.(Model)
	t.Cleanup(m.side.run.cancel)
	for _, step := range []struct {
		event agent.Event
		want  string
	}{
		{agent.Event{Kind: agent.EventUsage}, "Asking…"},
		{agent.Event{Kind: agent.EventThinking}, "Thinking…"},
		{agent.Event{Kind: agent.EventText, Text: "answer"}, "Answering…"},
	} {
		updated, _ = m.Update(runMsg{epoch: m.side.run.epoch, event: step.event})
		m = updated.(Model)
		if got := m.side.transcript.liveTimer; !strings.Contains(got, step.want) {
			t.Fatalf("marker after %v = %q, want %q", step.event.Kind, got, step.want)
		}
	}
}

// TestBTWTicksStopWithTheAnswer checks that a side answer's marker ticks only
// while it streams, and that its ticks never repaint the main turn's marker.
func TestBTWTicksStopWithTheAnswer(t *testing.T) {
	m := busyModel(t)
	openSide(&m)
	epoch := m.side.run.epoch
	if epoch == m.turn.epoch {
		t.Fatal("the side answer shares the main turn's epoch")
	}
	updated, cmd := m.Update(runTickMsg{epoch: epoch})
	m = updated.(Model)
	if cmd == nil || m.transcript.liveTimer != "" {
		t.Fatalf("side tick: rescheduled = %v, main marker = %q", cmd != nil, m.transcript.liveTimer)
	}
	updated, _ = m.Update(runMsg{epoch: epoch, done: true})
	m = updated.(Model)
	if _, cmd := m.Update(runTickMsg{epoch: epoch}); cmd != nil {
		t.Fatal("a tick rescheduled after the side answer ended")
	}
}
