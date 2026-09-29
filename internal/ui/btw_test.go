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
)

type sideRuntime struct {
	Runtime
	run func(context.Context, string, func(agent.Event)) error
}

func (r sideRuntime) SideChat(ctx context.Context, question string, emit func(agent.Event)) error {
	return r.run(ctx, question, emit)
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
	t.Cleanup(m.side.cancel)
	if m.message != "" {
		t.Fatalf("valid side question left stale status: %q", m.message)
	}
	if !m.busy || len(m.inbox.Pending()) != 0 || len(m.queued) != 0 {
		t.Fatal("side question changed the main run or pending messages")
	}
	for {
		select {
		case msg := <-m.side.events:
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
	if got := plain(m.View().Content); !strings.Contains(got, "side answer") || strings.Contains(got, "main answer") {
		t.Fatalf("side view = %s", got)
	}
	if m.contextTokens != 42 || m.sideSpent != 0.01 || m.spent != 0 {
		t.Fatalf("usage leaked: context=%v, side=%v, main=%v", m.contextTokens, m.sideSpent, m.spent)
	}
	updated, _ = m.sideKey("enter")
	m = updated.(Model)
	if strings.Contains(m.statusText(), "usage:") {
		t.Fatalf("dismiss restored stale usage hint: %q", m.statusText())
	}
	if got := plain(m.View().Content); !strings.Contains(got, "main answer") || strings.Contains(got, "side answer") {
		t.Fatalf("restored view = %s", got)
	}
	if m.transcript.lastReply() != "main answer" || len(m.history.entries) != 0 {
		t.Fatal("side chat polluted transcript or prompt history")
	}
}

func TestBTWDismissCancelsOnlySideAndDropsLateEvents(t *testing.T) {
	m := busyModel(t)
	mainCancelled := false
	m.runCancel = func() { mainCancelled = true }
	stopped := make(chan struct{})
	m.runtime = sideRuntime{Runtime: m.runtime, run: func(ctx context.Context, _ string, _ func(agent.Event)) error {
		<-ctx.Done()
		close(stopped)
		return ctx.Err()
	}}
	updated, _ := m.startSideChat("question")
	m = updated.(Model)
	epoch := m.sideEpoch
	updated, _ = m.Update(tea.KeyPressMsg{Code: tea.KeyEnter})
	m = updated.(Model)
	if m.side == nil {
		t.Fatal("Enter dismissed an unfinished side answer")
	}
	if hint := plain(m.View().Content); strings.Contains(hint, "Esc or Enter") {
		t.Fatal("streaming side view advertised Enter dismissal")
	}
	updated, _ = m.Update(tea.KeyPressMsg{Code: tea.KeyEscape})
	m = updated.(Model)
	select {
	case <-stopped:
	case <-time.After(3 * time.Second):
		t.Fatal("dismiss did not cancel side request")
	}
	if mainCancelled || !m.busy || m.side != nil {
		t.Fatal("dismiss affected the main run")
	}
	updated, cmd := m.Update(sideEventMsg{epoch: epoch, event: agent.Event{Kind: agent.EventText, Text: "late"}})
	m = updated.(Model)
	if cmd != nil || len(m.transcript.blocks) != 0 {
		t.Fatal("late event changed main transcript")
	}
}

func TestBTWErrorAndModalInput(t *testing.T) {
	m := newTestModel(t)
	m.sideEpoch = 1
	m.side = &sideChat{cancel: func() {}}
	updated, _ := m.Update(tea.PasteMsg{Content: "do not steer"})
	m = updated.(Model)
	updated, _ = m.Update(tea.KeyPressMsg{Code: 'x', Text: "x"})
	m = updated.(Model)
	if m.input.Value() != "" || m.View().Cursor != nil {
		t.Fatal("modal accepted hidden input")
	}
	updated, _ = m.Update(sideEventMsg{epoch: 1, done: true, err: errors.New("provider failed")})
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
			m.sideEpoch = 1
			m.side = &sideChat{cancel: func() {}}
			for _, chunk := range tc.chunks {
				updated, _ := m.Update(sideEventMsg{epoch: 1, event: agent.Event{Kind: agent.EventText, Text: chunk}})
				m = updated.(Model)
			}
			updated, _ := m.Update(sideEventMsg{epoch: 1, done: true, err: tc.err})
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
			m.closeSideChat()
			if strings.Contains(plain(m.View().Content), "Nothing was executed.") {
				t.Fatal("side notice remained after dismissal")
			}
		})
	}
}
