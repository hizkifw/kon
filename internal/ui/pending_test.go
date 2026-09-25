package ui

import (
	"context"
	"errors"
	"strings"
	"testing"
)

// busyModel is a test model with a run in flight.
func busyModel(t *testing.T) Model {
	t.Helper()
	m := newTestModel(t)
	m.busy = true
	m.runCancel = func() {}
	return m
}

func press(t *testing.T, m Model, key string) Model {
	t.Helper()
	updated, _, _ := m.handleKey(key)
	return updated.(Model)
}

func TestEnterWhileBusySteers(t *testing.T) {
	m := busyModel(t)
	m.input.SetValue("use v2")
	m = press(t, m, "enter")
	m.input.SetValue("and v3")
	m = press(t, m, "enter")
	if got := m.inbox.Pending(); len(got) != 2 || got[0] != "use v2" || got[1] != "and v3" {
		t.Fatalf("inbox = %q", got)
	}
	if m.input.Value() != "" {
		t.Fatalf("steer left input behind: %q", m.input.Value())
	}
	if view := plain(m.View().Content); !strings.Contains(view, "↳ steer  use v2") || !strings.Contains(view, "↳ steer  and v3") {
		t.Fatalf("pending strip missing from view:\n%s", view)
	}
}

func TestTabWhileBusyQueuesProseOnly(t *testing.T) {
	m := busyModel(t)
	m.input.SetValue("then run make check")
	m = press(t, m, "tab")
	if len(m.queued) != 1 || m.queued[0] != "then run make check" || m.input.Value() != "" {
		t.Fatalf("queued = %q, input = %q", m.queued, m.input.Value())
	}
	if view := plain(m.View().Content); !strings.Contains(view, "⏵ queue  then run make check") {
		t.Fatalf("queue missing from view:\n%s", view)
	}
	m.input.SetValue("/qu")
	m.refreshInput()
	m = press(t, m, "tab")
	if len(m.queued) != 1 || !strings.HasPrefix(m.input.Value(), "/queue") {
		t.Fatalf("tab on a slash command queued it: queued = %q, input = %q", m.queued, m.input.Value())
	}
}

func TestTabWhileIdleDoesNotQueue(t *testing.T) {
	m := newTestModel(t)
	m.input.SetValue("hello")
	m = press(t, m, "tab")
	if len(m.queued) != 0 || m.input.Value() != "hello" {
		t.Fatalf("idle tab queued: queued = %q, input = %q", m.queued, m.input.Value())
	}
}

func TestPlaceholderNamesKeysWhileBusy(t *testing.T) {
	m := busyModel(t)
	if view := plain(m.View().Content); !strings.Contains(view, "⇥ queue · ⏎ steer") {
		t.Fatalf("busy prompt has no key hints:\n%s", view)
	}
}

func TestCleanFinishSendsNextQueued(t *testing.T) {
	m := busyModel(t)
	m.queued = []string{"first", "second"}
	updated, _ := m.Update(runDoneMsg{})
	m = updated.(Model)
	if !m.busy || len(m.queued) != 1 || m.queued[0] != "second" {
		t.Fatalf("busy = %v, queued = %q", m.busy, m.queued)
	}
	if !strings.Contains(plain(m.viewport.View()), "first") {
		t.Fatal("the queued prompt was not added to the transcript")
	}
}

func TestInterruptSendsPendingSteerAndHoldsQueue(t *testing.T) {
	m := busyModel(t)
	m.inbox.Push("stop, use v2")
	m.syncSteering()
	m.queued = []string{"later"}
	updated, _ := m.Update(runDoneMsg{err: context.Canceled})
	m = updated.(Model)
	if !m.busy || len(m.steering) != 0 || len(m.queued) != 1 {
		t.Fatalf("busy = %v, steering = %q, queued = %q", m.busy, m.steering, m.queued)
	}
	if !strings.Contains(plain(m.viewport.View()), "stop, use v2") {
		t.Fatal("the steer did not start the next run")
	}

	// With no steer pending, an interrupt holds the queue until Enter on an
	// empty prompt.
	m.busy, m.runCancel = true, func() {}
	updated, _ = m.Update(runDoneMsg{err: context.Canceled})
	m = updated.(Model)
	if m.busy || len(m.queued) != 1 || !strings.Contains(m.status, "queue held") {
		t.Fatalf("busy = %v, queued = %q, status = %q", m.busy, m.queued, m.status)
	}
	if view := plain(m.View().Content); !strings.Contains(view, "⏎ send next queued") {
		t.Fatalf("held queue has no hint:\n%s", view)
	}
	m = press(t, m, "enter")
	if !m.busy || len(m.queued) != 0 {
		t.Fatalf("enter on an empty prompt did not resume the queue: busy = %v, queued = %q", m.busy, m.queued)
	}
}

func TestFailedRunQueuesUnreadSteer(t *testing.T) {
	m := busyModel(t)
	m.inbox.Push("use v2")
	m.syncSteering()
	m.queued = []string{"later"}
	updated, _ := m.Update(runDoneMsg{err: errors.New("boom")})
	m = updated.(Model)
	if m.busy || len(m.steering) != 0 || len(m.queued) != 2 || m.queued[0] != "use v2" {
		t.Fatalf("busy = %v, steering = %q, queued = %q", m.busy, m.steering, m.queued)
	}
}

func TestQueueCommandPullsBackAndClears(t *testing.T) {
	m := busyModel(t)
	m.inbox.Push("steer one")
	m.syncSteering()
	m.queued = []string{"queued one", "queued two"}

	items := completePending(m, "")
	if len(items) != 4 || items[0].Label != "1 steer" || items[1].Label != "2 queue" || items[3].Value != "clear" {
		t.Fatalf("picker = %#v", items)
	}

	updated, _ := m.managePending([]string{"1"})
	m = updated.(Model)
	if m.input.Value() != "steer one" || len(m.inbox.Pending()) != 0 || len(m.steering) != 0 {
		t.Fatalf("steer not pulled back: input = %q, inbox = %q", m.input.Value(), m.inbox.Pending())
	}
	updated, _ = m.managePending([]string{"2"})
	m = updated.(Model)
	if m.input.Value() != "queued two" || len(m.queued) != 1 || m.queued[0] != "queued one" {
		t.Fatalf("queued not pulled back: input = %q, queued = %q", m.input.Value(), m.queued)
	}

	m.inbox.Push("again")
	m.syncSteering()
	updated, _ = m.managePending([]string{"clear"})
	m = updated.(Model)
	if len(m.queued) != 0 || len(m.inbox.Pending()) != 0 || m.pendingHeight() != 0 {
		t.Fatalf("clear left messages: queued = %q, inbox = %q", m.queued, m.inbox.Pending())
	}
}

func TestQueueCommandReportsSteerAlreadySent(t *testing.T) {
	m := busyModel(t)
	m.inbox.Push("too late")
	m.syncSteering()
	m.inbox.Take() // the runner delivered it before the command ran
	updated, _ := m.managePending([]string{"1"})
	if got := updated.(Model); got.status != "already sent to the agent" || got.input.Value() != "" {
		t.Fatalf("status = %q, input = %q", got.status, got.input.Value())
	}
}

func TestPendingStripKeepsTranscriptHeight(t *testing.T) {
	m := busyModel(t)
	for _, text := range []string{"a", "b", "c", "d", "e"} {
		m.queued = append(m.queued, text)
	}
	m.resize()
	view := m.View().Content
	if lines := strings.Count(view, "\n") + 1; lines != m.height {
		t.Fatalf("view is %d lines, want the window height %d", lines, m.height)
	}
	if !strings.Contains(plain(view), "+3 more · /queue to edit") {
		t.Fatalf("overflow row missing:\n%s", plain(view))
	}
}
