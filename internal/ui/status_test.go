package ui

import "testing"

// done delivers the end of the notice with the given epoch.
func done(m Model, epoch int) Model {
	m, _ = update(m, flashDoneMsg{epoch})
	return m
}

func TestCopyNoticeGivesBackTheStatusItCovered(t *testing.T) {
	m := newTestModel(t)
	m.status = "needs configuration"
	m, cmd := update(m, selectionTextMsg{"some text"})
	if cmd == nil || m.status != "copied selection" {
		t.Fatalf("status %q after a copy", m.status)
	}
	if m = done(m, m.flashEpoch); m.status != "needs configuration" {
		t.Fatalf("status %q once the notice is done", m.status)
	}
}

func TestNoticeClearsOnlyItself(t *testing.T) {
	m := newTestModel(t)
	m.status = "ready"
	m.flash("first")
	first := m.flashEpoch
	m.flash("second")
	if m = done(m, first); m.status != "second" {
		t.Fatalf("the first notice's time took down the second: %q", m.status)
	}
	// The second covered what the first covered, not the first itself.
	if m = done(m, m.flashEpoch); m.status != "ready" {
		t.Fatalf("status %q once both are done", m.status)
	}
}

func TestNoticeLeavesALaterStatus(t *testing.T) {
	m := newTestModel(t)
	m.flash("copied selection")
	m.status = "interrupted"
	if m = done(m, m.flashEpoch); m.status != "interrupted" {
		t.Fatalf("the notice's time cleared a later status: %q", m.status)
	}
}

func TestCopyCommandNoticePasses(t *testing.T) {
	m := selectionModel(t)
	m.input.SetValue("/copy")
	updated, _ := m.submit()
	m = updated.(Model)
	if m.flashing == nil || m.status != m.flashing.text {
		t.Fatalf("/copy set %q without a notice", m.status)
	}
	if m = done(m, m.flashEpoch); m.status != "" {
		t.Fatalf("status %q once the notice is done", m.status)
	}
}
