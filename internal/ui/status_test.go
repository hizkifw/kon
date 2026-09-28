package ui

import (
	"strings"
	"testing"

	tea "charm.land/bubbletea/v2"
)

// done delivers the end of the notice with the given epoch.
func done(m Model, epoch int) Model {
	m, _ = update(m, flashDoneMsg{epoch})
	return m
}

func unconfiguredModel(t *testing.T) Model {
	t.Helper()
	m := newTestModel(t)
	m.configured = false
	return m
}

// The bug the split fixed: a command's result used to take the place of
// "needs configuration" for good.
func TestAMessageCannotHideAMode(t *testing.T) {
	m := unconfiguredModel(t)
	m.input.SetValue("/model")
	updated, _ := m.submit()
	if text := updated.(Model).statusText(); !strings.HasPrefix(text, "needs configuration · ") {
		t.Fatalf("status line %q lost the mode", text)
	}
}

func TestCopyNoticePassesAndTheModeStays(t *testing.T) {
	m := unconfiguredModel(t)
	m, cmd := update(m, selectionTextMsg{"some text"})
	if cmd == nil || m.statusText() != "needs configuration · copied selection" {
		t.Fatalf("status line %q after a copy", m.statusText())
	}
	if m = done(m, m.flashEpoch); m.statusText() != "needs configuration" {
		t.Fatalf("status line %q once the notice is done", m.statusText())
	}
}

func TestNoticeClearsOnlyItself(t *testing.T) {
	m := newTestModel(t)
	m.flash("first")
	first := m.flashEpoch
	m.flash("second")
	if m = done(m, first); m.message != "second" {
		t.Fatalf("the first notice's time took down the second: %q", m.message)
	}
	if m = done(m, m.flashEpoch); m.message != "" {
		t.Fatalf("message %q once both are done", m.message)
	}
}

func TestNoticeLeavesALaterMessage(t *testing.T) {
	m := newTestModel(t)
	m.flash("copied selection")
	m.message = "interrupted"
	if m = done(m, m.flashEpoch); m.message != "interrupted" {
		t.Fatalf("the notice's time cleared a later message: %q", m.message)
	}
}

func TestCopyCommandNoticePasses(t *testing.T) {
	m := selectionModel(t)
	m.input.SetValue("/copy")
	updated, _ := m.submit()
	m = updated.(Model)
	if m.message == "" || m.message != m.flashed {
		t.Fatalf("/copy set %q without a notice", m.message)
	}
	if m = done(m, m.flashEpoch); m.message != "" {
		t.Fatalf("message %q once the notice is done", m.message)
	}
}

func TestSearchTakesTheStatusLineAndGivesItBack(t *testing.T) {
	m := unconfiguredModel(t)
	m.message = "new session"
	m, _ = update(m, tea.KeyPressMsg{Code: 'r', Mod: tea.ModCtrl})
	if text := m.statusText(); text != "(reverse-i-search)`'" {
		t.Fatalf("status line %q while searching", text)
	}
	m, _ = update(m, tea.KeyPressMsg{Code: tea.KeyEscape})
	if text := m.statusText(); text != "needs configuration · new session" {
		t.Fatalf("status line %q after the search", text)
	}
}

// A login's question is its mode, so a message about a step, such as a
// missing key, shows beside it rather than in its place.
func TestLoginAsksBesideItsMessages(t *testing.T) {
	m := newTestModel(t)
	m.message = "new session"
	started, _ := m.startLogin("azure")
	m = started.(Model)
	if text := m.statusText(); !strings.HasPrefix(text, "provider URL for azure") || strings.Contains(text, "new session") {
		t.Fatalf("status line %q when the login starts", text)
	}
	m.login.input.SetValue("https://example.openai.azure.com/openai/v1")
	updated, _ := m.updateLogin(tea.KeyPressMsg{Code: tea.KeyEnter})
	updated, _ = updated.(Model).updateLogin(tea.KeyPressMsg{Code: tea.KeyEnter})
	if text := updated.(Model).statusText(); text != "API key for azure · Esc cancels · API key is required" {
		t.Fatalf("status line %q", text)
	}
}
