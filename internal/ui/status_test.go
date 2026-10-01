package ui

import (
	"errors"
	"strings"
	"testing"

	tea "charm.land/bubbletea/v2"
	"github.com/charmbracelet/x/ansi"
	"github.com/hizkifw/kon/internal/tui"
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
	if m = done(m, m.flashed.epoch); m.statusText() != "needs configuration" {
		t.Fatalf("status line %q once the notice is done", m.statusText())
	}
}

func TestNoticeClearsOnlyItself(t *testing.T) {
	m := newTestModel(t)
	m.flash(toneInfo, "first")
	first := m.flashed.epoch
	m.flash(toneInfo, "second")
	if m = done(m, first); m.message != "second" {
		t.Fatalf("the first notice's time took down the second: %q", m.message)
	}
	if m = done(m, m.flashed.epoch); m.message != "" {
		t.Fatalf("message %q once both are done", m.message)
	}
}

func TestNoticeLeavesALaterMessage(t *testing.T) {
	m := newTestModel(t)
	m.flash(toneSuccess, "copied selection")
	m.message = "interrupted"
	if m = done(m, m.flashed.epoch); m.message != "interrupted" {
		t.Fatalf("the notice's time cleared a later message: %q", m.message)
	}
}

func TestCopyCommandNoticePasses(t *testing.T) {
	m := selectionModel(t)
	m.input.SetValue("/copy")
	updated, _ := m.submit()
	m = updated.(Model)
	if m.message == "" || m.message != m.flashed.text {
		t.Fatalf("/copy set %q without a notice", m.message)
	}
	if m = done(m, m.flashed.epoch); m.message != "" {
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

// A failed /compact once reported the provider's multi-line error body in the
// status line, which then spilled over the prompt.
func TestAMultiLineMessageKeepsTheStatusLineOneRow(t *testing.T) {
	m := newTestModel(t)
	fakeTurn(&m)
	m, _ = update(m, turnDone(m, errors.New("provider failed\n{\n  \"error\": \"overloaded\"\n}")))
	if text := m.statusText(); strings.Contains(text, "\n") || !strings.Contains(text, `provider failed { "error": "overloaded" }`) {
		t.Fatalf("status line = %q", text)
	}
	if lines := strings.Split(m.View().Content, "\n"); len(lines) != m.height {
		t.Fatalf("view is %d rows, want %d", len(lines), m.height)
	}
}

// A toned message colors only itself, including when the line is cut short,
// and a message truncation removed entirely leaves the line as it was.
func TestToneColorsOnlyTheMessage(t *testing.T) {
	m := newTestModel(t)
	m.width = 40
	m.say(toneDanger, "interrupted · press Esc again to kill the command")
	row := ""
	for _, line := range strings.Split(m.View().Content, "\n") {
		if strings.Contains(ansi.Strip(line), "interrupted") {
			row = line
		}
	}
	if !strings.Contains(ansi.Strip(row), "…") {
		t.Fatalf("status row was not cut short: %q", ansi.Strip(row))
	}
	fitted := tui.Fit("~/w · ctx ? · interrupted · press Esc", 20)
	at := len("~/w · ctx ? · ")
	if got := toneLine(fitted, at, toneDanger); ansi.Strip(got) != fitted || got == fitted {
		t.Fatalf("toned line %q", got)
	}
	if got := toneLine(fitted, len(fitted)+5, toneDanger); got != fitted {
		t.Fatalf("a message cut off entirely was still toned: %q", got)
	}
	m.message = "plain"
	if m.messageTone() != toneInfo {
		t.Fatal("a plain message inherited the last tone")
	}
}
