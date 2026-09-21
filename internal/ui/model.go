// Package ui implements kon's full-screen Bubble Tea interface.
package ui

import (
	"context"
	"errors"
	"strings"
	"time"

	"charm.land/bubbles/v2/textarea"
	"charm.land/bubbles/v2/viewport"
	tea "charm.land/bubbletea/v2"
	"github.com/hizkifw/kon/internal/agent"
	"github.com/hizkifw/kon/internal/app"
	"github.com/hizkifw/kon/internal/history"
)

const streamFrameInterval = 50 * time.Millisecond

type Runtime interface {
	Models() []app.Model
	State() app.State
	Run(context.Context, string, func(agent.Event)) error
	SwitchModel(string) error
	NewSession() error
	KillShell() bool
}

type runDoneMsg struct{ err error }
type runEventMsg struct{ event agent.Event }
type flushTranscriptMsg struct{}

type Model struct {
	width, height           int
	viewport                viewport.Model
	input                   textarea.Model
	transcript              transcript
	history                 promptHistory
	runtime                 Runtime
	active                  app.Model
	cwd, configPath, status string
	contextTokens           int
	contextApprox           bool
	busy                    bool
	runCancel               context.CancelFunc
	runEvents               chan tea.Msg
	cancelRequested         bool
	flushPending            bool
}

func New(cwd, configPath string, runtime Runtime, historyStore *history.Store, entries []history.Entry) Model {
	input := textarea.New()
	input.Placeholder = "Ask kon…"
	input.Prompt = "> "
	input.ShowLineNumbers = false
	input.SetHeight(1)
	input.MaxHeight = 6
	input.CharLimit = 0
	input.Focus()
	vp := viewport.New()
	vp.SoftWrap = true
	vp.MouseWheelEnabled = true
	// Keyboard input belongs to the prompt. The viewport's pager keymap
	// scrolls on plain letters (j, k, d, u, b, f) and space, so letting it see
	// key presses made the transcript jump while typing. Scrolling happens
	// through the mouse wheel and the pgup/pgdown handling in handleKey.
	vp.KeyMap = viewport.KeyMap{}
	state := runtime.State()
	status := "ready"
	if !state.Ready() {
		status = state.Problem.Error() + " in " + configPath
	}
	return Model{
		viewport: vp, input: input, history: newPromptHistory(historyStore, entries),
		runtime: runtime, active: state.Active, cwd: cwd, configPath: configPath,
		transcript: transcript{cwd: cwd},
		status:     status, contextTokens: -1,
	}
}

func (m Model) Init() tea.Cmd { return m.input.Focus() }

func (m Model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	var commands []tea.Cmd
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		m.width, m.height = msg.Width, msg.Height
		m.resize()
		m.refreshTranscript(false)
		return m, nil
	case runEventMsg:
		isText := m.applyAgentEvent(msg.event)
		commands = append(commands, waitRunEvent(m.runEvents))
		if isText && !m.flushPending {
			m.flushPending = true
			commands = append(commands, tea.Tick(streamFrameInterval, func(time.Time) tea.Msg { return flushTranscriptMsg{} }))
		} else if !isText {
			m.refreshTranscript(true)
		}
		return m, tea.Batch(commands...)
	case flushTranscriptMsg:
		m.flushPending = false
		m.refreshTranscript(true)
		return m, nil
	case runDoneMsg:
		m.busy, m.runCancel, m.runEvents, m.cancelRequested = false, nil, nil, false
		if msg.err == nil {
			m.status = "ready"
		} else if errors.Is(msg.err, context.Canceled) {
			m.status = "cancelled"
		} else {
			m.status = "error: " + msg.err.Error()
			m.transcript.add(block{kind: blockError, text: msg.err.Error()})
		}
		m.refreshTranscript(true)
		return m, nil
	case tea.KeyPressMsg:
		if updated, cmd, handled := m.handleKey(msg.String()); handled {
			return updated, cmd
		}
	}
	var cmd tea.Cmd
	m.input, cmd = m.input.Update(msg)
	commands = append(commands, cmd)
	m.viewport, cmd = m.viewport.Update(msg)
	commands = append(commands, cmd)
	m.resize()
	return m, tea.Batch(commands...)
}

func (m Model) handleKey(key string) (tea.Model, tea.Cmd, bool) {
	switch key {
	case "ctrl+c":
		if m.busy && m.runCancel != nil {
			// The first press cancels the run, which interrupts a running
			// shell command so it can stop cleanly. The second press kills
			// the command outright in case it ignored the interrupt.
			if !m.cancelRequested {
				m.cancelRequested = true
				m.runCancel()
				m.status = "cancelling… press Ctrl+C again to kill the command"
				return m, nil, true
			}
			if m.runtime.KillShell() {
				m.status = "killed the command"
			} else {
				m.status = "no command to kill; waiting for the run to cancel"
			}
			return m, nil, true
		}
		return m, tea.Quit, true
	case "ctrl+d":
		if strings.TrimSpace(m.input.Value()) == "" {
			return m, tea.Quit, true
		}
		return m, nil, false
	case "pgup":
		m.viewport.PageUp()
		return m, nil, true
	case "pgdown":
		m.viewport.PageDown()
		return m, nil, true
	case "alt+enter":
		m.input.InsertString("\n")
		m.resize()
		return m, nil, true
	case "enter":
		updated, cmd := m.submit()
		return updated, cmd, true
	case "up", "down":
		if strings.Contains(m.input.Value(), "\n") {
			return m, nil, false
		}
		direction := 1
		if key == "up" {
			direction = -1
		}
		if value, ok := m.history.recall(m.input.Value(), direction); ok {
			m.input.SetValue(value)
			return m, nil, true
		}
	}
	return m, nil, false
}

func (m Model) submit() (tea.Model, tea.Cmd) {
	text := strings.TrimSpace(m.input.Value())
	if text == "" {
		return m, nil
	}
	if strings.HasPrefix(text, "/") {
		command, err := parseSlashCommand(text)
		if err != nil {
			m.status = err.Error()
			return m, nil
		}
		return m.executeCommand(command)
	}
	if m.busy {
		m.status = "agent is busy; Ctrl+C cancels"
		return m, nil
	}
	state := m.runtime.State()
	if !state.Ready() {
		m.status = state.Problem.Error() + " in " + m.configPath
		return m, nil
	}
	if err := m.history.append(m.cwd, text); err != nil {
		m.status = "error: " + err.Error()
		return m, nil
	}
	m.transcript.add(block{kind: blockUser, text: sanitize(text)})
	m.input.Reset()
	m.resize()
	m.refreshTranscript(true)
	// Submitting is the user's own action: always show the new prompt, even
	// if they were scrolled up reading the transcript.
	m.viewport.GotoBottom()
	m.busy, m.status, m.cancelRequested = true, "thinking…", false
	ctx, cancel := context.WithCancel(context.Background())
	m.runCancel = cancel
	m.runEvents = make(chan tea.Msg)
	events := m.runEvents
	go func() {
		err := m.runtime.Run(ctx, text, func(event agent.Event) { events <- runEventMsg{event: event} })
		events <- runDoneMsg{err: err}
		close(events)
	}()
	return m, waitRunEvent(events)
}

func waitRunEvent(events <-chan tea.Msg) tea.Cmd {
	return func() tea.Msg {
		msg, ok := <-events
		if !ok {
			return runDoneMsg{}
		}
		return msg
	}
}

func (m *Model) syncRuntimeState() { m.active = m.runtime.State().Active }
