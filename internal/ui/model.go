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
	commands                *registry
	active                  app.Model
	cwd, configPath, status string
	contextTokens           int
	contextApprox           bool
	busy                    bool
	runCancel               context.CancelFunc
	runEvents               chan tea.Msg
	cancelRequested         bool
	flushPending            bool
	menu                    menu
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
		runtime: runtime, commands: defaultRegistry(),
		active: state.Active, cwd: cwd, configPath: configPath,
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
		updated, cmd, handled := m.handleKey(msg.String())
		if handled {
			return updated, cmd
		}
		// handleKey closed the popup and may have updated the model; keep
		// those changes while normal input handling continues below.
		m = updated.(Model)
	}
	var cmd tea.Cmd
	before := m.input.Value()
	m.input, cmd = m.input.Update(msg)
	commands = append(commands, cmd)
	m.viewport, cmd = m.viewport.Update(msg)
	commands = append(commands, cmd)
	if m.input.Value() != before {
		// Refresh the popup whenever the prompt changed, regardless of which
		// key or paste produced the change. The command source only yields
		// candidates for input beginning with "/", so ordinary text closes it
		// and "/" as the first rune opens the command list.
		m.refreshInput()
		return m, tea.Batch(commands...)
	}
	m.resize()
	return m, tea.Batch(commands...)
}

// refreshInput recomputes the popup for the current prompt and re-lays-out the
// frame. Every path that changes the input or the menu ends here, so the
// viewport height always matches the menu in the same frame instead of
// reflowing on a later update.
func (m *Model) refreshInput() {
	m.openMenu()
	m.resize()
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
		m.refreshInput()
		return m, nil, true
	case "enter":
		if m.menu.open() {
			return m.completeMenu()
		}
		updated, cmd := m.submit()
		return updated, cmd, true
	case "tab":
		return m.completeMenu()
	case "shift+tab":
		if m.menu.open() {
			m.menu.move(-1)
			return m, nil, true
		}
		return m, nil, false
	case "esc":
		if m.menu.open() {
			m.resetMenu()
			return m, nil, true
		}
		return m, nil, false
	case "up", "down":
		if m.menu.open() {
			if key == "up" {
				m.menu.move(-1)
			} else {
				m.menu.move(1)
			}
			return m, nil, true
		}
		if strings.Contains(m.input.Value(), "\n") {
			return m, nil, false
		}
		direction := 1
		if key == "up" {
			direction = -1
		}
		if value, ok := m.history.recall(m.input.Value(), direction); ok {
			m.input.SetValue(value)
			m.refreshInput()
			return m, nil, true
		}
	}
	m.menu.close()
	return m, nil, false
}

// resetMenu closes the popup and re-lays-out the frame so the reclaimed rows
// are handed back to the viewport in the same update.
func (m *Model) resetMenu() {
	m.menu.close()
	m.resize()
}

// openMenu refreshes the popup from the command registry. An empty candidate
// set closes it. Typing "/" at the start of the prompt opens it immediately so
// the available commands are discoverable.
func (m *Model) openMenu() {
	items := m.commands.Candidates(*m, m.input.Value())
	if len(items) == 0 {
		m.menu.close()
		return
	}
	// Preserve the highlighted row while it is still present so cycling does
	// not jump when the candidate set is stable.
	previous := m.menu.selected().Value
	m.menu.items = items
	m.menu.index = 0
	if previous != "" {
		for i, item := range items {
			if item.Value == previous {
				m.menu.index = i
				break
			}
		}
	}
}

// completeMenu fills the prompt with the highlighted popup entry and refreshes
// the menu. If the popup is closed it is opened first, so Tab after "/"
// completes the top candidate. Arrow keys move the selection; Tab and Enter
// only accept, they never cycle. The appended space ends the current segment:
// completing a command with no more arguments (e.g. "/new") leaves no matching
// candidate and closes the menu, while "/model" advances to its argument list.
func (m Model) completeMenu() (tea.Model, tea.Cmd, bool) {
	if !m.menu.open() {
		m.openMenu()
		if !m.menu.open() {
			return m, nil, false
		}
	}
	m.commitMenu()
	m.refreshInput()
	return m, nil, true
}

// commitMenu applies the highlighted item through the popup source.
// Callers are responsible for resizing once the menu state is final.
func (m *Model) commitMenu() {
	m.commands.Accept(m, m.menu.selected().Value)
}

// replaceToken swaps the trailing token of input for value. Command names
// (values beginning with "/") replace the whole input; argument values keep
// the already-typed prefix.
func replaceToken(input, value string) string {
	if strings.HasPrefix(value, "/") {
		return value
	}
	if at := strings.LastIndexAny(input, " \t"); at >= 0 {
		return input[:at+1] + value
	}
	return value
}

func (m Model) submit() (tea.Model, tea.Cmd) {
	text := strings.TrimSpace(m.input.Value())
	if text == "" {
		return m, nil
	}
	if strings.HasPrefix(text, "/") {
		command, err := m.commands.parse(text)
		if err != nil {
			m.status = err.Error()
			return m, nil
		}
		return command.run(m)
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
