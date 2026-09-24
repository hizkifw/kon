// Package ui implements kon's full-screen Bubble Tea interface.
package ui

import (
	"context"
	"encoding/json"
	"errors"
	"strings"
	"time"

	"charm.land/bubbles/v2/textarea"
	tea "charm.land/bubbletea/v2"
	"github.com/hizkifw/kon/internal/agent"
	"github.com/hizkifw/kon/internal/app"
	"github.com/hizkifw/kon/internal/config"
	"github.com/hizkifw/kon/internal/history"
	"github.com/hizkifw/kon/internal/session"
	"github.com/hizkifw/kon/internal/tokens"
	"github.com/hizkifw/kon/internal/tools"
	"github.com/hizkifw/kon/internal/typedid"
)

const streamFrameInterval = 50 * time.Millisecond

type Runtime interface {
	Models() []app.Model
	State() app.State
	Run(context.Context, string, func(agent.Event)) error
	Compact(context.Context, func(agent.Event)) error
	SwitchModel(string) error
	// CycleEffort advances the active model's reasoning effort and returns the
	// new level, or "" for the provider default.
	CycleEffort() (string, error)
	Login(context.Context, config.Provider) (int, bool, error)
	LoginProviders() []string
	LoginConnection(string) (config.Provider, bool)
	LoadCatalog()
	NewSession() error
	Resume(typedid.SessionID) error
	Sessions() ([]session.Summary, error)
	SessionID() typedid.SessionID
	SessionHistory() []session.Entry
	// SessionPreview returns the last maxTurns user turns of the session at
	// path for a read-only preview without opening it for append.
	SessionPreview(path string, maxTurns int) ([]session.Entry, error)
	// DescribeTool resolves a persisted tool call's transcript display through
	// the tool that owns it, so replay matches live rendering.
	DescribeTool(name string, args json.RawMessage, result string, failed bool, details json.RawMessage) tools.Display
	// ContextUsage reports the last provider-reported context size and whether it
	// is known, so a resumed session can show it instead of an unknown value.
	ContextUsage() (tokens.Count, bool)
	// Interrupt escalates cancellation of the running tool call. attempt is
	// the number of consecutive Esc presses; see agent.Runner.Interrupt.
	Interrupt(attempt int) bool
}

type runDoneMsg struct{ err error }
type runEventMsg struct{ event agent.Event }
type flushTranscriptMsg struct{}

type Model struct {
	width, height           int
	viewport                scrollView
	input                   textarea.Model
	transcript              transcript
	history                 promptHistory
	runtime                 Runtime
	commands                *registry
	active                  app.Model
	cwd, configPath, status string
	contextTokens           tokens.Count
	contextApprox           bool
	terminalFocused         bool
	busy                    bool
	runCancel               context.CancelFunc
	runEvents               chan tea.Msg
	// interruptPresses counts consecutive Esc presses while a run is in
	// flight, so the harness can escalate: the first press cancels the run
	// (interrupting a running command), the second kills it.
	interruptPresses int
	flushPending     bool
	// killRing holds the last line segment removed by a kill key (Ctrl+U,
	// Ctrl+K, Ctrl+W) so Ctrl+Y can yank it back, mirroring the shell's kill
	// and yank commands.
	killRing string
	// killPending marks that the key being processed is a kill command, so the
	// removed text is captured once the textarea has applied the deletion.
	killPending bool
	// search is the active reverse history search (Ctrl+R), nil when idle.
	search *reverseSearch
	// startAtBottom asks the first sized frame to scroll to the end, so a
	// resumed conversation opens on its latest messages instead of at the top.
	startAtBottom bool
	menu          menu
	login         *loginFlow
	// preview is a scratch transcript shown in place of the live one while a
	// popup row that carries a Preview is highlighted, so a picker can be
	// browsed without committing. previewKey is that row's value; previewReturn
	// remembers where the live transcript was scrolled so cancelling restores
	// the reader's place.
	preview       *transcript
	previewKey    string
	previewReturn previewReturn
}

// previewReturn snapshots the live transcript's scroll position so closing a
// preview puts the reader back where they left off.
type previewReturn struct {
	offset   int
	atBottom bool
}

func New(cwd, configPath string, runtime Runtime, historyStore *history.Store, entries []history.Entry) Model {
	input := textarea.New()
	input.Placeholder = "Ask kon…"
	input.Prompt = ""
	input.ShowLineNumbers = false
	input.SetHeight(1)
	// The input grows with its content — soft-wrapped rows included — so a
	// long prompt stays visible instead of scrolling out of a one-row box.
	// MaxHeight bounds it; resize() re-bounds it against the window height.
	input.DynamicHeight = true
	input.MinHeight = 1
	input.MaxHeight = maxInputLines
	input.CharLimit = 0
	// A real terminal cursor is hidden by tmux when another pane is active.
	// The default virtual cursor is only styled text, so tmux cannot distinguish
	// it from the rest of the prompt.
	input.SetVirtualCursor(false)
	input.Focus()
	// The transcript renders every line pre-wrapped to the viewport width (see
	// transcript.linesFor), so the scroll view needs no soft wrap or per-line
	// width measurement. Keyboard scrolling is handled in handleKey; the view
	// only consumes the mouse wheel.
	vp := newScrollView()
	state := runtime.State()
	// An unconfigured launch explains itself in the transcript below, so the
	// status stays a short pointer rather than repeating the whole problem.
	status := "ready"
	if !state.Ready() {
		status = "needs configuration"
	}
	model := Model{
		viewport: vp, input: input, history: newPromptHistory(historyStore, entries),
		runtime: runtime, commands: defaultRegistry(),
		active: state.Active, cwd: cwd, configPath: configPath,
		transcript: transcript{cwd: cwd, banner: welcomeBanner},
		status:     status, contextTokens: -1, terminalFocused: true,
	}
	// A resumed session opens with its conversation already in the transcript.
	// The viewport has no size until the first resize, so defer the scroll to
	// the bottom to that first sized frame.
	if history := runtime.SessionHistory(); len(history) > 0 {
		model.applyHistory(history)
		model.startAtBottom = true
		model.seedContextUsage()
	}
	// An unconfigured launch introduces itself as an assistant turn so a first
	// run reads as a conversation instead of a bare error state.
	if !state.Ready() {
		model.transcript.add(block{kind: blockAssistant, text: welcomeMessage(configPath)})
	}
	return model
}

// catalogLoadedMsg reports that the runtime has applied catalog metadata, so
// the active model's display name and context window can be refreshed.
type catalogLoadedMsg struct{}

// Init loads the catalog in the background so the first frame never waits
// for it.
func (m Model) Init() tea.Cmd {
	runtime := m.runtime
	return tea.Batch(m.input.Focus(), func() tea.Msg {
		runtime.LoadCatalog()
		return catalogLoadedMsg{}
	})
}

func (m Model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	var commands []tea.Cmd
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		m.width, m.height = msg.Width, msg.Height
		m.resize()
		m.refreshTranscript(false)
		m.anchorStartAtBottom()
		return m, nil
	case tea.FocusMsg:
		m.terminalFocused = true
	case tea.BlurMsg:
		m.terminalFocused = false
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
		m.busy, m.runCancel, m.runEvents, m.interruptPresses = false, nil, nil, 0
		m.syncRuntimeState()
		// An interrupted stream never received its done event, so finalize the
		// live stream here to freeze the partial answer and reasoning that were
		// already displayed. A cleanly finished run has nothing pending.
		m.transcript.finishStream()
		switch {
		case msg.err == nil:
			m.status = "ready"
		case errors.Is(msg.err, context.Canceled):
			m.status = "interrupted"
		case errors.Is(msg.err, agent.ErrNothingToCompact):
			m.status = "nothing to compact"
		default:
			m.status = "error: " + msg.err.Error()
			m.transcript.add(block{kind: blockError, text: msg.err.Error()})
		}
		m.refreshTranscript(true)
		return m, nil
	case loginDoneMsg:
		return m.finishLogin(msg)
	case catalogLoadedMsg:
		m.syncRuntimeState()
		return m, nil
	case tea.KeyPressMsg:
		if m.login != nil {
			return m.updateLogin(msg)
		}
		if m.search != nil {
			// The search owns every key while active so plain typing extends
			// the query instead of editing the prompt.
			return m.updateSearch(msg)
		}
		updated, cmd, handled := m.handleKey(msg.String())
		if handled {
			return updated, cmd
		}
		// handleKey closed the popup and may have updated the model; keep
		// those changes while normal input handling continues below.
		m = updated.(Model)
	}
	if m.login != nil {
		return m.updateLogin(msg)
	}
	var cmd tea.Cmd
	before := m.input.Value()
	pending := m.killPending
	m.killPending = false
	cursor := 0
	if pending {
		cursor = m.cursorOffset()
	}
	m.input, cmd = m.input.Update(msg)
	commands = append(commands, cmd)
	m.viewport.Update(msg)
	if pending {
		// A kill key ran; record what it removed so Ctrl+Y can yank it. An
		// empty kill leaves the ring untouched, so yank keeps the prior text.
		if removed := removedSpan(before, m.input.Value(), cursor); removed != "" {
			m.killRing = removed
		}
	}
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

// removedSpan returns the text a kill deleted between before and after, or ""
// when the edit was not a kill at cursor, the byte offset of the cursor in
// before. Every kill removes one run that ends at the cursor (Ctrl+U, Ctrl+W)
// or starts at it (Ctrl+K), so anchoring on the cursor recovers the exact span.
// A plain prefix/suffix diff cannot: killing "ab" from "aba" would match the
// leading "a" and report "ba", and could split a multibyte rune.
func removedSpan(before, after string, cursor int) string {
	n := len(before) - len(after)
	if n <= 0 || cursor < 0 || cursor > len(before) {
		return ""
	}
	if start := cursor - n; start >= 0 && before[:start]+before[cursor:] == after {
		return before[start:cursor]
	}
	if end := cursor + n; end <= len(before) && before[:cursor]+before[end:] == after {
		return before[cursor:end]
	}
	return ""
}

// cursorOffset returns the cursor's byte offset into the input value. The
// textarea reports a logical row and a rune column, so the line is sliced by
// rune to land on a character boundary.
func (m Model) cursorOffset() int {
	lines := strings.Split(m.input.Value(), "\n")
	row := min(max(m.input.Line(), 0), len(lines)-1)
	offset := 0
	for _, line := range lines[:row] {
		offset += len(line) + 1
	}
	line := []rune(lines[row])
	col := min(max(m.input.Column(), 0), len(line))
	return offset + len(string(line[:col]))
}

func (m Model) handleKey(key string) (tea.Model, tea.Cmd, bool) {
	switch key {
	case "ctrl+c":
		if m.input.Value() != "" {
			m.input.Reset()
			m.refreshInput()
			return m, nil, true
		}
		m.status = "press Ctrl+D to exit"
		return m, nil, true
	case "ctrl+d":
		if m.input.Value() != "" {
			// Ctrl+D only exits on an empty input box; with text present it
			// does nothing so it cannot silently drop an in-progress prompt.
			return m, nil, true
		}
		if m.runCancel != nil {
			m.runCancel()
		}
		return m, tea.Quit, true
	case "ctrl+u", "ctrl+k", "ctrl+w":
		// Kill commands: Ctrl+U to line start, Ctrl+K to line end, Ctrl+W by
		// word, all mirroring the shell. The textarea performs the deletion, so
		// mark the key and let Update record exactly what it removed.
		m.killPending = true
		return m, nil, false
	case "ctrl+y":
		m.input.InsertString(m.killRing)
		m.refreshInput()
		return m, nil, true
	case "ctrl+r":
		updated, cmd := m.startSearch()
		return updated, cmd, true
	case "pgup":
		m.viewport.PageUp()
		return m, nil, true
	case "pgdown":
		m.viewport.PageDown()
		return m, nil, true
	case "shift+enter", "ctrl+enter":
		m.input.InsertString("\n")
		m.refreshInput()
		return m, nil, true
	case "enter":
		if m.menu.open() {
			return m.completeMenu()
		}
		if strings.HasSuffix(m.input.Value(), `\`) {
			// A trailing backslash escapes the Enter, like a shell line
			// continuation: drop the backslash and open a new line instead of
			// submitting.
			m.input.SetValue(strings.TrimSuffix(m.input.Value(), `\`) + "\n")
			m.refreshInput()
			return m, nil, true
		}
		updated, cmd := m.submit()
		return updated, cmd, true
	case "tab":
		return m.completeMenu()
	case "shift+tab":
		if m.menu.open() {
			m.menu.move(-1)
			m.syncPreview()
			return m, nil, true
		}
		updated, cmd := m.cycleEffort()
		return updated, cmd, true
	case "esc":
		if m.menu.open() {
			m.resetMenu()
			return m, nil, true
		}
		if m.busy && m.runCancel != nil {
			// Esc is the only interrupt. The first press cancels the run,
			// which interrupts a running tool so it can stop cleanly; a
			// second press escalates to a kill for a tool that ignored the
			// interrupt.
			m.interruptPresses++
			m.runCancel()
			if m.interruptPresses == 1 {
				m.status = "interrupting… press Esc again to kill the command"
				return m, nil, true
			}
			if m.runtime.Interrupt(m.interruptPresses) {
				m.status = "killed the command"
			} else {
				m.status = "no command to kill; waiting for the run to cancel"
			}
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
			m.syncPreview()
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
	m.closePreview()
	return m, nil, false
}

// resetMenu closes the popup and re-lays-out the frame so the reclaimed rows
// are handed back to the viewport in the same update. Any preview is dropped
// and the live transcript restored.
func (m *Model) resetMenu() {
	m.menu.close()
	m.closePreview()
	m.resize()
}

// openMenu refreshes the popup from the command registry. An empty candidate
// set closes it. Typing "/" at the start of the prompt opens it immediately so
// the available commands are discoverable.
func (m *Model) openMenu() {
	items := m.commands.Candidates(*m, m.input.Value())
	if len(items) == 0 {
		m.menu.close()
		m.syncPreview()
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
	m.syncPreview()
}

// syncPreview makes the drawn transcript match the highlighted popup row: a
// row with a preview renders into a scratch transcript, anything else restores
// the live one. It captures the reader's live scroll position when previewing
// begins, so cancelling the popup returns them exactly where they were, and it
// opens a preview scrolled to the latest turn like a real resume would.
func (m *Model) syncPreview() {
	selected := m.menu.selected()
	if !m.menu.open() || selected.Preview == nil {
		m.closePreview()
		return
	}
	if m.preview != nil && m.previewKey == selected.Value {
		return
	}
	if m.preview == nil {
		m.previewReturn = previewReturn{offset: m.viewport.YOffset(), atBottom: m.viewport.AtBottom()}
	}
	m.previewKey = selected.Value
	m.preview = selected.Preview()
	if m.preview == nil {
		m.previewKey = ""
		m.restorePreviewScroll()
		return
	}
	m.resize()
	m.refreshTranscript(false)
	m.viewport.GotoBottom()
}

// closePreview drops any active preview and restores the live transcript at the
// scroll position the reader had before previewing began.
func (m *Model) closePreview() {
	if m.preview == nil {
		return
	}
	m.preview, m.previewKey = nil, ""
	m.restorePreviewScroll()
}

// restorePreviewScroll repaints the live transcript and returns the viewport to
// the position recorded when previewing started.
func (m *Model) restorePreviewScroll() {
	m.resize()
	m.refreshTranscript(false)
	if m.previewReturn.atBottom {
		m.viewport.GotoBottom()
		return
	}
	m.viewport.SetYOffset(m.previewReturn.offset)
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
	// Submitting commits: drop any highlighted preview so the live transcript
	// (or the resumed one) is what the command operates on and shows.
	m.closePreview()
	if strings.HasPrefix(text, "/") {
		command, err := m.commands.parse(text)
		if err != nil {
			m.status = err.Error()
			return m, nil
		}
		return command.run(m)
	}
	if m.busy {
		m.status = "agent is busy; Esc interrupts"
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
	return m.startRun("thinking…", func(ctx context.Context, emit func(agent.Event)) error {
		return m.runtime.Run(ctx, text, emit)
	})
}

// startRun marks the model busy and drives a runtime operation on a goroutine,
// forwarding agent events into the transcript. It is shared by prompt
// submission and manual compaction so both report progress and cancel the same
// way.
func (m Model) startRun(status string, fn func(context.Context, func(agent.Event)) error) (tea.Model, tea.Cmd) {
	m.busy, m.status, m.interruptPresses = true, status, 0
	ctx, cancel := context.WithCancel(context.Background())
	m.runCancel = cancel
	m.runEvents = make(chan tea.Msg)
	events := m.runEvents
	go runAndForward(ctx, events, fn)
	return m, waitRunEvent(events)
}

func runAndForward(ctx context.Context, events chan tea.Msg, fn func(context.Context, func(agent.Event)) error) {
	defer close(events)
	err := fn(ctx, func(event agent.Event) {
		select {
		case events <- runEventMsg{event: event}:
		case <-ctx.Done():
		}
	})
	select {
	case events <- runDoneMsg{err: err}:
	case <-ctx.Done():
	}
}

func waitRunEvent(events <-chan tea.Msg) tea.Cmd {
	return func() tea.Msg {
		msg, ok := <-events
		if !ok {
			return runDoneMsg{err: context.Canceled}
		}
		return msg
	}
}

// syncRuntimeState refreshes the active model.
func (m *Model) syncRuntimeState() { m.active = m.runtime.State().Active }

// seedContextUsage adopts the live session's last provider-reported context size
// so a resumed conversation shows it instead of the unknown placeholder. It
// leaves contextTokens at -1 when no reported usage applies.
func (m *Model) seedContextUsage() {
	if used, ok := m.runtime.ContextUsage(); ok {
		m.contextTokens = used
		m.contextApprox = false
		return
	}
	m.contextTokens = -1
}

// anchorStartAtBottom consumes the one-shot startup request to scroll to the
// end of a resumed transcript. It runs once the viewport has a height, so the
// offset is set after the lines have been wrapped to the real terminal width.
func (m *Model) anchorStartAtBottom() {
	if !m.startAtBottom || m.height <= 0 {
		return
	}
	m.startAtBottom = false
	m.viewport.GotoBottom()
}
