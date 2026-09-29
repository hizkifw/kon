// Package ui implements kon's full-screen Bubble Tea interface.
package ui

import (
	"context"
	"encoding/json"
	"errors"
	"strings"
	"time"
	"unicode/utf8"

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

// catalogDelay holds the catalog load until the first frame is on screen.
// Init runs before Bubble Tea writes that frame, on its first render tick,
// and nothing reports when it has. Decoding the catalog allocates about
// 30 MB, and the garbage collection that starts slowed the first frame by
// 15 ms at the 90th percentile when the two overlapped.
const catalogDelay = 100 * time.Millisecond

type Runtime interface {
	Models() []app.Model
	State() app.State
	Run(context.Context, string, *agent.Inbox, func(agent.Event)) error
	SideChat(context.Context, string, func(agent.Event)) error
	Compact(context.Context, func(agent.Event)) error
	SwitchModel(string) error
	// CycleEffort advances the active model's reasoning effort and returns the
	// new level, or "" for the provider default.
	CycleEffort() (string, error)
	Login(context.Context, config.Provider) (int, bool, error)
	LoginProviders() []string
	LoginEntry(string) (app.LoginEntry, bool)
	DescribeSelection(session.ModelSelection) app.Model
	LoadCatalog()
	NewSession() error
	Resume(typedid.SessionID) error
	Sessions() ([]session.Summary, error)
	SessionID() typedid.SessionID
	SessionHistory() []session.Entry
	// Incognito reports that sessions are kept in memory only, which the
	// banner shows.
	Incognito() bool
	// Follow reads what another kon has appended to a session this one
	// follows read-only; TakeOver makes this kon its writer once it is free.
	Follow() (app.Followed, error)
	TakeOver() ([]session.Entry, error)
	// SessionPreview returns the last maxTurns user turns of the session at
	// path for a read-only preview without opening it for append.
	SessionPreview(path string, maxTurns int) ([]session.Entry, error)
	// DescribeTool resolves a persisted tool call's transcript display through
	// the tool that owns it, so replay matches live rendering.
	DescribeTool(name string, args json.RawMessage, result string, failed bool, details json.RawMessage) tools.Display
	// Notices delivers kon's notices for the agent, such as a background job
	// exiting; RunningJobs counts the live session's running jobs.
	Notices() <-chan string
	RunningJobs() int
	// Jobs lists the session's background jobs, KillJob stops one, and
	// OpenSubagent opens a subagent job's own session to follow it.
	Jobs() []tools.Job
	KillJob(id int) error
	OpenSubagent(id typedid.SessionID) (*session.View, error)
	// ContextUsage reports the last provider-reported context size and whether it
	// is known, so a resumed session can show it instead of an unknown value.
	ContextUsage() (tokens.Count, bool)
	// SubagentUsage adds up what the session's subagents have used so far.
	// It reads files, so the UI calls it off its own goroutine.
	SubagentUsage() session.Usage
	// Interrupt escalates cancellation of the running tool call. attempt is
	// the number of consecutive Esc presses; see agent.Runner.Interrupt.
	Interrupt(attempt int) bool
}

type flushTranscriptMsg struct{}

type Model struct {
	// ctx lives as long as the program. Runs and logins derive from it so that
	// every exit path, including a signal that bypasses Update, releases them.
	ctx             context.Context
	width, height   int
	viewport        scrollView
	input           textarea.Model
	transcript      transcript
	history         promptHistory
	runtime         Runtime
	commands        *registry
	active          app.Model
	cwd, configPath string
	// message is the last thing that happened, as the status line tells it:
	// a command's result, an error, a passing notice. The mode kon is in,
	// which lasts as long as the mode does, is apart from it (see mode).
	message string
	// tone colors the message; toned is the text it was set for (see
	// messageTone).
	tone  tone
	toned string
	// configured is whether kon has a model to send to, or follows a session
	// that has one, as of the last sync with the runtime.
	configured      bool
	contextTokens   tokens.Count
	contextApprox   bool
	terminalFocused bool
	// turn is the user turn or /compact in flight, nil while idle.
	turn *run
	// runEpoch counts started runs, main and side alike; see run.epoch.
	runEpoch int
	// drawers is the stack of surfaces painted over the screen, top last.
	drawers   []*drawer
	side      *sideChat
	sideSpent float64
	// jobsView is the /jobs drawers while they are open, and jobsEpoch
	// counts their reads' chains, so one from drawers since closed stops.
	jobsView  *jobsView
	jobsEpoch int
	// interruptPresses counts Esc presses that interrupted the run in
	// flight, so the harness can escalate: the first cancels the run
	// (interrupting a running command), the second kills it.
	interruptPresses int
	// interruptEpoch is the notice epoch of the warning a first Esc showed
	// before interrupting (see interruptArmed); 0 when none was shown.
	interruptEpoch int
	flushPending   bool
	// inbox carries steering (Enter while a run is in flight) to the runner,
	// which drains it before its next request. steering mirrors what it held
	// when last synced, so the pending strip and the layout agree within a
	// frame even while the runner drains it concurrently.
	inbox    *agent.Inbox
	steering []string
	// queued holds prompts (Tab while a run is in flight) that each start
	// their own run once the one before finishes cleanly.
	queued []string
	// jobs is the number of background jobs running, as of the last event
	// that could have changed it.
	jobs int
	// spent is what the live session's own responses have cost, in US
	// dollars, and subagentSpent what its subagents have, as of the last read
	// of their sessions. spendPolling marks a poll in flight, and spendEpoch
	// counts sessions opened so a read for an earlier one is dropped.
	spent         float64
	subagentSpent float64
	// streamed counts the stream chunks received since the last usage report,
	// each taken as one token, so the status bar moves while a response
	// streams. A chunk usually holds more than one token, so the estimate
	// undercounts until the report replaces it. streamedContext counts only
	// the chunks that extend the context: a compaction summary replaces the
	// context rather than adding to it.
	streamed        tokens.Count
	streamedContext tokens.Count
	spendPolling    bool
	spendEpoch      int
	// selectEpoch counts drags past the transcript's edge, so a scroll tick
	// from one that ended stops instead of scrolling on.
	selectEpoch int
	// click is the last press on the transcript, to tell double and triple
	// clicks apart and to start a drag from.
	click click
	// flashed is the last passing notice shown as the message. flashEpoch
	// counts notices, so each clears only itself.
	flashed    string
	flashEpoch int
	// killRing holds the last line segment removed by a kill key (Ctrl+U,
	// Ctrl+K, Ctrl+W) so Ctrl+Y can yank it back, mirroring the shell's kill
	// and yank commands.
	killRing string
	// killPending marks that the key being processed is a kill command, so the
	// removed text is captured once the textarea has applied the deletion.
	killPending bool
	// search is the active reverse history search (Ctrl+R), nil when idle.
	// lastSearch is the query it last ended with, which Ctrl+R on an empty
	// query searches for again.
	search     *reverseSearch
	lastSearch string
	// startAtBottom asks the first sized frame to scroll to the end, so a
	// resumed conversation opens on its latest messages instead of at the top.
	startAtBottom bool
	menu          menu
	menuSource    menuSource
	mentions      fileMentions
	login         *loginFlow
	// preview is a scratch transcript shown in place of the live one while a
	// popup row that carries a Preview is highlighted, so a picker can be
	// browsed without committing. previewKey is that row's value; previewReturn
	// remembers where the live transcript was scrolled so cancelling restores
	// the reader's place.
	preview       *transcript
	previewKey    string
	previewReturn previewReturn
	// follow carries replay across batches while the session is open in
	// another kon and shown read-only, nil otherwise. followEpoch drops ticks
	// from a follow that has ended, and followMode says what the follow is
	// doing, for the status line.
	follow      *replayState
	followEpoch int
	followMode  string
}

// previewReturn snapshots the live transcript's scroll position so closing a
// preview puts the reader back where they left off.
type previewReturn struct {
	offset   int
	atBottom bool
}

func New(ctx context.Context, cwd, configPath string, runtime Runtime, historyStore *history.Store, entries []history.Entry) Model {
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
	mark := welcomeBanner
	if runtime.Incognito() {
		mark = incognitoBanner
	}
	model := Model{
		ctx: ctx, viewport: vp, input: input, history: newPromptHistory(historyStore, entries),
		runtime: runtime, commands: defaultRegistry(), inbox: &agent.Inbox{},
		active: state.Active, cwd: cwd, configPath: configPath,
		transcript: transcript{cwd: cwd, banner: mark},
		configured: state.Ready() || state.Following(), contextTokens: -1, terminalFocused: true,
	}
	// A resumed session opens with its conversation already in the transcript.
	// The viewport has no size until the first resize, so defer the scroll to
	// the bottom to that first sized frame.
	if len(runtime.SessionHistory()) > 0 {
		// A followed session's first read is scheduled by Init.
		_ = model.loadSession()
		model.startAtBottom = true
		model.seedContextUsage()
	}
	// An unconfigured launch introduces itself as an assistant turn so a first
	// run reads as a conversation instead of a bare error state.
	if !state.Ready() && !state.Following() {
		model.transcript.add(block{kind: blockAssistant, text: welcomeMessage(configPath)})
	}
	return model
}

// catalogLoadedMsg reports that the runtime has applied catalog metadata, so
// the active model's display name and context window can be refreshed.
type catalogLoadedMsg struct{}

// Init loads the catalog in the background once the first frame is out, so
// the frame never waits for it or shares the CPU with it.
func (m Model) Init() tea.Cmd {
	runtime := m.runtime
	// A resumed session's subagents are read along with the catalog, once
	// the first frame is out.
	commands := []tea.Cmd{m.input.Focus(), tea.Tick(catalogDelay, func(time.Time) tea.Msg {
		runtime.LoadCatalog()
		return catalogLoadedMsg{}
	}), tea.Tick(catalogDelay, m.readSpend(false)), waitNotice(runtime.Notices())}
	if m.follow != nil {
		commands = append(commands, followTick(m.followEpoch))
	}
	return tea.Batch(commands...)
}

func (m Model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	var commands []tea.Cmd
	switch msg := msg.(type) {
	case mentionFilesMsg:
		return m.applyMentionFiles(msg)
	case runMsg:
		return m.updateRun(msg)
	case runTickMsg:
		return m.tickRun(msg)
	case jobsTickMsg:
		return m.tickJobs(msg)
	case jobsPolledMsg:
		return m.applyJobsPolled(msg)
	case tea.WindowSizeMsg:
		// Anchor the bottom edge across the resize, so a reader at the bottom
		// keeps the last line in view. A width change rewraps the transcript,
		// which makes the anchor approximate away from the bottom.
		below := m.viewport.LinesBelow()
		m.width, m.height = msg.Width, msg.Height
		m.resize()
		m.refreshTranscript(false)
		m.viewport.SetLinesBelow(below)
		m.anchorStartAtBottom()
		return m, nil
	case tea.MouseClickMsg:
		return m.pressMouse(msg)
	case tea.MouseMotionMsg:
		return m.dragMouse(msg)
	case tea.MouseReleaseMsg:
		return m.releaseMouse(msg)
	case tea.MouseWheelMsg:
		return m.wheelMouse(msg)
	case selectScrollMsg:
		return m.scrollSelection(msg)
	case selectionTextMsg:
		return m.copied(msg)
	case flashDoneMsg:
		return m.flashDone(msg)
	case tea.PasteMsg:
		if len(m.drawers) > 0 {
			return m, nil
		}
		if m.search != nil {
			return m.pasteSearch(msg), nil
		}
	case tea.FocusMsg:
		m.terminalFocused = true
	case tea.BlurMsg:
		m.terminalFocused = false
	case flushTranscriptMsg:
		m.flushPending = false
		m.refreshTranscript(true)
		return m, nil
	case followTickMsg:
		if m.follow == nil || msg.epoch != m.followEpoch {
			return m, nil
		}
		return m, m.pollFollowed(msg.epoch)
	case followedMsg:
		return m, m.applyFollowed(msg)
	case spendMsg:
		return m, m.applySpend(msg)
	case noticeMsg:
		updated, cmd := m.deliverNotice(msg.text)
		return updated, tea.Batch(cmd, waitNotice(m.runtime.Notices()))
	case loginDoneMsg:
		return m.finishLogin(msg)
	case catalogLoadedMsg:
		m.syncRuntimeState()
		m.retitleModelChanges()
		return m, nil
	case tea.KeyPressMsg:
		if len(m.drawers) > 0 {
			return m.drawerKey(msg.String())
		}
		if m.login != nil {
			return m.updateLogin(msg)
		}
		if m.search != nil {
			// The search takes the keys it knows. Any other has ended it,
			// keeping the match, and acts on the prompt as usual below.
			var took bool
			if m, took = m.searchKey(msg); took {
				return m, nil
			}
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
	cursor := m.cursorOffset()
	pending := m.killPending
	m.killPending = false
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
	if m.input.Value() != before || m.cursorOffset() != cursor {
		// Mentions follow the cursor, including edits in the middle of a prompt.
		commands = append(commands, m.refreshInput())
		return m, tea.Batch(commands...)
	}
	m.resize()
	return m, tea.Batch(commands...)
}

// refreshInput recomputes the popup for the current prompt and re-lays-out the
// frame. Every path that changes the input or the menu ends here, so the
// viewport height always matches the menu in the same frame instead of
// reflowing on a later update.
func (m *Model) refreshInput() tea.Cmd {
	cmd := m.loadMentionFiles()
	m.openMenu()
	m.resize()
	return cmd
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

func (m *Model) setCursorOffset(offset int) {
	text := m.input.Value()
	before := text[:min(max(offset, 0), len(text))]
	row := strings.Count(before, "\n")
	m.input.MoveToBegin()
	for range len(text) {
		if m.input.Line() >= row {
			break
		}
		m.input.CursorDown()
	}
	m.input.SetCursorColumn(utf8.RuneCountInString(before[strings.LastIndex(before, "\n")+1:]))
}

func (m Model) handleKey(key string) (tea.Model, tea.Cmd, bool) {
	switch key {
	case "ctrl+c":
		if m.input.Value() != "" {
			m.input.Reset()
			cmd := m.refreshInput()
			return m, cmd, true
		}
		m.message = "press Ctrl+D to exit"
		return m, nil, true
	case "ctrl+d":
		if m.input.Value() != "" {
			// Ctrl+D only exits on an empty input box; with text present it
			// does nothing so it cannot silently drop an in-progress prompt.
			return m, nil, true
		}
		if m.turn != nil {
			m.turn.cancel()
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
		cmd := m.refreshInput()
		return m, cmd, true
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
		cmd := m.refreshInput()
		return m, cmd, true
	case "enter":
		if m.menu.open() {
			return m.completeMenu()
		}
		if strings.HasSuffix(m.input.Value(), `\`) {
			// A trailing backslash escapes the Enter, like a shell line
			// continuation: drop the backslash and open a new line instead of
			// submitting.
			m.input.SetValue(strings.TrimSuffix(m.input.Value(), `\`) + "\n")
			cmd := m.refreshInput()
			return m, cmd, true
		}
		updated, cmd := m.submit()
		return updated, cmd, true
	case "tab":
		if _, _, _, active := mentionAt(m.input.Value(), m.cursorOffset()); active {
			updated, cmd, handled := m.completeMenu()
			m = updated.(Model)
			if handled || !m.canQueue() {
				return m, cmd, handled
			}
		}
		if !m.menu.open() && m.canQueue() {
			updated, cmd := m.enqueue()
			return updated, cmd, true
		}
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
		if m.menu.open() || (m.menu.note != "" && m.mentions.loading) {
			m.mentions.dismissed = true
			m.resetMenu()
			return m, nil, true
		}
		m.resetMenu()
		if m.busy() {
			// Esc is the only interrupt, and a stray press must not throw a
			// turn away: the first press only warns, and a second while the
			// warning is up cancels the run, which interrupts a running tool
			// so it can stop cleanly. A press after that escalates to a kill
			// for a tool that ignored the interrupt.
			if m.interruptPresses == 0 && !m.interruptArmed() {
				text := "press Esc again to interrupt"
				if len(m.steering) > 0 {
					text = "press Esc again to interrupt and send your steer"
				}
				cmd := m.flash(toneWarn, text)
				m.interruptEpoch = m.flashEpoch
				return m, cmd, true
			}
			m.interruptPresses++
			m.turn.cancel()
			if m.interruptPresses == 1 {
				m.say(toneDanger, "interrupted · press Esc again to kill the command")
				if len(m.steering) > 0 {
					m.say(toneDanger, "interrupted · sending your steer now…")
				}
				return m, nil, true
			}
			if m.runtime.Interrupt(m.interruptPresses) {
				m.say(toneDanger, "killed the command")
			} else {
				m.say(toneWarn, "no command to kill; waiting for the run to cancel")
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
			cmd := m.refreshInput()
			return m, cmd, true
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

// openMenu chooses the source at the cursor; both sources share navigation and
// rendering. Slash commands with a free-form argument can contain mentions too.
func (m *Model) openMenu() {
	m.menuSource = m.commands
	m.menu.note = ""
	if _, _, _, active := mentionAt(m.input.Value(), m.cursorOffset()); active {
		m.menuSource = mentionSource{}
	}
	items := m.menuSource.Candidates(*m, m.input.Value())
	if len(items) == 0 {
		m.menu.close()
		if _, mentions := m.menuSource.(mentionSource); mentions {
			switch {
			case m.mentions.loading:
				m.menu.note = "Finding files…"
			case m.mentions.err != nil:
				m.menu.note = "File search: " + oneLine(m.mentions.err.Error())
			default:
				m.menu.note = "No matching files"
			}
		}
		m.syncPreview()
		return
	}
	// Preserve the highlighted row while it is still present so cycling does
	// not jump when the candidate set is stable.
	previous := m.menu.selected().Value
	m.menu.items = items
	m.menu.index = 0
	if _, mentions := m.menuSource.(mentionSource); mentions && m.mentions.err != nil {
		m.menu.note = "Partial file list: " + oneLine(m.mentions.err.Error())
	}
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
	var cmd tea.Cmd
	if !m.menu.open() {
		cmd = m.loadMentionFiles()
		m.openMenu()
		m.resize()
		if !m.menu.open() {
			return m, cmd, m.mentions.loading
		}
	}
	m.commitMenu()
	cmd = tea.Batch(cmd, m.refreshInput())
	return m, cmd, true
}

// commitMenu applies the highlighted item through the popup source.
// Callers are responsible for resizing once the menu state is final.
func (m *Model) commitMenu() {
	m.menuSource.Accept(m, m.menu.selected().Value)
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
		// Enter on an empty prompt resumes a queue that an interrupted or
		// failed run left held.
		if !m.busy() && len(m.queued) > 0 {
			return m.sendQueued()
		}
		return m, nil
	}
	// Submitting commits: drop any highlighted preview so the live transcript
	// (or the resumed one) is what the command operates on and shows.
	m.resetMenu()
	m.resetMentions()
	if strings.HasPrefix(text, "/") {
		command, err := m.commands.parse(text)
		if err != nil {
			m.say(toneWarn, err.Error())
			return m, nil
		}
		return command.run(m)
	}
	if m.busy() {
		return m.steer(text)
	}
	if !m.canSend() {
		return m, nil
	}
	if err := m.history.append(m.cwd, text); err != nil {
		m.say(toneDanger, "error: "+err.Error())
		return m, nil
	}
	m.input.Reset()
	return m.send(text)
}

// canSend reports whether a prompt can start a run now, explaining in the
// status line when it cannot.
func (m *Model) canSend() bool {
	if !m.takeOver() {
		return false
	}
	if state := m.runtime.State(); !state.Ready() {
		m.say(toneDanger, state.Problem.Error()+" in "+m.configPath)
		return false
	}
	return true
}

// send starts a run for text, which the caller has already recorded in the
// prompt history and taken out of the input.
func (m Model) send(text string) (tea.Model, tea.Cmd) {
	m.transcript.add(block{kind: blockUser, text: sanitize(text)})
	m.resize()
	runtime, inbox := m.runtime, m.inbox
	cmd := m.startTurn("Working", func(ctx context.Context, emit func(agent.Event)) error {
		return runtime.Run(ctx, text, inbox, emit)
	})
	// Submitting is the user's own action: always show the new prompt, even
	// if they were scrolled up reading the transcript.
	m.viewport.GotoBottom()
	return m, cmd
}

// startTurn marks the model busy with fn, a prompt's run or a /compact, and
// shows its marker at once rather than a second later.
func (m *Model) startTurn(verb string, fn func(context.Context, func(agent.Event)) error) tea.Cmd {
	m.message, m.interruptPresses = "", 0
	r, cmd := m.startRun(verb, fn)
	m.turn = r
	r.paint(&m.transcript, r.start)
	m.refreshTranscript(true)
	return cmd
}

// busy reports whether a turn is in flight.
func (m *Model) busy() bool { return m.turn != nil }

func (m Model) applyTurnEvent(event agent.Event) (tea.Model, tea.Cmd) {
	commands := []tea.Cmd{m.turn.wait(), m.pollSpend()}
	if m.applyAgentEvent(event) {
		commands = append(commands, m.scheduleFlush())
	} else {
		m.refreshTranscript(true)
	}
	return m, tea.Batch(commands...)
}

func (m Model) finishTurn(err error) (tea.Model, tea.Cmd) {
	turn := m.turn
	m.turn, m.interruptPresses, m.interruptEpoch = nil, 0, 0
	// A response cut off before its usage report leaves an estimate that
	// nothing will replace, and the session never records it.
	m.streamed, m.streamedContext = 0, 0
	m.jobs = m.runtime.RunningJobs()
	m.syncRuntimeState()
	// An interrupted stream never received its done event, so finalize the
	// live stream here to freeze the partial answer and reasoning that were
	// already displayed. A cleanly finished run has nothing pending.
	m.transcript.finishStream()
	switch {
	case err == nil:
		m.message = ""
	case errors.Is(err, context.Canceled):
		m.say(toneDanger, "interrupted")
	case errors.Is(err, agent.ErrNothingToCompact):
		m.message = "nothing to compact"
	default:
		m.say(toneDanger, "error: "+err.Error())
		m.transcript.add(block{kind: blockError, text: err.Error()})
	}
	// The elapsed marker is the turn's last line: finalizing the stream and
	// appending any error first keeps it below everything it timed.
	m.transcript.liveTimer = ""
	if !turn.compaction {
		m.transcript.add(block{kind: blockElapsed, text: workedLabel(time.Since(turn.start))})
	}
	m.refreshTranscript(true)
	// A subagent the run started in the foreground has finished writing,
	// and one in a background job keeps spending, which the read's
	// answer goes on to poll.
	read := m.loadSpend()
	updated, cmd := m.dispatchPending(err)
	return updated, tea.Batch(read, cmd)
}

// retitleModelChanges names replayed model changes again. They were named
// before the catalog loaded, when a derived model had only its model ID.
func (m *Model) retitleModelChanges() {
	changed := false
	for i, b := range m.transcript.blocks {
		if b.kind != blockModel || b.model == nil {
			continue
		}
		if text := modelChangedText(m.runtime.DescribeSelection(*b.model)); text != b.text {
			m.transcript.blocks[i].text = text
			changed = true
		}
	}
	if changed {
		m.transcript.restyle()
		m.refreshTranscript(true)
	}
}

// syncRuntimeState refreshes the active model.
func (m *Model) syncRuntimeState() {
	state := m.runtime.State()
	m.active = state.Active
	m.configured = state.Ready() || state.Following()
}

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
