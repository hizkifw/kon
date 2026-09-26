package ui

import (
	"context"
	"encoding/json"
	"slices"
	"strings"
	"sync/atomic"
	"testing"
	"time"
	"unicode/utf8"

	"charm.land/bubbles/v2/textinput"
	tea "charm.land/bubbletea/v2"
	"github.com/hizkifw/kon/internal/agent"
	"github.com/hizkifw/kon/internal/app"
	"github.com/hizkifw/kon/internal/catalog"
	"github.com/hizkifw/kon/internal/config"
	"github.com/hizkifw/kon/internal/history"
	"github.com/hizkifw/kon/internal/login"
	"github.com/hizkifw/kon/internal/session"
	"github.com/hizkifw/kon/internal/tokens"
	"github.com/hizkifw/kon/internal/tools"
	"github.com/hizkifw/kon/internal/typedid"
)

type fakeRuntime struct {
	state         app.State
	models        []app.Model
	kills         int
	killFails     bool
	sessions      []session.Summary
	entries       []session.Entry
	id            typedid.SessionID
	contextTokens tokens.Count
	contextKnown  bool

	previewEntries []session.Entry
	previewErr     error
	previewed      string
	loginProvider  config.Provider
	catalogLoads   int
	efforts        []string

	followed    app.Followed
	followErr   error
	missed      []session.Entry
	takeOverErr error
	// runs counts Run calls, which arrive on the run's goroutine.
	runs    atomic.Int32
	notices chan string
	jobs    int
	jobList []tools.Job
	killed  []int
}

func (f *fakeRuntime) Models() []app.Model { return f.models }
func (f *fakeRuntime) State() app.State    { return f.state }
func (f *fakeRuntime) Run(context.Context, string, *agent.Inbox, func(agent.Event)) error {
	f.runs.Add(1)
	return nil
}
func (f *fakeRuntime) Compact(context.Context, func(agent.Event)) error { return nil }

// Notices is closed unless a test opens it, so draining Init's commands ends.
func (f *fakeRuntime) Notices() <-chan string {
	if f.notices == nil {
		f.notices = make(chan string)
		close(f.notices)
	}
	return f.notices
}
func (f *fakeRuntime) RunningJobs() int  { return f.jobs }
func (f *fakeRuntime) Jobs() []tools.Job { return f.jobList }
func (f *fakeRuntime) KillJob(id int) error {
	f.killed = append(f.killed, id)
	return nil
}
func (f *fakeRuntime) SubagentPreview(typedid.SessionID, int) ([]session.Entry, error) {
	return f.previewEntries, f.previewErr
}
func (f *fakeRuntime) NewSession() error { return nil }
func (f *fakeRuntime) Login(_ context.Context, provider config.Provider) (int, bool, error) {
	f.loginProvider = provider
	return 0, true, nil
}
func (f *fakeRuntime) LoginProviders() []string {
	return []string{"azure", "fireworks-ai", "ollama", "openai", "openai-compatible", "openrouter"}
}
func (f *fakeRuntime) LoginEntry(id string) (app.LoginEntry, bool) {
	if entry, ok := login.LocalEntry(id); ok {
		return entry, true
	}
	entries := map[string]catalog.Provider{
		"openai":       {ID: "openai", NPM: "@ai-sdk/openai"},
		"openrouter":   {ID: "openrouter", NPM: "@openrouter/ai-sdk-provider", API: "https://openrouter.ai/api/v1"},
		"fireworks-ai": {ID: "fireworks-ai", NPM: "@ai-sdk/openai-compatible", API: "https://api.fireworks.ai/inference/v1/"},
		"azure":        {ID: "azure", NPM: "@ai-sdk/azure"},
	}
	entry, ok := entries[id]
	if !ok {
		return app.LoginEntry{}, false
	}
	return login.CatalogEntry(entry)
}
func (f *fakeRuntime) DescribeSelection(selection session.ModelSelection) app.Model {
	for _, model := range f.models {
		if model.Name == selection.Name {
			return model
		}
	}
	return app.Model{Name: selection.Name, DisplayName: selection.ExternalID.String()}
}
func (f *fakeRuntime) LoadCatalog()                         { f.catalogLoads++ }
func (f *fakeRuntime) Interrupt(attempt int) bool           { f.kills++; return !f.killFails }
func (f *fakeRuntime) Resume(id typedid.SessionID) error    { f.id = id; return nil }
func (f *fakeRuntime) Sessions() ([]session.Summary, error) { return f.sessions, nil }
func (f *fakeRuntime) SessionID() typedid.SessionID         { return f.id }
func (f *fakeRuntime) SessionHistory() []session.Entry      { return f.entries }
func (f *fakeRuntime) Follow() (app.Followed, error)        { return f.followed, f.followErr }
func (f *fakeRuntime) TakeOver() ([]session.Entry, error) {
	if f.takeOverErr != nil {
		return nil, f.takeOverErr
	}
	f.state.Phase = app.PhaseReady
	return f.missed, nil
}
func (f *fakeRuntime) SessionPreview(path string, maxTurns int) ([]session.Entry, error) {
	f.previewed = path
	return f.previewEntries, f.previewErr
}
func (f *fakeRuntime) ContextUsage() (tokens.Count, bool) { return f.contextTokens, f.contextKnown }
func (f *fakeRuntime) DescribeTool(name string, args json.RawMessage, result string, failed bool, details json.RawMessage) tools.Display {
	return tools.Describe(name, args, result, failed, details, "/tmp")
}
func (f *fakeRuntime) SwitchModel(name string) error {
	for _, model := range f.models {
		if model.Name == name {
			f.state.Active = model
			return nil
		}
	}
	return app.ErrNotReady
}

func (f *fakeRuntime) CycleEffort() (string, error) {
	active := &f.state.Active
	if len(active.ReasoningEfforts) == 0 {
		return "", app.ErrNoEffort
	}
	i := slices.Index(active.ReasoningEfforts, active.ReasoningEffort)
	active.ReasoningEffort = ""
	if i+1 < len(active.ReasoningEfforts) {
		active.ReasoningEffort = active.ReasoningEfforts[i+1]
	}
	return active.ReasoningEffort, nil
}

func TestShiftTabCyclesEffortIntoHeader(t *testing.T) {
	m := newTestModel(t)
	header := func(m Model) string { return plain(strings.SplitN(m.View().Content, "\n", 2)[0]) }
	if got := header(m); !strings.HasSuffix(got, "kon · fast") {
		t.Fatalf("header without levels = %q", got)
	}
	updated, _, _ := m.handleKey("shift+tab")
	if status := updated.(Model).status; status != app.ErrNoEffort.Error() {
		t.Fatalf("status without levels = %q", status)
	}
	m.runtime.(*fakeRuntime).state.Active.ReasoningEfforts = []string{"low", "none"}
	m.syncRuntimeState()
	for _, want := range []string{"low", "no thinking", "default"} {
		updated, _, handled := m.handleKey("shift+tab")
		m = updated.(Model)
		if !handled || m.status != "reasoning effort: "+want+" (saved to config)" {
			t.Fatalf("handled = %v, status = %q", handled, m.status)
		}
		if got := header(m); !strings.HasSuffix(got, "kon · fast · "+want) {
			t.Fatalf("header = %q, want effort %q", got, want)
		}
	}
}

func TestHeaderPaintsWholeLineOnBarBackground(t *testing.T) {
	m := newTestModel(t)
	m.runtime.(*fakeRuntime).state.Active.ReasoningEfforts = []string{"low", "none"}
	m.syncRuntimeState()
	// The brand and the label are rendered as separate spans; a style reset in
	// either must not drop the bar background for the rest of the header line.
	line := strings.SplitN(m.View().Content, "\n", 2)[0]
	want := bgSeq(colorBarBg)
	bg := ""
	for i := 0; i < len(line); {
		if line[i] == 0x1b && i+1 < len(line) && line[i+1] == '[' {
			j := i + 2
			for j < len(line) && line[j] != 'm' {
				j++
			}
			params := line[i+2 : j]
			switch {
			case strings.Contains(params, "48;2;"):
				bg = params[strings.Index(params, "48;2;"):]
			case params == "" || params == "0" || strings.Contains(params, "49"):
				bg = ""
			}
			i = j + 1
			continue
		}
		r, size := utf8.DecodeRuneInString(line[i:])
		if bg != want {
			t.Fatalf("cell %q at byte %d has background %q, want %q", r, i, bg, want)
		}
		i += size
	}
}

func TestSanitizeRemovesTerminalEscapes(t *testing.T) {
	got := sanitize("plain\x1b[31mred\x1b[0m\x07")
	if got != "plainred" {
		t.Fatalf("sanitize = %q", got)
	}
}

func TestFitLineHonorsCellWidth(t *testing.T) {
	got := fitLine("123456", 4)
	if got != "123…" {
		t.Fatalf("fitLine = %q", got)
	}
}

func TestSwitchModelUpdatesRuntimeState(t *testing.T) {
	models := []app.Model{
		{Name: "fast", WireFormat: "openai", ExternalID: "gpt", ContextWindow: 100},
		{Name: "review", WireFormat: "anthropic", ExternalID: "claude", ContextWindow: 200},
	}
	runtime := &fakeRuntime{state: app.State{Active: models[0], Phase: app.PhaseReady}, models: models}
	m := New(context.Background(), "/tmp", "/tmp/config.json", runtime, history.New(t.TempDir()+"/history.jsonl"), nil)
	updated, _ := m.switchModel("review")
	got := updated.(Model)
	if got.active.Name != "review" || got.active.ContextWindow != 200 || got.contextTokens != -1 {
		t.Fatalf("unexpected state: %#v", got)
	}
}

func TestSwitchModelMessageMatchesHeaderFormatting(t *testing.T) {
	models := []app.Model{
		{Name: "fast", WireFormat: "openai", ExternalID: "gpt"},
		{
			Name:             "review",
			ConnectionID:     "fireworks-ai",
			DisplayName:      "DeepSeek V4.1 Flash",
			ExternalID:       "accounts/fireworks/models/deepseek-v4p1-flash",
			ReasoningEfforts: []string{"low", "high"},
		},
	}
	runtime := &fakeRuntime{state: app.State{Active: models[0], Phase: app.PhaseReady}, models: models}
	m := New(context.Background(), "/tmp", "/tmp/config.json", runtime, history.New(t.TempDir()+"/history.jsonl"), nil)
	updated, _ := m.switchModel("review")
	got := updated.(Model)
	if len(got.transcript.blocks) != 1 {
		t.Fatalf("expected one model change block, got %#v", got.transcript.blocks)
	}
	text := got.transcript.blocks[0].text
	want := " Model changed to fireworks-ai · DeepSeek V4.1 Flash · default"
	if text != want {
		t.Fatalf("model change message = %q, want %q", text, want)
	}
	if strings.Contains(text, "accounts/") || strings.Contains(text, "openai-compatible") {
		t.Fatalf("model change message leaked provider detail: %q", text)
	}
}

func TestLoginMasksKeyAndKeepsItOutOfTranscript(t *testing.T) {
	m := newTestModel(t)
	started, _ := m.startLogin("openai")
	m = started.(Model)
	m.login.input.SetValue("topsecret")
	if strings.Contains(m.View().Content, "topsecret") {
		t.Fatal("API key appeared in rendered view")
	}
	updated, cmd := m.updateLogin(tea.KeyPressMsg{Code: tea.KeyEnter})
	m = updated.(Model)
	if cmd == nil || !m.login.pending {
		t.Fatal("login request was not started")
	}
	finished, _ := m.Update(cmd())
	m = finished.(Model)
	if m.login != nil || !strings.Contains(m.status, "connected") {
		t.Fatalf("login did not finish: %q", m.status)
	}
	if strings.Contains(m.transcript.render(80), "topsecret") {
		t.Fatal("API key entered the transcript")
	}
}

func TestCompatibleLoginCollectsEndpointBeforeKey(t *testing.T) {
	runtime := &fakeRuntime{state: app.State{Phase: app.PhaseNeedsConfiguration}}
	m := New(context.Background(), "/tmp", "/tmp/config.json", runtime, history.New(t.TempDir()+"/history.jsonl"), nil)
	started, _ := m.startLogin("openai-compatible")
	m = started.(Model)
	m.login.input.SetValue("http://localhost:8080/v1")
	updated, cmd := m.updateLogin(tea.KeyPressMsg{Code: tea.KeyEnter})
	m = updated.(Model)
	if cmd != nil || m.login.step != 1 || m.login.input.EchoMode != textinput.EchoPassword {
		t.Fatalf("did not advance to hidden key step: %#v", m.login)
	}
	m.login.input.SetValue("secret")
	updated, cmd = m.updateLogin(tea.KeyPressMsg{Code: tea.KeyEnter})
	if cmd == nil {
		t.Fatal("login command was not started")
	}
	updated, _ = updated.(Model).Update(cmd())
	if runtime.loginProvider.BaseURL != "http://localhost:8080/v1" || runtime.loginProvider.APIKey != "secret" {
		t.Fatalf("login received %#v", runtime.loginProvider)
	}
}

func TestCatalogProviderLoginUsesKnownEndpoint(t *testing.T) {
	runtime := &fakeRuntime{state: app.State{Phase: app.PhaseNeedsConfiguration}}
	m := New(context.Background(), "/tmp", "/tmp/config.json", runtime, history.New(t.TempDir()+"/history.jsonl"), nil)
	started, _ := m.startLogin("fireworks-ai")
	m = started.(Model)
	if m.login.asksURL() || m.login.input.EchoMode != textinput.EchoPassword {
		t.Fatal("fixed-endpoint provider prompted for a URL")
	}
	m.login.input.SetValue("secret")
	updated, cmd := m.updateLogin(tea.KeyPressMsg{Code: tea.KeyEnter})
	if cmd == nil {
		t.Fatal("login command was not started")
	}
	updated, _ = updated.(Model).Update(cmd())
	connection := runtime.loginProvider
	if connection.ID != "fireworks-ai" || connection.CatalogProvider != "fireworks-ai" || connection.Type != "openai-compatible" || connection.BaseURL != "https://api.fireworks.ai/inference/v1" {
		t.Fatalf("login received %#v", connection)
	}
}

func TestAzureLoginRequiresEndpointAndKey(t *testing.T) {
	m := newTestModel(t)
	started, _ := m.startLogin("azure")
	m = started.(Model)
	if !m.login.asksURL() {
		t.Fatal("Azure endpoint was not requested")
	}
	m.login.input.SetValue("https://example.openai.azure.com/openai/v1")
	updated, _ := m.updateLogin(tea.KeyPressMsg{Code: tea.KeyEnter})
	m = updated.(Model)
	updated, cmd := m.updateLogin(tea.KeyPressMsg{Code: tea.KeyEnter})
	if cmd != nil || !strings.Contains(updated.(Model).status, "API key is required") {
		t.Fatal("Azure accepted an empty API key")
	}
}

func TestCancelledLoginCannotFinishNewFlow(t *testing.T) {
	m := newTestModel(t)
	started, _ := m.startLogin("openai")
	m = started.(Model)
	stale := m.login
	stale.cancel = func() {}
	cancelled, _ := m.updateLogin(tea.KeyPressMsg{Code: tea.KeyEscape})
	m = cancelled.(Model)
	started, _ = m.startLogin("openrouter")
	m = started.(Model)
	finished, _ := m.finishLogin(loginDoneMsg{flow: stale, verified: true})
	if finished.(Model).login == nil || finished.(Model).login.connection.ID != "openrouter" {
		t.Fatal("stale login response replaced the new flow")
	}
}

func TestInitLoadsCatalogAndRefreshesHeader(t *testing.T) {
	m := newTestModel(t)
	runtime := m.runtime.(*fakeRuntime)
	runtime.state.Active = app.Model{Name: "fw/deepseek", ConnectionID: "fw", DisplayName: "deepseek"}
	m.syncRuntimeState()
	var loaded tea.Msg
	cmds := []tea.Cmd{m.Init()}
	for len(cmds) > 0 {
		cmd := cmds[0]
		cmds = cmds[1:]
		if cmd == nil {
			continue
		}
		switch msg := cmd().(type) {
		case tea.BatchMsg:
			cmds = append(cmds, msg...)
		case catalogLoadedMsg:
			loaded = msg
		}
	}
	if loaded == nil || runtime.catalogLoads != 1 {
		t.Fatalf("Init did not load the catalog: loads=%d", runtime.catalogLoads)
	}
	runtime.state.Active = app.Model{Name: "fw/deepseek", ConnectionID: "fw", DisplayName: "DeepSeek V4 Pro", ContextWindow: 128_000}
	updated, _ := m.Update(loaded)
	view := plain(updated.(Model).View().Content)
	if !strings.Contains(view, "kon · fw · DeepSeek V4 Pro") || !strings.Contains(view, "/128.0k") {
		t.Fatalf("header/status not refreshed:\n%s", view)
	}
}

func TestTranscriptUsesTypedBlocks(t *testing.T) {
	var transcript transcript
	transcript.cwd = "/tmp"
	transcript.add(toolCallBlock("read", `{"path":"file"}`, "/tmp"))
	transcript.appendStream("answer")
	got := plain(transcript.render(80))
	if !strings.Contains(got, "file") || !strings.Contains(got, "answer") {
		t.Fatalf("render = %q", got)
	}
	if strings.Index(got, "file") > strings.Index(got, "answer") {
		t.Fatalf("tool call rendered after the answer: %q", got)
	}
	transcript.finishStream()
	if got := plain(transcript.render(80)); strings.Index(got, "file") > strings.Index(got, "answer") {
		t.Fatalf("final render = %q", got)
	}
}

func TestTranscriptRendersThinkingBeforeAnswer(t *testing.T) {
	var transcript transcript
	transcript.appendThinking("let me look")
	got := plain(transcript.render(80))
	if !strings.Contains(got, "let me look") {
		t.Fatalf("live render = %q", got)
	}
	transcript.appendStream("answer")
	got = plain(transcript.render(80))
	if strings.Index(got, "let me look") > strings.Index(got, "answer") {
		t.Fatalf("render = %q", got)
	}
	transcript.finishStream()
	got = plain(transcript.render(80))
	if strings.Index(got, "let me look") > strings.Index(got, "answer") {
		t.Fatalf("final render = %q", got)
	}
}

func TestInterleavedThinkingKeepsChronologicalOrder(t *testing.T) {
	var transcript transcript
	transcript.appendThinking("one")
	transcript.appendStream("a")
	transcript.appendThinking("two")
	transcript.appendStream("b")
	transcript.finishStream()
	got := plain(transcript.render(80))
	if order(got, "one", "a", "two", "b") {
		t.Fatalf("render = %q", got)
	}
}

// order reports whether any marker is missing or appears out of sequence.
func order(text string, markers ...string) bool {
	last := -1
	for _, marker := range markers {
		at := strings.Index(text, marker)
		if at < 0 || at < last {
			return true
		}
		last = at
	}
	return false
}

func TestTranscriptResetClearsPendingThinking(t *testing.T) {
	var transcript transcript
	transcript.appendThinking("hmm")
	transcript.reset()
	if got := transcript.render(80); got != "" {
		t.Fatalf("render after reset = %q", got)
	}
}

func TestThinkingDeltasWaitForRenderFrame(t *testing.T) {
	model := newTestModel(t)
	model.runEvents = make(chan tea.Msg)
	before := model.viewport.View()
	updated, _ := model.Update(runEventMsg{event: agent.Event{Kind: agent.EventThinking, Text: "hmm"}})
	afterThinking := updated.(Model)
	if !afterThinking.flushPending || afterThinking.viewport.View() != before {
		t.Fatal("thinking event repainted before the render frame")
	}
	updated, _ = afterThinking.Update(flushTranscriptMsg{})
	afterFlush := updated.(Model)
	if !strings.Contains(afterFlush.viewport.View(), "hmm") {
		t.Fatal("render frame did not flush streaming thinking")
	}
}

func TestTextEventsWaitForRenderFrame(t *testing.T) {
	model := newTestModel(t)
	model.runEvents = make(chan tea.Msg)
	before := model.viewport.View()
	updated, _ := model.Update(runEventMsg{event: agent.Event{Kind: agent.EventText, Text: "a"}})
	afterText := updated.(Model)
	if !afterText.flushPending || afterText.viewport.View() != before {
		t.Fatal("text event repainted before the render frame")
	}
	updated, _ = afterText.Update(flushTranscriptMsg{})
	afterFlush := updated.(Model)
	if afterFlush.flushPending || !strings.Contains(afterFlush.viewport.View(), "a") {
		t.Fatal("render frame did not flush streaming text")
	}
}

func TestReadResultsStayOutOfTranscript(t *testing.T) {
	model := newTestModel(t)
	event := agent.Event{
		Kind: agent.EventToolDone, Tool: "read",
		Arguments: `{"path":"internal/ui/events.go"}`,
		Text:      "     1  package ui\n… 44 more lines",
	}
	updated, _ := model.Update(runEventMsg{event: event})
	got := plain(updated.(Model).viewport.View())
	if strings.Contains(got, "package ui") {
		t.Fatal("read result echoed file contents into the transcript")
	}
	if !strings.Contains(got, "internal/ui/events.go") || !strings.Contains(got, "45 lines") {
		t.Fatalf("read result summary missing: %q", got)
	}
}

func TestReadErrorsStillRender(t *testing.T) {
	model := newTestModel(t)
	event := agent.Event{
		Kind: agent.EventToolDone, Tool: "read", IsError: true,
		Arguments: `{"path":"missing.txt"}`, Text: "error: open missing.txt: no such file or directory",
	}
	updated, _ := model.Update(runEventMsg{event: event})
	got := plain(updated.(Model).viewport.View())
	if !strings.Contains(got, "no such file or directory") {
		t.Fatalf("read error was hidden: %q", got)
	}
}

func TestReplayedToolOutcomeUsesPersistedErrorAndDetails(t *testing.T) {
	model := newTestModel(t)
	callID := typedid.ExternalToolCallID("failed-call")
	entries := []session.Entry{
		{Message: &session.Message{Role: session.RoleAssistant, Parts: []session.Part{{Type: session.PartToolCall, ToolCallID: callID, ToolName: "shell", ToolInput: json.RawMessage(`{"command":"./build"}`)}}}},
		{Message: &session.Message{Role: session.RoleTool, Parts: []session.Part{{Type: session.PartToolResult, ToolCallID: callID, ToolName: "shell", ToolOutput: "unstructured output"}}, IsError: true, Details: json.RawMessage(`{"exit_code":3,"duration":"4ms","output_bytes":0}`)}},
	}
	model.applyHistory(entries)
	result := model.transcript.blocks[len(model.transcript.blocks)-1]
	if result.display.State != tools.StateFailed || result.display.Note != "exit 3 · took 4ms" {
		t.Fatalf("replayed outcome = %#v", result.display)
	}
}

func TestReplayedCompactionShowsMarker(t *testing.T) {
	model := newTestModel(t)
	entries := []session.Entry{
		{Type: session.EntryTypeCompaction, TokensBefore: 12_300, TokensBeforeEstimated: true},
		{Message: &session.Message{Role: session.RoleUser, Parts: []session.Part{{Type: session.PartText, Text: "after"}}}},
	}
	model.applyHistory(entries)
	blocks := model.transcript.blocks
	if len(blocks) != 2 || blocks[0].kind != blockContext || blocks[0].text != "compacted ~12.3k tokens" {
		t.Fatalf("replayed compaction blocks = %#v", blocks)
	}
}

func TestParseSlashCommand(t *testing.T) {
	registry := defaultRegistry()
	command, err := registry.parse("/model review")
	if err != nil || command.command.name != "model" || len(command.args) != 1 || command.args[0] != "review" {
		t.Fatalf("command = %#v, err = %v", command, err)
	}
	if _, err := registry.parse("/new extra"); err == nil {
		t.Fatal("invalid /new was accepted")
	}
	if _, err := registry.parse("/model"); err != nil {
		t.Fatalf("optional argument rejected: %v", err)
	}
	if _, err := registry.parse("/bogus"); err == nil {
		t.Fatal("unknown command was accepted")
	}
	// An alias resolves to the same command and shares its argument validation.
	alias, err := registry.parse("/clear")
	if err != nil || alias.command.name != "new" {
		t.Fatalf("alias = %#v, err = %v", alias, err)
	}
	if _, err := registry.parse("/clear extra"); err == nil {
		t.Fatal("alias bypassed argument validation")
	}
}

// newMultiModel builds a model with the named profiles and a ready runtime,
// sized for layout-sensitive assertions. It covers the multi-model boilerplate
// shared by the popup tests.
func newMultiModel(t testing.TB, names ...string) Model {
	t.Helper()
	models := make([]app.Model, 0, len(names))
	for _, name := range names {
		models = append(models, app.Model{Name: name, WireFormat: "anthropic", ExternalID: "claude"})
	}
	runtime := &fakeRuntime{state: app.State{Active: models[0], Phase: app.PhaseReady}, models: models}
	m := New(context.Background(), "/tmp", "/tmp/config.json", runtime, history.New(t.TempDir()+"/history.jsonl"), nil)
	m.width, m.height = 80, 24
	return m
}

func TestRegistryCompleteDispatchesToArgument(t *testing.T) {
	m := newMultiModel(t, "fast", "review", "reason")

	got := m.commands.completion(m, "/co")
	if len(got) != 1 || got[0].Value != "/compact" {
		t.Fatalf("command completion = %#v", got)
	}
	// An alias matches completion but resolves to the canonical command's row.
	alias := m.commands.completion(m, "/cle")
	if len(alias) != 1 || alias[0].Value != "/new" {
		t.Fatalf("alias completion = %#v", alias)
	}
	got = m.commands.completion(m, "/mo")
	if len(got) != 1 || got[0].Value != "/model" {
		t.Fatalf("command completion = %#v", got)
	}
	got = m.commands.completion(m, "/model re")
	if len(got) != 2 || got[0].Value != "reason" || got[1].Value != "review" {
		t.Fatalf("argument completion = %#v", got)
	}
	if got := m.commands.completion(m, "/model review "); got != nil {
		t.Fatalf("completion past last argument = %#v", got)
	}
}

func TestModelPickerShowsDisplayNameAndKeepsQualifiedValue(t *testing.T) {
	const qualified = "fireworks-2/accounts/fireworks/models/deepseek-v4-pro"
	models := []app.Model{{
		Name: qualified, ConnectionID: "fireworks-2", DisplayName: "DeepSeek V4 Pro",
		WireFormat: "openai-compatible", ExternalID: "accounts/fireworks/models/deepseek-v4-pro", Source: "catalog",
	}}
	runtime := &fakeRuntime{state: app.State{Active: models[0], Phase: app.PhaseReady}, models: models}
	m := New(context.Background(), "/tmp", "/tmp/config.json", runtime, history.New(t.TempDir()+"/history.jsonl"), nil)
	items := completeModelNames(m, "deepseek")
	if len(items) != 1 || items[0].Value != qualified || items[0].Label != "fireworks-2 · DeepSeek V4 Pro" || items[0].Description != "accounts/fireworks/models/deepseek-v4-pro" {
		t.Fatalf("picker items = %#v", items)
	}
	rendered := plain((menu{items: items}).render(100))
	if !strings.Contains(rendered, "fireworks-2 · DeepSeek V4 Pro") || !strings.Contains(rendered, "accounts/fireworks/models/deepseek-v4-pro") || strings.Contains(rendered, "openai-compatible") {
		t.Fatalf("picker row = %q", rendered)
	}
	m.input.SetValue("/model deepseek")
	m.commands.Accept(&m, items[0].Value)
	if got := m.input.Value(); got != "/model "+qualified+" " {
		t.Fatalf("accepted value = %q", got)
	}
}

func TestRegistryCompletesAllCommandsOnBareSlash(t *testing.T) {
	m := newTestModel(t)
	got := m.commands.completion(m, "/")
	want := []string{"/new", "/model", "/login", "/resume", "/compact", "/jobs", "/kill", "/queue"}
	values := make([]string, 0, len(got))
	for _, item := range got {
		values = append(values, item.Value)
	}
	if strings.Join(values, " ") != strings.Join(want, " ") {
		t.Fatalf("bare slash completion = %#v", got)
	}
}

func TestSlashCommandsOnlyCompleteAtStart(t *testing.T) {
	m := newTestModel(t)
	if got := m.commands.completion(m, "can you help me with /foo"); got != nil {
		t.Fatalf("completion mid-prompt = %#v", got)
	}
	// A leading space also disqualifies the input.
	if got := m.commands.completion(m, " /model"); got != nil {
		t.Fatalf("completion after leading space = %#v", got)
	}
}

func TestTabFillsSelectedAfterArrowing(t *testing.T) {
	m := newMultiModel(t, "fast", "review", "reason")
	m.input.SetValue("/model re")
	m.openMenu()
	if got := m.menu.selected().Value; got != "reason" {
		t.Fatalf("initial selection = %q", got)
	}
	// The arrow key moves the selection; Tab fills exactly that selection.
	moved, _, handled := m.handleKey("down")
	if !handled {
		t.Fatal("down was not handled with the popup open")
	}
	filled, _, handled := moved.(Model).handleKey("tab")
	if !handled || filled.(Model).input.Value() != "/model review " {
		t.Fatalf("tab did not fill the arrow selection: %q", filled.(Model).input.Value())
	}
}

func TestTabFillsSelectedParameter(t *testing.T) {
	m := newMultiModel(t, "fast", "review")
	m.input.SetValue("/model r")

	opened, _, handled := m.handleKey("tab")
	if !handled || opened.(Model).input.Value() != "/model review " {
		t.Fatalf("tab did not fill the parameter: %q", opened.(Model).input.Value())
	}
}

func TestArrowsCycleWithoutFilling(t *testing.T) {
	m := newMultiModel(t, "fast", "review", "reason")
	m.input.SetValue("/model re")
	m.openMenu()

	if got := m.menu.selected().Value; got != "reason" {
		t.Fatalf("initial selection = %q", got)
	}
	moved, _, handled := m.handleKey("down")
	movedModel := moved.(Model)
	if !handled || movedModel.menu.selected().Value != "review" {
		t.Fatalf("down did not move the selection: %#v", movedModel.menu)
	}
	// Arrowing must not rewrite the prompt; only Tab fills it in.
	if movedModel.input.Value() != "/model re" {
		t.Fatalf("arrow key rewrite the prompt: %q", movedModel.input.Value())
	}
	up, _, _ := movedModel.handleKey("up")
	if got := up.(Model).menu.selected().Value; got != "reason" {
		t.Fatalf("up did not move the selection back: %q", got)
	}
}

func TestEnterAcceptsCompletion(t *testing.T) {
	m := newMultiModel(t, "fast", "review", "reason")
	m.input.SetValue("/model re")
	m.openMenu()
	if !m.menu.open() {
		t.Fatal("popup did not open for a multi-candidate argument")
	}
	accepted, _, handled := m.handleKey("enter")
	if !handled {
		t.Fatal("enter was not handled with the popup open")
	}
	if got := accepted.(Model).input.Value(); got != "/model reason " {
		t.Fatalf("enter did not accept the selection: %q", got)
	}
}

func TestEnterCompletesLikeTabThenSubmits(t *testing.T) {
	runtime := &fakeRuntime{state: app.State{Phase: app.PhaseReady}}
	m := New(context.Background(), "/tmp", "/tmp/config.json", runtime, history.New(t.TempDir()+"/history.jsonl"), nil)
	m.input.SetValue("/new")
	m.openMenu()
	// Enter behaves like Tab: with the popup open it accepts the selection,
	// appending a space. "/new " has no candidates, so the menu closes.
	completed, _ := m.Update(tea.KeyPressMsg{Code: tea.KeyEnter})
	got := completed.(Model)
	if got.input.Value() != "/new " {
		t.Fatalf("enter did not complete the selection: %q", got.input.Value())
	}
	if got.menu.open() {
		t.Fatal("menu stayed open after completion")
	}
	if got.status == "new session" {
		t.Fatal("enter ran the command instead of only completing")
	}
	// With the menu closed a second Enter submits and runs the command.
	submitted, _ := got.Update(tea.KeyPressMsg{Code: tea.KeyEnter})
	if submitted.(Model).status != "new session" {
		t.Fatalf("second enter did not submit: status %q", submitted.(Model).status)
	}
}

func TestMenuPopupAppearsOnLeadingSlashAndClears(t *testing.T) {
	m := newTestModel(t)
	typed, _ := m.Update(tea.KeyPressMsg{Code: '/', Text: "/"})
	m = typed.(Model)
	if !m.menu.open() || len(m.menu.items) != 8 {
		t.Fatalf("popup did not open on slash: %#v", m.menu)
	}
	// Typing ordinary text mid-prompt closes the popup and offers nothing.
	m.input.SetValue("hello /mo")
	m.openMenu()
	if m.menu.open() {
		t.Fatalf("popup opened for a mid-prompt slash: %#v", m.menu)
	}
}

func TestTabAcceptsTrailingToken(t *testing.T) {
	m := newTestModel(t)
	m.input.SetValue("/model fa")
	updated, _, handled := m.handleKey("tab")
	if !handled || updated.(Model).input.Value() != "/model fast " {
		t.Fatalf("tab completion = %q", updated.(Model).input.Value())
	}
}

func TestForwardDeleteRefreshesPopup(t *testing.T) {
	m := newMultiModel(t, "fast", "review")
	m.input.SetValue("/model review")
	m.openMenu()
	if !m.menu.open() {
		t.Fatal("popup did not open for the argument")
	}
	// Place the cursor before the trailing "w" and delete it forward; the
	// input becomes "/model revie" and the popup must recompute.
	m.input.SetCursorColumn(len("/model revie"))
	updated, _ := m.Update(tea.KeyPressMsg{Code: tea.KeyDelete})
	got := updated.(Model)
	if got.input.Value() != "/model revie" {
		t.Fatalf("delete did not change the input: %q", got.input.Value())
	}
	if !got.menu.open() || got.menu.selected().Value != "review" {
		t.Fatalf("popup was stale after forward delete: %#v", got.menu)
	}
}

func TestDeleteToClosePopup(t *testing.T) {
	m := newTestModel(t)
	typed, _ := m.Update(tea.KeyPressMsg{Code: '/', Text: "/"})
	m = typed.(Model)
	if !m.menu.open() {
		t.Fatal("popup did not open on slash")
	}
	// Backspace the slash away: the popup must close.
	updated, _ := m.Update(tea.KeyPressMsg{Code: tea.KeyBackspace})
	if updated.(Model).menu.open() {
		t.Fatal("popup stayed open after the slash was removed")
	}
}

func TestPasteRefreshesPopup(t *testing.T) {
	m := newTestModel(t)
	typed, _ := m.Update(tea.KeyPressMsg{Code: '/', Text: "/"})
	m = typed.(Model)
	if !m.menu.open() {
		t.Fatal("popup did not open on slash")
	}
	// Pasting "new" turns the input into "/new"; the popup must narrow to it.
	pasted, _ := m.Update(tea.PasteMsg{Content: "new"})
	got := pasted.(Model)
	if got.input.Value() != "/new" {
		t.Fatalf("paste did not update the input: %q", got.input.Value())
	}
	if !got.menu.open() || len(got.menu.items) != 1 || got.menu.items[0].Value != "/new" {
		t.Fatalf("popup was stale after paste: %#v", got.menu)
	}
}

func TestPasteClosesPopup(t *testing.T) {
	m := newTestModel(t)
	typed, _ := m.Update(tea.KeyPressMsg{Code: '/', Text: "/"})
	m = typed.(Model)
	if !m.menu.open() {
		t.Fatal("popup did not open on slash")
	}
	// Pasting plain text that breaks the command prefix closes the popup.
	pasted, _ := m.Update(tea.PasteMsg{Content: "zzz"})
	if pasted.(Model).menu.open() {
		t.Fatal("popup stayed open after pasting a non-command")
	}
}

func TestTabCompletionAppendsSpaceAndAdvancesMenu(t *testing.T) {
	m := newMultiModel(t, "fast", "review")
	m.input.SetValue("/mod")

	// Completing the command appends a space and advances to the argument
	// list, so the menu stays open on the profiles.
	completed, _, handled := m.handleKey("tab")
	completedModel := completed.(Model)
	if !handled || completedModel.input.Value() != "/model " {
		t.Fatalf("command completion = %q", completedModel.input.Value())
	}
	if !completedModel.menu.open() || completedModel.menu.selected().Value != "fast" {
		t.Fatalf("menu did not advance to the argument list: %#v", completedModel.menu)
	}
	// The next Tab fills an argument and then closes, since nothing follows.
	argued, _, handled := completedModel.handleKey("tab")
	if !handled || argued.(Model).input.Value() != "/model fast " {
		t.Fatalf("argument completion = %q", argued.(Model).input.Value())
	}
	if argued.(Model).menu.open() {
		t.Fatal("menu stayed open after completing the last argument")
	}
}

func TestTabCompletionResizesViewportImmediately(t *testing.T) {
	m := newMultiModel(t, "fast", "review", "reason")
	m.input.SetValue("/mod")
	m.openMenu()

	// The command menu has one entry (height 1); after Tab the argument menu
	// has three. The viewport must already account for the taller menu in the
	// same frame, otherwise the popup renders in the wrong place and reflows
	// on the next update.
	completed, _, _ := m.handleKey("tab")
	completedModel := completed.(Model)
	if completedModel.menu.height() != 3 {
		t.Fatalf("menu height = %d, want 3", completedModel.menu.height())
	}
	want := 24 - 1 - 2 - 3
	if got := completedModel.viewport.Height(); got != want {
		t.Fatalf("viewport height = %d, want %d", got, want)
	}
}

func TestTabCompletionClosesSegmentWithNoCandidates(t *testing.T) {
	m := newTestModel(t)
	m.input.SetValue("/new")
	completed, _, handled := m.handleKey("tab")
	if !handled || completed.(Model).input.Value() != "/new " {
		t.Fatalf("no-argument command completion = %q", completed.(Model).input.Value())
	}
	if completed.(Model).menu.open() {
		t.Fatal("menu stayed open after completing a no-argument command")
	}
}

func TestShellResultBlockSplitsCodeAndDuration(t *testing.T) {
	event := agent.Event{
		Kind: agent.EventToolDone, Tool: "shell",
		Arguments: `{"command":"go build ./...","timeout":120}`,
		Text:      "warnings here\nexit code: 0 (took 4.2s)",
	}
	model := newTestModel(t)
	b := model.toolResultBlock(event)
	d := b.display
	if d.State != tools.StateDone || d.Status != "exit 0 · took 4.2s" || len(d.Lines) != 1 || d.Lines[0] != "warnings here" || !d.Quiet {
		t.Fatalf("toolResultBlock display = %#v", d)
	}
}

func TestSecondEscKillsRunningCommand(t *testing.T) {
	model := newTestModel(t)
	model.busy = true
	model.runCancel = func() {}
	first, _, handled := model.handleKey("esc")
	firstModel := first.(Model)
	if !handled || firstModel.interruptPresses != 1 || !strings.Contains(firstModel.status, "interrupt") {
		t.Fatalf("first esc did not cancel the run: status %q", firstModel.status)
	}
	second, _, _ := firstModel.handleKey("esc")
	secondModel := second.(Model)
	runtime := secondModel.runtime.(*fakeRuntime)
	if runtime.kills != 1 || !strings.Contains(secondModel.status, "killed") {
		t.Fatalf("second esc did not kill the command: kills=%d status %q", runtime.kills, secondModel.status)
	}
}

func TestEscWithoutACommandReportsCancellation(t *testing.T) {
	model := newTestModel(t)
	model.busy = true
	model.runCancel = func() {}
	model.runtime.(*fakeRuntime).killFails = true
	first, _, _ := model.handleKey("esc")
	second, _, handled := first.(Model).handleKey("esc")
	secondModel := second.(Model)
	if !handled {
		t.Fatal("second esc was not handled")
	}
	if secondModel.runtime.(*fakeRuntime).kills != 1 {
		t.Fatal("second esc did not attempt the kill")
	}
	if !strings.Contains(secondModel.status, "no command to kill") {
		t.Fatalf("second esc left a stale status: %q", secondModel.status)
	}
}

func TestPromptUsesTerminalCursor(t *testing.T) {
	model := newTestModel(t)
	view := model.View()
	if !view.ReportFocus {
		t.Fatal("view does not request focus reports, so the cursor cannot stop blinking on blur")
	}
	if model.input.VirtualCursor() {
		t.Fatal("prompt uses a virtual cursor that tmux cannot hide in an inactive pane")
	}
	want := model.input.Cursor()
	if view.Cursor == nil || want == nil {
		t.Fatal("focused prompt does not expose a terminal cursor")
	}
	if view.Cursor.X != want.X+1 || view.Cursor.Y <= want.Y {
		t.Fatalf("terminal cursor is not offset into the prompt: got (%d,%d), textarea (%d,%d)", view.Cursor.X, view.Cursor.Y, want.X, want.Y)
	}
}

func TestBlurHidesPromptCursor(t *testing.T) {
	model := newTestModel(t)
	blurred, _ := model.Update(tea.BlurMsg{})
	if cursor := blurred.(Model).View().Cursor; cursor != nil {
		t.Fatalf("blur left the terminal cursor visible at (%d,%d)", cursor.X, cursor.Y)
	}
	focused, _ := blurred.(Model).Update(tea.FocusMsg{})
	if focused.(Model).View().Cursor == nil {
		t.Fatal("focus did not restore the terminal cursor")
	}
}

func TestEscInterruptsBusyRun(t *testing.T) {
	model := newTestModel(t)
	model.busy = true
	canceled := false
	model.runCancel = func() { canceled = true }
	updated, _, handled := model.handleKey("esc")
	got := updated.(Model)
	if !handled || !canceled {
		t.Fatalf("esc did not interrupt the run: handled=%v canceled=%v status=%q", handled, canceled, got.status)
	}
	if !strings.Contains(got.status, "interrupt") {
		t.Fatalf("status = %q", got.status)
	}
}

func TestEscClosesMenuBeforeInterrupting(t *testing.T) {
	model := newTestModel(t)
	model.busy = true
	model.runCancel = func() {}
	model.menu.items = []menuItem{{Value: "/model", Description: "pick"}}
	updated, _, handled := model.handleKey("esc")
	got := updated.(Model)
	if !handled || got.menu.open() {
		t.Fatalf("esc did not close the menu first: handled=%v open=%v", handled, got.menu.open())
	}
}

func TestEscWithoutBusyRunIsUnhandled(t *testing.T) {
	model := newTestModel(t)
	if _, _, handled := model.handleKey("esc"); handled {
		t.Fatal("esc was handled when there was nothing to interrupt")
	}
}

func TestCtrlCClearsInputOrHints(t *testing.T) {
	model := newTestModel(t)
	model.input.SetValue("draft prompt")
	updated, cmd, handled := model.handleKey("ctrl+c")
	got := updated.(Model)
	if !handled || cmd != nil {
		t.Fatalf("ctrl+c with text: handled=%v cmd=%v", handled, cmd)
	}
	if got.input.Value() != "" {
		t.Fatalf("ctrl+c did not clear the input: %q", got.input.Value())
	}

	updated, _, handled = got.handleKey("ctrl+c")
	got = updated.(Model)
	if !handled {
		t.Fatal("ctrl+c on empty input was not handled")
	}
	if !strings.Contains(got.status, "Ctrl+D") {
		t.Fatalf("ctrl+c on empty input did not hint at Ctrl+D: %q", got.status)
	}
}

func TestCtrlCNeverQuitsOrInterrupts(t *testing.T) {
	model := newTestModel(t)
	canceled := false
	model.busy = true
	model.runCancel = func() { canceled = true }
	_, cmd, handled := model.handleKey("ctrl+c")
	if !handled {
		t.Fatal("ctrl+c was not handled")
	}
	if cmd != nil {
		if _, quit := cmd().(tea.QuitMsg); quit {
			t.Fatal("ctrl+c quit the app")
		}
	}
	if canceled {
		t.Fatal("ctrl+c interrupted the run")
	}
}

func TestCtrlDQuitsOnEmptyInput(t *testing.T) {
	model := newTestModel(t)
	canceled := false
	model.busy = true
	model.runCancel = func() { canceled = true }
	_, cmd, handled := model.handleKey("ctrl+d")
	if !handled || cmd == nil {
		t.Fatal("ctrl+d on empty input was not handled")
	}
	if !canceled {
		t.Fatal("ctrl+d did not cancel the active run")
	}
	if _, ok := cmd().(tea.QuitMsg); !ok {
		t.Fatal("ctrl+d on empty input did not quit")
	}
	model.input.SetValue("  ")
	if _, cmd, handled := model.handleKey("ctrl+d"); !handled || cmd != nil {
		t.Fatal("ctrl+d on whitespace input should no-op, not quit")
	}
	model.input.SetValue("hello")
	if _, cmd, handled := model.handleKey("ctrl+d"); !handled || cmd != nil {
		t.Fatal("ctrl+d with text should no-op, not quit")
	}
}

func TestCtrlUKillsToLineStartAndCtrlYYanks(t *testing.T) {
	// Drive through Update so the textarea performs the deletion and the
	// kill ring is populated exactly as it is at runtime.
	model := newTestModel(t)
	model.input.SetValue("keep this tail")

	// Move the cursor to sit between "keep this" and " tail".
	for i := 0; i < len(" tail"); i++ {
		updated, _ := model.Update(tea.KeyPressMsg{Code: tea.KeyLeft})
		model = updated.(Model)
	}

	updated, _ := model.Update(tea.KeyPressMsg{Code: 'u', Mod: tea.ModCtrl})
	model = updated.(Model)
	if got := model.input.Value(); got != " tail" {
		t.Fatalf("ctrl+u did not discard to line start: %q", got)
	}
	if model.killRing != "keep this" {
		t.Fatalf("ctrl+u kill ring = %q, want %q", model.killRing, "keep this")
	}

	updated, _ = model.Update(tea.KeyPressMsg{Code: 'y', Mod: tea.ModCtrl})
	model = updated.(Model)
	if got := model.input.Value(); got != "keep this tail" {
		t.Fatalf("ctrl+y did not restore the killed text: %q", got)
	}
}

func TestCtrlYWithoutAKillIsNoop(t *testing.T) {
	model := newTestModel(t)
	model.input.SetValue("prompt")
	updated, _ := model.Update(tea.KeyPressMsg{Code: 'y', Mod: tea.ModCtrl})
	if got := updated.(Model).input.Value(); got != "prompt" {
		t.Fatalf("ctrl+y with an empty kill ring changed the input: %q", got)
	}
}

func TestCtrlUOnEmptyPrefixKeepsKillRing(t *testing.T) {
	model := newTestModel(t)
	model.input.SetValue("alpha")
	// Kill the whole line so the ring holds it, then kill again with the
	// cursor already at line start: the second kill removes nothing and must
	// not wipe the ring.
	updated, _ := model.Update(tea.KeyPressMsg{Code: 'u', Mod: tea.ModCtrl})
	model = updated.(Model)
	if model.killRing != "alpha" {
		t.Fatalf("kill ring = %q, want %q", model.killRing, "alpha")
	}
	updated, _ = model.Update(tea.KeyPressMsg{Code: 'u', Mod: tea.ModCtrl})
	model = updated.(Model)
	if model.killRing != "alpha" {
		t.Fatalf("empty kill clobbered the ring: %q", model.killRing)
	}
}

func TestCtrlUKillsOnlyCurrentLine(t *testing.T) {
	model := newTestModel(t)
	model.input.SetValue("first line\nsecond line")
	// Cursor starts at the end of the last line; kill it and confirm the
	// first line is untouched.
	updated, _ := model.Update(tea.KeyPressMsg{Code: 'u', Mod: tea.ModCtrl})
	model = updated.(Model)
	if got := model.input.Value(); got != "first line\n" {
		t.Fatalf("ctrl+u crossed a line boundary: %q", got)
	}
	if model.killRing != "second line" {
		t.Fatalf("kill ring = %q, want %q", model.killRing, "second line")
	}
}

func TestCtrlKKillsToLineEndAndCtrlYYanks(t *testing.T) {
	model := newTestModel(t)
	model.input.SetValue("keep this tail")
	// Cursor at end; move left over " tail" so Ctrl+K removes it.
	for i := 0; i < len(" tail"); i++ {
		updated, _ := model.Update(tea.KeyPressMsg{Code: tea.KeyLeft})
		model = updated.(Model)
	}
	updated, _ := model.Update(tea.KeyPressMsg{Code: 'k', Mod: tea.ModCtrl})
	model = updated.(Model)
	if got := model.input.Value(); got != "keep this" {
		t.Fatalf("ctrl+k did not kill to line end: %q", got)
	}
	if model.killRing != " tail" {
		t.Fatalf("ctrl+k kill ring = %q, want %q", model.killRing, " tail")
	}
	updated, _ = model.Update(tea.KeyPressMsg{Code: 'y', Mod: tea.ModCtrl})
	model = updated.(Model)
	if got := model.input.Value(); got != "keep this tail" {
		t.Fatalf("ctrl+y after ctrl+k = %q", got)
	}
}

func TestCtrlWKillsWordAndCtrlYYanks(t *testing.T) {
	model := newTestModel(t)
	model.input.SetValue("alpha beta")
	updated, _ := model.Update(tea.KeyPressMsg{Code: 'w', Mod: tea.ModCtrl})
	model = updated.(Model)
	if model.killRing != "beta" {
		t.Fatalf("ctrl+w kill ring = %q, want %q", model.killRing, "beta")
	}
	updated, _ = model.Update(tea.KeyPressMsg{Code: 'y', Mod: tea.ModCtrl})
	model = updated.(Model)
	if got := model.input.Value(); got != "alpha beta" {
		t.Fatalf("ctrl+y after ctrl+w = %q", got)
	}
}

func TestRemovedSpan(t *testing.T) {
	cases := []struct {
		before, after string
		cursor        int
		want          string
	}{
		{"keep this tail", " tail", 9, "keep this"},
		{"keep this tail", "keep this", 9, " tail"},
		{"alpha beta", "alpha ", 10, "beta"},
		// The deleted text shares bytes with what follows it; a prefix diff
		// would report the shifted "ba" or split the multibyte rune.
		{"aba", "a", 2, "ab"},
		{"éè", "è", 2, "é"},
		{"same", "same", 4, ""},
		{"short", "longer text", 5, ""},
	}
	for _, tc := range cases {
		if got := removedSpan(tc.before, tc.after, tc.cursor); got != tc.want {
			t.Errorf("removedSpan(%q, %q, %d) = %q, want %q", tc.before, tc.after, tc.cursor, got, tc.want)
		}
	}
}

func TestCtrlUKillsRepeatedTextAndCtrlYRestoresIt(t *testing.T) {
	for _, tc := range []struct{ value, killed string }{
		{"aba", "ab"},
		{"éè", "é"},
	} {
		model := newTestModel(t)
		model.input.SetValue(tc.value)
		updated, _ := model.Update(tea.KeyPressMsg{Code: tea.KeyLeft})
		model = updated.(Model)
		updated, _ = model.Update(tea.KeyPressMsg{Code: 'u', Mod: tea.ModCtrl})
		model = updated.(Model)
		if model.killRing != tc.killed {
			t.Fatalf("%q: kill ring = %q, want %q", tc.value, model.killRing, tc.killed)
		}
		updated, _ = model.Update(tea.KeyPressMsg{Code: 'y', Mod: tea.ModCtrl})
		model = updated.(Model)
		if got := model.input.Value(); got != tc.value {
			t.Fatalf("%q: ctrl+y restored %q", tc.value, got)
		}
	}
}

func TestCtrlUOnLaterLineUsesThatLinesCursor(t *testing.T) {
	model := newTestModel(t)
	model.input.SetValue("xx\nxxy")
	updated, _ := model.Update(tea.KeyPressMsg{Code: tea.KeyLeft})
	model = updated.(Model)
	updated, _ = model.Update(tea.KeyPressMsg{Code: 'u', Mod: tea.ModCtrl})
	model = updated.(Model)
	if model.killRing != "xx" {
		t.Fatalf("kill ring = %q, want %q", model.killRing, "xx")
	}
	if got := model.input.Value(); got != "xx\ny" {
		t.Fatalf("ctrl+u left %q", got)
	}
}

func TestInterruptedRunFinalizesStreamedTurn(t *testing.T) {
	model := newTestModel(t)
	model.busy = true
	model.runCancel = func() {}
	model.transcript.appendThinking("hmm")
	model.transcript.appendStream("half an answer")
	updated, _ := model.Update(runDoneMsg{err: context.Canceled})
	got := updated.(Model)
	if got.busy || got.status != "interrupted" {
		t.Fatalf("busy=%v status=%q", got.busy, got.status)
	}
	// The partial stream must be frozen into stable blocks so it survives.
	if got.transcript.thinking != "" || len(got.transcript.stream) != 0 {
		t.Fatal("interrupted run left the transcript stream open")
	}
	rendered := plain(got.viewport.View())
	if !strings.Contains(rendered, "half an answer") {
		t.Fatalf("partial answer missing from transcript: %q", rendered)
	}
}

func TestShiftOrCtrlEnterInsertsNewline(t *testing.T) {
	for _, mod := range []tea.KeyMod{tea.ModShift, tea.ModCtrl} {
		model := newTestModel(t)
		model.input.SetValue("first")
		updated, _ := model.Update(tea.KeyPressMsg{Code: tea.KeyEnter, Mod: mod})
		got := updated.(Model)
		if got.input.Value() != "first\n" {
			t.Fatalf("enter with mod %v = %q, want a newline", mod, got.input.Value())
		}
		if got.busy {
			t.Fatalf("enter with mod %v submitted the prompt", mod)
		}
	}
}

func TestAltEnterNoLongerInsertsNewline(t *testing.T) {
	model := newTestModel(t)
	model.input.SetValue("first")
	updated, _ := model.Update(tea.KeyPressMsg{Code: tea.KeyEnter, Mod: tea.ModAlt})
	if got := updated.(Model).input.Value(); got != "first" {
		t.Fatalf("alt+enter changed the prompt: %q", got)
	}
}

func TestEnterAfterBackslashInsertsNewline(t *testing.T) {
	model := newTestModel(t)
	model.input.SetValue(`keep this\`)
	updated, _ := model.Update(tea.KeyPressMsg{Code: tea.KeyEnter})
	got := updated.(Model)
	if got.input.Value() != "keep this\n" {
		t.Fatalf("trailing backslash enter = %q, want %q", got.input.Value(), "keep this\n")
	}
	if got.busy {
		t.Fatalf("trailing backslash enter submitted the prompt")
	}
}

func TestEnterWithoutBackslashSubmits(t *testing.T) {
	model := newTestModel(t)
	model.input.SetValue("plain prompt")
	updated, _ := model.Update(tea.KeyPressMsg{Code: tea.KeyEnter})
	if got := updated.(Model); got.input.Value() != "" {
		t.Fatalf("enter left the prompt unsubmitted: %q", got.input.Value())
	}
}

func TestRunForwardingStopsWhenEventsAreNoLongerRead(t *testing.T) {
	for _, tc := range []struct {
		name string
		run  func(context.Context, func(agent.Event), chan<- struct{}) error
	}{
		{
			name: "event send",
			run: func(_ context.Context, emit func(agent.Event), started chan<- struct{}) error {
				close(started)
				emit(agent.Event{Kind: agent.EventText, Text: "hello"})
				return nil
			},
		},
		{
			name: "completion send",
			run: func(_ context.Context, _ func(agent.Event), started chan<- struct{}) error {
				close(started)
				return nil
			},
		},
	} {
		t.Run(tc.name, func(t *testing.T) {
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()
			events := make(chan tea.Msg)
			finished := make(chan struct{})
			started := make(chan struct{})
			go func() {
				runAndForward(ctx, events, func(ctx context.Context, emit func(agent.Event)) error {
					return tc.run(ctx, emit, started)
				})
				close(finished)
			}()
			<-started
			cancel()
			select {
			case <-finished:
			case <-time.After(time.Second):
				t.Fatal("run forwarding blocked after cancellation")
			}
		})
	}
}

func TestCancellingProgramContextReleasesAbandonedRun(t *testing.T) {
	// A signal quits Bubble Tea without Update, so nothing reads the run's
	// events and the run's own cancel is never called. Cancelling the context
	// the model was built with must still let the run return.
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	runtime := &fakeRuntime{state: app.State{Phase: app.PhaseReady}}
	m := New(ctx, "/tmp", "/tmp/config.json", runtime, history.New(t.TempDir()+"/history.jsonl"), nil)
	finished := make(chan struct{})
	emitted := make(chan struct{})
	m.startRun("thinking…", func(_ context.Context, emit func(agent.Event)) error {
		defer close(finished)
		close(emitted)
		emit(agent.Event{Kind: agent.EventText, Text: "hello"})
		return nil
	})
	<-emitted
	cancel()
	select {
	case <-finished:
	case <-time.After(time.Second):
		t.Fatal("run stayed blocked after the program context was cancelled")
	}
}

func TestTypingDoesNotScrollTranscript(t *testing.T) {
	model := newTestModel(t)
	model.transcript.add(block{kind: blockUser, text: strings.Repeat("line\n", 60)})
	model.refreshTranscript(false)
	if got := model.viewport.YOffset(); got != 0 {
		t.Fatalf("initial offset = %d, want 0", got)
	}
	for _, key := range []string{"j", "k", "d", "u", "b", "f", "h", "l", " "} {
		updated, _ := model.Update(tea.KeyPressMsg{Code: rune(key[0]), Text: key})
		model = updated.(Model)
		if got := model.viewport.YOffset(); got != 0 {
			t.Fatalf("typing %q scrolled the transcript to offset %d", key, got)
		}
	}
	model.input.SetValue("hello")
	updated, _ := model.Update(tea.KeyPressMsg{Code: 'd', Mod: tea.ModCtrl})
	model = updated.(Model)
	if got := model.viewport.YOffset(); got != 0 {
		t.Fatalf("typing ctrl+d scrolled the transcript to offset %d", got)
	}
}

func TestMouseWheelStillScrollsTranscript(t *testing.T) {
	model := newTestModel(t)
	model.transcript.add(block{kind: blockUser, text: strings.Repeat("line\n", 60)})
	model.refreshTranscript(false)
	updated, _ := model.Update(tea.MouseWheelMsg{Button: tea.MouseWheelDown})
	model = updated.(Model)
	if got := model.viewport.YOffset(); got == 0 {
		t.Fatal("mouse wheel did not scroll the transcript")
	}
}

func TestStreamingFollowsOnlyWhenAtBottom(t *testing.T) {
	model := newTestModel(t)
	model.transcript.add(block{kind: blockUser, text: strings.Repeat("line\n", 60)})
	model.refreshTranscript(false)

	// A user who scrolled up must not be yanked to the bottom by new output.
	model.transcript.add(block{kind: blockAssistant, text: "new output"})
	model.refreshTranscript(true)
	if got := model.viewport.YOffset(); got != 0 {
		t.Fatalf("streaming yanked a scrolled-up reader to offset %d", got)
	}

	// A user parked at the bottom must keep following as content grows.
	model.viewport.GotoBottom()
	model.transcript.add(block{kind: blockAssistant, text: "more output"})
	model.refreshTranscript(true)
	if !model.viewport.AtBottom() {
		t.Fatalf("viewport stopped following at offset %d", model.viewport.YOffset())
	}
	if !strings.Contains(model.viewport.View(), "more output") {
		t.Fatal("followed viewport did not show the newest block")
	}
}

func TestSubmitAnchorsToBottom(t *testing.T) {
	model := newTestModel(t)
	model.transcript.add(block{kind: blockUser, text: strings.Repeat("line\n", 60)})
	model.refreshTranscript(false)
	if model.viewport.AtBottom() {
		t.Fatal("precondition failed: viewport should start scrolled up")
	}
	model.input.SetValue("hello")
	updated, _ := model.submit()
	got := updated.(Model)
	if !got.viewport.AtBottom() {
		t.Fatalf("submit left the viewport at offset %d", got.viewport.YOffset())
	}
}

func TestResumeCommandReplaysSession(t *testing.T) {
	id, err := typedid.ParseSessionID("ses_00000000000000000000")
	if err != nil {
		t.Fatal(err)
	}
	models := []app.Model{{Name: "fast", WireFormat: "openai", ExternalID: "gpt"}}
	runtime := &fakeRuntime{
		state:    app.State{Active: models[0], Phase: app.PhaseReady},
		models:   models,
		sessions: []session.Summary{{ID: id}},
		entries: []session.Entry{
			{Message: &session.Message{Role: session.RoleSystem, Parts: []session.Part{{Type: session.PartText, Text: "system"}}}},
			{Message: &session.Message{Role: session.RoleUser, Parts: []session.Part{{Type: session.PartText, Text: "earlier question"}}}},
			{Message: &session.Message{Role: session.RoleAssistant, Parts: []session.Part{{Type: session.PartText, Text: "earlier answer"}}}},
		},
	}
	m := New(context.Background(), "/tmp", "/tmp/config.json", runtime, history.New(t.TempDir()+"/history.jsonl"), nil)
	m.width, m.height = 80, 24
	m.resize()

	updated, _ := m.resume([]string{id.String()})
	got := updated.(Model)
	if runtime.id != id {
		t.Fatalf("runtime resumed %s, want %s", runtime.id, id)
	}
	rendered := plain(got.viewport.View())
	if !strings.Contains(rendered, "earlier question") || !strings.Contains(rendered, "earlier answer") {
		t.Fatalf("resumed transcript missing history: %q", rendered)
	}
	if got.status != "resumed "+id.String() {
		t.Fatalf("status = %q", got.status)
	}
}

func TestResumeCommandReplaysThinking(t *testing.T) {
	id, err := typedid.ParseSessionID("ses_00000000000000000000")
	if err != nil {
		t.Fatal(err)
	}
	models := []app.Model{{Name: "fast", WireFormat: "openai", ExternalID: "gpt"}}
	runtime := &fakeRuntime{
		state:    app.State{Active: models[0], Phase: app.PhaseReady},
		models:   models,
		sessions: []session.Summary{{ID: id}},
		entries: []session.Entry{
			{Message: &session.Message{Role: session.RoleSystem, Parts: []session.Part{{Type: session.PartText, Text: "system"}}}},
			{Message: &session.Message{Role: session.RoleUser, Parts: []session.Part{{Type: session.PartText, Text: "earlier question"}}}},
			{Message: &session.Message{
				Role:  session.RoleAssistant,
				Parts: []session.Part{{Type: "reasoning", Text: "let me think"}, {Type: session.PartText, Text: "earlier answer"}},
			}},
		},
	}
	m := New(context.Background(), "/tmp", "/tmp/config.json", runtime, history.New(t.TempDir()+"/history.jsonl"), nil)
	m.width, m.height = 80, 24
	m.resize()

	updated, _ := m.resume([]string{id.String()})
	got := updated.(Model)
	rendered := plain(got.viewport.View())
	if !strings.Contains(rendered, "let me think") {
		t.Fatalf("resumed transcript missing reasoning: %q", rendered)
	}
}

func TestResumeAdoptsPersistedContextUsage(t *testing.T) {
	id, err := typedid.ParseSessionID("ses_00000000000000000000")
	if err != nil {
		t.Fatal(err)
	}
	models := []app.Model{{Name: "fast", WireFormat: "openai", ExternalID: "gpt"}}
	runtime := &fakeRuntime{
		state:         app.State{Active: models[0], Phase: app.PhaseReady},
		models:        models,
		sessions:      []session.Summary{{ID: id}},
		entries:       []session.Entry{{Message: &session.Message{Role: session.RoleUser, Parts: []session.Part{{Type: session.PartText, Text: "hi"}}}}},
		contextTokens: 4321,
		contextKnown:  true,
	}
	m := New(context.Background(), "/tmp", "/tmp/config.json", runtime, history.New(t.TempDir()+"/history.jsonl"), nil)
	m.width, m.height = 80, 24
	m.resize()

	updated, _ := m.resume([]string{id.String()})
	got := updated.(Model)
	if got.contextTokens != 4321 {
		t.Fatalf("contextTokens = %d, want 4321", got.contextTokens)
	}
}

func TestResumeCommandListsWithoutID(t *testing.T) {
	id, err := typedid.ParseSessionID("ses_00000000000000000000")
	if err != nil {
		t.Fatal(err)
	}
	runtime := &fakeRuntime{
		state:    app.State{Phase: app.PhaseReady},
		sessions: []session.Summary{{ID: id, CreatedAt: time.Unix(0, 0)}},
	}
	m := New(context.Background(), "/tmp", "/tmp/config.json", runtime, history.New(t.TempDir()+"/history.jsonl"), nil)
	m.width, m.height = 80, 24
	m.resize()
	updated, _ := m.resume(nil)
	got := updated.(Model)
	if !strings.Contains(plain(got.viewport.View()), id.String()) {
		t.Fatalf("listing does not show the session ID: %q", plain(got.viewport.View()))
	}
}

func TestResumeCommandRejectsMalformedID(t *testing.T) {
	m := newTestModel(t)
	updated, _ := m.resume([]string{"not-a-session"})
	if got := updated.(Model); !strings.HasPrefix(got.status, "error:") {
		t.Fatalf("status = %q", got.status)
	}
}

// TestResumePreviewRendersHighlightedSession drives the temporary transcript
// override: highlighting a /resume row shows that session in the viewport, and
// cancelling the popup restores the live transcript.
func TestResumePreviewRendersHighlightedSession(t *testing.T) {
	newID, err := typedid.ParseSessionID("ses_00000000000000000001")
	if err != nil {
		t.Fatal(err)
	}
	oldID, err := typedid.ParseSessionID("ses_00000000000000000002")
	if err != nil {
		t.Fatal(err)
	}
	runtime := &fakeRuntime{
		state: app.State{Phase: app.PhaseReady},
		sessions: []session.Summary{
			{ID: newID, Path: "newer.jsonl", Title: "newer work", CreatedAt: time.Unix(10, 0)},
			{ID: oldID, Path: "older.jsonl", Title: "older work", CreatedAt: time.Unix(0, 0)},
		},
		previewEntries: []session.Entry{
			{Message: &session.Message{Role: session.RoleUser, Parts: []session.Part{{Type: session.PartText, Text: "previewed question"}}}},
			{Message: &session.Message{Role: session.RoleAssistant, Parts: []session.Part{{Type: session.PartText, Text: "previewed answer"}}}},
		},
	}
	m := New(context.Background(), "/tmp", "/tmp/config.json", runtime, history.New(t.TempDir()+"/history.jsonl"), nil)
	m.width, m.height = 80, 24
	m.resize()
	m.transcript.add(block{kind: blockUser, text: "live conversation"})

	// Opening the resume argument list highlights the newest row and previews
	// it without switching the live session.
	m.input.SetValue("/resume ")
	m.openMenu()
	if runtime.previewed != "newer.jsonl" {
		t.Fatalf("previewed %q, want the newest session's path", runtime.previewed)
	}
	rendered := plain(m.viewport.View())
	if !strings.Contains(rendered, "previewed question") || !strings.Contains(rendered, "previewed answer") {
		t.Fatalf("preview missing from the transcript: %q", rendered)
	}
	if strings.Contains(rendered, "live conversation") {
		t.Fatalf("live transcript leaked into the preview: %q", rendered)
	}
	if runtime.id != (typedid.SessionID{}) {
		t.Fatalf("preview switched the live session to %s", runtime.id)
	}

	// Cycling to the older row previews it instead.
	m.menu.move(1)
	m.syncPreview()
	if runtime.previewed != "older.jsonl" {
		t.Fatalf("cycled preview = %q, want the older session's path", runtime.previewed)
	}

	// Cancelling restores the live transcript.
	m.resetMenu()
	rendered = plain(m.viewport.View())
	if !strings.Contains(rendered, "live conversation") {
		t.Fatalf("cancel did not restore the live transcript: %q", rendered)
	}
	if strings.Contains(rendered, "previewed question") {
		t.Fatalf("preview survived cancellation: %q", rendered)
	}
	if runtime.id != (typedid.SessionID{}) {
		t.Fatalf("cancel switched the live session to %s", runtime.id)
	}
}

// TestResumePreviewIsLazy guards the cost of listing: building candidates must
// not read every session file, only the highlighted row's.
func TestResumePreviewIsLazy(t *testing.T) {
	id, err := typedid.ParseSessionID("ses_00000000000000000001")
	if err != nil {
		t.Fatal(err)
	}
	runtime := &fakeRuntime{
		state:    app.State{Phase: app.PhaseReady},
		sessions: []session.Summary{{ID: id, Path: "one.jsonl"}},
	}
	m := newTestModel(t)
	m.runtime = runtime
	got := m.commands.completion(m, "/resume ")
	if len(got) != 1 {
		t.Fatalf("completion = %#v", got)
	}
	if runtime.previewed != "" {
		t.Fatalf("completion read a session file before a row was highlighted: %s", runtime.previewed)
	}
}

func TestResumedSessionStartsAtBottom(t *testing.T) {
	entries := []session.Entry{{Message: &session.Message{Role: session.RoleSystem, Parts: []session.Part{{Type: session.PartText, Text: "system"}}}}}
	for i := 0; i < 40; i++ {
		entries = append(entries, session.Entry{Message: &session.Message{Role: session.RoleUser, Parts: []session.Part{{Type: session.PartText, Text: strings.Repeat("line\n", 3) + "tail"}}}})
	}
	runtime := &fakeRuntime{state: app.State{Phase: app.PhaseReady}, entries: entries}
	m := New(context.Background(), "/tmp", "/tmp/config.json", runtime, history.New(t.TempDir()+"/history.jsonl"), nil)
	if !m.startAtBottom {
		t.Fatal("resumed model did not request an initial scroll to the bottom")
	}

	updated, _ := m.Update(tea.WindowSizeMsg{Width: 80, Height: 24})
	got := updated.(Model)
	if got.startAtBottom {
		t.Fatal("start-at-bottom request was not consumed")
	}
	if !got.viewport.AtBottom() {
		t.Fatalf("resumed transcript opened at offset %d, not the bottom", got.viewport.YOffset())
	}
	if !strings.Contains(got.viewport.View(), "tail") {
		t.Fatal("bottom of the resumed transcript is not visible")
	}
}

func TestWindowResizeKeepsBottomLineVisible(t *testing.T) {
	entries := []session.Entry{{Message: &session.Message{Role: session.RoleSystem, Parts: []session.Part{{Type: session.PartText, Text: "system"}}}}}
	for i := 0; i < 40; i++ {
		entries = append(entries, session.Entry{Message: &session.Message{Role: session.RoleUser, Parts: []session.Part{{Type: session.PartText, Text: strings.Repeat("line ", 20) + "tail"}}}})
	}
	runtime := &fakeRuntime{state: app.State{Phase: app.PhaseReady}, entries: entries}
	var m tea.Model = New(context.Background(), "/tmp", "/tmp/config.json", runtime, history.New(t.TempDir()+"/history.jsonl"), nil)
	m, _ = m.Update(tea.WindowSizeMsg{Width: 120, Height: 30})
	for _, size := range []tea.WindowSizeMsg{{Width: 120, Height: 12}, {Width: 50, Height: 12}, {Width: 160, Height: 40}} {
		m, _ = m.Update(size)
		got := m.(Model)
		if !got.viewport.AtBottom() {
			t.Fatalf("resize to %dx%d left offset %d, not the bottom", size.Width, size.Height, got.viewport.YOffset())
		}
		if !strings.Contains(got.viewport.View(), "tail") {
			t.Fatalf("resize to %dx%d hid the last transcript line", size.Width, size.Height)
		}
	}
}

func TestFreshSessionDoesNotForceBottom(t *testing.T) {
	m := newTestModel(t)
	if m.startAtBottom {
		t.Fatal("a fresh session requested an initial scroll to the bottom")
	}
}

func TestCompactCommandStartsBusyRun(t *testing.T) {
	m := newTestModel(t)
	m.input.SetValue("/compact")
	updated, cmd := m.submit()
	got := updated.(Model)
	if !got.busy || got.status != "compacting…" || got.runEvents == nil {
		t.Fatalf("compact did not start a run: busy=%v status=%q", got.busy, got.status)
	}
	if cmd == nil {
		t.Fatal("compact did not return a wait command")
	}
	if got.input.Value() != "" {
		t.Fatalf("compact left input behind: %q", got.input.Value())
	}
}

func TestCompactCommandRefusesWhileBusy(t *testing.T) {
	m := newTestModel(t)
	m.busy = true
	updated, _ := m.compact()
	if got := updated.(Model); got.status != "agent is busy; Esc interrupts" {
		t.Fatalf("status = %q", got.status)
	}
}

func TestNothingToCompactIsNotAnError(t *testing.T) {
	m := newTestModel(t)
	m.busy = true
	updated, _ := m.Update(runDoneMsg{err: agent.ErrNothingToCompact})
	got := updated.(Model)
	if got.status != "nothing to compact" {
		t.Fatalf("status = %q", got.status)
	}
	if strings.Contains(plain(got.viewport.View()), "error") {
		t.Fatalf("nothing-to-compact rendered as an error: %q", plain(got.viewport.View()))
	}
}

// TestUnconfiguredLaunchGreetsInTranscript guards the first-run experience: an
// unconfigured launch introduces itself as an assistant message in the
// transcript (naming the config file and the key tips) instead of only a status
// line, and the status stays a short pointer. A configured launch shows no such
// message.
func TestUnconfiguredLaunchGreetsInTranscript(t *testing.T) {
	runtime := &fakeRuntime{state: app.State{
		Phase:   app.PhaseNeedsConfiguration,
		Problem: app.ErrNotReady,
		Active:  app.Model{Name: "default"},
	}}
	m := New(context.Background(), "/tmp", "/tmp/config.json", runtime, history.New(t.TempDir()+"/history.jsonl"), nil)
	if m.status != "needs configuration" {
		t.Fatalf("status = %q", m.status)
	}
	if len(m.transcript.blocks) != 1 || m.transcript.blocks[0].kind != blockAssistant {
		t.Fatalf("unconfigured launch did not greet in the transcript: %#v", m.transcript.blocks)
	}
	greeting := m.transcript.blocks[0].text
	for _, want := range []string{"/tmp/config.json", "`/`", "Ctrl+D"} {
		if !strings.Contains(greeting, want) {
			t.Fatalf("greeting missing %q: %q", want, greeting)
		}
	}

	ready := &fakeRuntime{state: app.State{Phase: app.PhaseReady}}
	if got := New(context.Background(), "/tmp", "/tmp/config.json", ready, history.New(t.TempDir()+"/history.jsonl"), nil); len(got.transcript.blocks) != 0 {
		t.Fatalf("configured launch greeted: %#v", got.transcript.blocks)
	}
}

func newTestModel(t testing.TB) Model {
	t.Helper()
	model := app.Model{Name: "fast", WireFormat: "openai", ExternalID: "gpt", ContextWindow: 100}
	runtime := &fakeRuntime{state: app.State{Active: model, Phase: app.PhaseReady}, models: []app.Model{model}}
	result := New(context.Background(), "/tmp", "/tmp/config.json", runtime, history.New(t.TempDir()+"/history.jsonl"), nil)
	result.width, result.height = 80, 24
	result.resize()
	return result
}
