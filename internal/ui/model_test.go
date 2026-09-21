package ui

import (
	"context"
	"strings"
	"testing"

	tea "charm.land/bubbletea/v2"
	"github.com/hizkifw/kon/internal/agent"
	"github.com/hizkifw/kon/internal/app"
	"github.com/hizkifw/kon/internal/history"
)

type fakeRuntime struct {
	state     app.State
	models    []app.Model
	kills     int
	killFails bool
}

func (f *fakeRuntime) Models() []app.Model                                  { return f.models }
func (f *fakeRuntime) State() app.State                                     { return f.state }
func (f *fakeRuntime) Run(context.Context, string, func(agent.Event)) error { return nil }
func (f *fakeRuntime) NewSession() error                                    { return nil }
func (f *fakeRuntime) KillShell() bool                                      { f.kills++; return !f.killFails }
func (f *fakeRuntime) SwitchModel(name string) error {
	for _, model := range f.models {
		if model.Name == name {
			f.state.Active = model
			return nil
		}
	}
	return app.ErrNotReady
}

func TestSanitizeRemovesTerminalEscapes(t *testing.T) {
	got := sanitize("plain\x1b[31mred\x1b[0m\x07")
	if got != "plainred" {
		t.Fatalf("sanitize = %q", got)
	}
}

func TestCompactNumber(t *testing.T) {
	if got := compactNumber(12_400); !strings.HasPrefix(got, "12.4k") {
		t.Fatalf("compactNumber = %q", got)
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
		{Name: "fast", Provider: "openai", ExternalID: "gpt", ContextWindow: 100},
		{Name: "review", Provider: "anthropic", ExternalID: "claude", ContextWindow: 200},
	}
	runtime := &fakeRuntime{state: app.State{Active: models[0], Phase: app.PhaseReady}, models: models}
	m := New("/tmp", "/tmp/config.json", runtime, history.New(t.TempDir()+"/history.jsonl"), nil)
	updated, _ := m.switchModel("review")
	got := updated.(Model)
	if got.active.Name != "review" || got.active.ContextWindow != 200 || got.contextTokens != -1 {
		t.Fatalf("unexpected state: %#v", got)
	}
}

func TestTranscriptUsesTypedBlocks(t *testing.T) {
	var transcript transcript
	transcript.add(block{kind: blockTool, name: "read", args: `{"path":"file"}`})
	transcript.appendStream("answer")
	got := plain(transcript.render(80))
	if !strings.Contains(got, "read") || !strings.Contains(got, "file") || !strings.Contains(got, "answer") {
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
	if !strings.Contains(got, "thinking") || !strings.Contains(got, "let me look") {
		t.Fatalf("live render = %q", got)
	}
	transcript.appendStream("answer")
	got = plain(transcript.render(80))
	if !strings.Contains(got, "thinking") || strings.Index(got, "let me look") > strings.Index(got, "answer") {
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

func TestParseSlashCommand(t *testing.T) {
	command, err := parseSlashCommand("/model review")
	if err != nil || command.name != "model" || command.argument != "review" {
		t.Fatalf("command = %#v, err = %v", command, err)
	}
	if _, err := parseSlashCommand("/new extra"); err == nil {
		t.Fatal("invalid /new was accepted")
	}
}

func TestShellResultBlockSplitsCodeAndDuration(t *testing.T) {
	event := agent.Event{
		Kind: agent.EventToolDone, Tool: "shell",
		Arguments: `{"command":"go build ./...","timeout":120}`,
		Text:      "warnings here\nexit code: 0 (took 4.2s)",
	}
	b := toolResultBlock(event)
	if b.failed || b.exit != "0" || b.took != "4.2s" || b.text != "warnings here" {
		t.Fatalf("toolResultBlock = %#v", b)
	}
}

func TestSecondCtrlCKillsRunningCommand(t *testing.T) {
	model := newTestModel(t)
	model.busy = true
	model.runCancel = func() {}
	first, _, handled := model.handleKey("ctrl+c")
	firstModel := first.(Model)
	if !handled || !firstModel.cancelRequested || !strings.Contains(firstModel.status, "cancelling") {
		t.Fatalf("first ctrl+c did not cancel the run: status %q", firstModel.status)
	}
	second, _, _ := firstModel.handleKey("ctrl+c")
	secondModel := second.(Model)
	runtime := secondModel.runtime.(*fakeRuntime)
	if runtime.kills != 1 || !strings.Contains(secondModel.status, "killed") {
		t.Fatalf("second ctrl+c did not kill the command: kills=%d status %q", runtime.kills, secondModel.status)
	}
}

func TestCtrlCWithoutACommandReportsCancellation(t *testing.T) {
	model := newTestModel(t)
	model.busy = true
	model.runCancel = func() {}
	model.runtime.(*fakeRuntime).killFails = true
	first, _, _ := model.handleKey("ctrl+c")
	second, _, handled := first.(Model).handleKey("ctrl+c")
	secondModel := second.(Model)
	if !handled {
		t.Fatal("second ctrl+c was not handled")
	}
	if secondModel.runtime.(*fakeRuntime).kills != 1 {
		t.Fatal("second ctrl+c did not attempt the kill")
	}
	if !strings.Contains(secondModel.status, "no command to kill") {
		t.Fatalf("second ctrl+c left a stale status: %q", secondModel.status)
	}
}

func TestCtrlDQuitsOnEmptyInput(t *testing.T) {
	model := newTestModel(t)
	_, cmd, handled := model.handleKey("ctrl+d")
	if !handled || cmd == nil {
		t.Fatal("ctrl+d on empty input was not handled")
	}
	if _, ok := cmd().(tea.QuitMsg); !ok {
		t.Fatal("ctrl+d on empty input did not quit")
	}
	model.input.SetValue("  ")
	if _, cmd, handled := model.handleKey("ctrl+d"); !handled || cmd == nil {
		t.Fatal("ctrl+d on whitespace input was not handled")
	}
	model.input.SetValue("hello")
	if _, _, handled := model.handleKey("ctrl+d"); handled {
		t.Fatal("ctrl+d with text was swallowed")
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

func newTestModel(t *testing.T) Model {
	t.Helper()
	model := app.Model{Name: "fast", Provider: "openai", ExternalID: "gpt", ContextWindow: 100}
	runtime := &fakeRuntime{state: app.State{Active: model, Phase: app.PhaseReady}, models: []app.Model{model}}
	result := New("/tmp", "/tmp/config.json", runtime, history.New(t.TempDir()+"/history.jsonl"), nil)
	result.width, result.height = 80, 24
	result.resize()
	return result
}
