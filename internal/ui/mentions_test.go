package ui

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"reflect"
	"slices"
	"strings"
	"testing"
	"time"

	tea "charm.land/bubbletea/v2"
	"kon.kitsu.red/core/agent"
)

func TestMentionAtCursor(t *testing.T) {
	for _, tt := range []struct {
		text, query, token string
		active             bool
	}{
		{"@|", "", "@", true},
		{"explain @mod|el.go next", "mod", "@model.go", true},
		{"read (@mod|el.go), please", "mod", "@model.go", true},
		{"你好\ncompare @一.go and @二|.go", "二", "@二.go", true},
		{`read @"docs/my fi|le.md" please`, "docs/my fi", `@"docs/my file.md"`, true},
		{`read @|"docs/my file.md"`, "", "", false},
		{`read @"|docs/my file.md"`, "", `@"docs/my file.md"`, true},
		{`read @"docs/my file.md"|`, "", "", false},
		{"mail person@exam|ple.com", "", "", false},
		{"`@deco|rator`", "", "", false},
		{"read @file.go |next", "", "", false},
		{"before |@file.go", "", "", false},
		{`open @internal\u|i\model.go`, `internal\u`, `@internal\ui\model.go`, true},
		{"/btw explain @mod|el.go", "mod", "@model.go", true},
	} {
		t.Run(tt.text, func(t *testing.T) {
			cursor := strings.IndexByte(tt.text, '|')
			input := strings.Replace(tt.text, "|", "", 1)
			start, end, query, active := mentionAt(input, cursor)
			if active != tt.active || active && (query != tt.query || input[start:end] != tt.token) {
				t.Fatalf("mention = %q, %q, %v; want %q, %q, %v", input[start:end], query, active, tt.token, tt.query, tt.active)
			}
		})
	}
}

func TestMentionCompletionPreservesPromptAndCursor(t *testing.T) {
	for _, tt := range []struct{ text, file, want string }{
		{"explain @mod|el.go please", "internal/ui/model.go", "explain @internal/ui/model.go |please"},
		{"read (@REA|DME.md), please", "README.md", "read (@README.md |), please"},
		{"你好\ncompare @one.go\nwith @二|.go\nnext", "src/二.go", "你好\ncompare @one.go\nwith @src/二.go |\nnext"},
		{`read @"docs/my fi|le.md" please`, "docs/my file.md", `read @"docs/my file.md" |please`},
		{"read @fi|", `docs/a"b.md`, `read @"docs/a\"b.md" |`},
	} {
		t.Run(tt.text, func(t *testing.T) {
			m := newTestModel(t)
			cursor := strings.IndexByte(tt.text, '|')
			m.input.SetValue(strings.Replace(tt.text, "|", "", 1))
			m.setCursorOffset(cursor)
			mentionSource{}.Accept(&m, tt.file)
			got := m.input.Value()[:m.cursorOffset()] + "|" + m.input.Value()[m.cursorOffset():]
			if got != tt.want {
				t.Fatalf("completed input = %q, want %q", got, tt.want)
			}
			if _, _, _, active := mentionAt(m.input.Value(), m.cursorOffset()); active {
				t.Fatal("accepted mention still activates completion")
			}
		})
	}
}

func TestMentionPickerFiltersAndAcceptsWhileBusy(t *testing.T) {
	m := newTestModel(t)
	fakeTurn(&m)
	m.input.SetValue("compare @mod")
	m.mentions = fileMentions{loaded: true, files: indexMentionFiles([]string{"README.md", "internal/ui/model_test.go", "internal/ui/model.go"})}
	m.refreshInput()
	if len(m.menu.items) != 2 || m.menu.selected().Value != "internal/ui/model.go" {
		t.Fatalf("file suggestions = %#v", m.menu.items)
	}
	for _, key := range []string{"down", "tab"} {
		updated, _, _ := m.handleKey(key)
		m = updated.(Model)
	}
	if got := m.input.Value(); got != "compare @internal/ui/model_test.go " || m.menu.open() || len(m.queued) != 0 {
		t.Fatalf("completion while busy: input %q, menu %#v, queue %#v", got, m.menu, m.queued)
	}
	updated, _, _ := m.handleKey("tab")
	m = updated.(Model)
	if !reflect.DeepEqual(m.queued, []string{"compare @internal/ui/model_test.go"}) {
		t.Fatalf("second Tab did not queue the completed prompt: %#v", m.queued)
	}
}

func TestMentionSearchIsLazyAndDoesNotReopenAfterDismissal(t *testing.T) {
	m := newTestModel(t)
	m.cwd = t.TempDir()
	writeMentionFile(t, m.cwd, "main.go")
	if cmd := m.loadMentionFiles(); cmd != nil || m.mentions.loading {
		t.Fatal("discovery ran without a mention")
	}
	m.input.SetValue("read @ma")
	cmd := m.refreshInput()
	if cmd == nil || !m.mentions.loading || m.menu.note != "Finding files…" {
		t.Fatal("mention did not schedule discovery")
	}
	updated, _, _ := m.handleKey("esc")
	m = updated.(Model)
	updated, _ = m.Update(cmd())
	m = updated.(Model)
	if m.menu.height() != 0 || !m.mentions.loaded {
		t.Fatal("late result reopened a dismissed menu")
	}
	updated, _, _ = m.handleKey("tab")
	m = updated.(Model)
	if m.input.Value() != "read @main.go " {
		t.Fatalf("Tab did not reopen and complete: %q", m.input.Value())
	}
	writeMentionFile(t, m.cwd, "new.go")
	m.input.SetValue("read @new")
	cmd = m.refreshInput()
	if cmd == nil {
		t.Fatal("next mention did not refresh the file list")
	}
	updated, _ = m.Update(cmd())
	if updated.(Model).menu.selected().Value != "new.go" {
		t.Fatal("newly created file was not discovered")
	}
}

func TestMentionAsyncResultsFollowCurrentInput(t *testing.T) {
	m := newTestModel(t)
	m.input.SetValue("@m")
	m.mentions = fileMentions{loading: true, epoch: 7}
	m.openMenu()
	updated, _ := m.Update(tea.PasteMsg{Content: "ain"})
	m = updated.(Model)
	updated, _ = m.Update(mentionFilesMsg{epoch: 6, files: indexMentionFiles([]string{"wrong.go"})})
	m = updated.(Model)
	if !m.mentions.loading {
		t.Fatal("stale discovery was accepted")
	}
	updated, _ = m.Update(mentionFilesMsg{epoch: 7, files: indexMentionFiles([]string{"model.go", "main.go"})})
	m = updated.(Model)
	if len(m.menu.items) != 1 || m.menu.selected().Value != "main.go" {
		t.Fatalf("results ignored current query: %#v", m.menu)
	}
	updated, _ = m.Update(tea.KeyPressMsg{Code: tea.KeyEnter})
	m = updated.(Model)
	if m.input.Value() != "@main.go " || m.busy() {
		t.Fatal("Enter did not accept without submitting")
	}
}

func TestMentionCursorMovementRefreshesPopup(t *testing.T) {
	m := newTestModel(t)
	m.input.SetValue("read @main.go next")
	m.setCursorOffset(len("read @ma"))
	m.mentions = fileMentions{loaded: true, files: indexMentionFiles([]string{"main.go"})}
	m.openMenu()
	updated, _ := m.Update(tea.KeyPressMsg{Code: tea.KeyRight})
	m = updated.(Model)
	if !m.menu.open() {
		t.Fatal("moving within a mention closed its popup")
	}
	updated, _ = m.Update(tea.KeyPressMsg{Code: tea.KeyEnd})
	if updated.(Model).menu.height() != 0 {
		t.Fatal("moving out of a mention left the popup open")
	}
}

func TestMentionPopupFitsSmallTerminal(t *testing.T) {
	m := newTestModel(t)
	m.height = 6
	m.input.SetValue("@")
	m.mentions = fileMentions{loaded: true, files: indexMentionFiles([]string{"a", "b", "c", "d", "e", "f", "g", "h", "i"})}
	m.refreshInput()
	for range 8 {
		updated, _, _ := m.handleKey("down")
		m = updated.(Model)
	}
	if rows := strings.Count(m.View().Content, "\n") + 1; rows != m.height {
		t.Fatalf("popup frame has %d rows, want %d", rows, m.height)
	}
	if !strings.Contains(plain(m.menu.render(m.width)), "@i") {
		t.Fatal("selected file is not visible")
	}
}

func TestUnmatchedMentionCanBeSent(t *testing.T) {
	m := newTestModel(t)
	fakeTurn(&m)
	m.input.SetValue("read @missing.go")
	m.mentions = fileMentions{loaded: true}
	m.refreshInput()
	if m.menu.note != "No matching files" {
		t.Fatal("missing file was not explained")
	}
	updated, _, _ := m.handleKey("enter")
	m = updated.(Model)
	if m.menu.height() != 0 || m.input.Value() != "" || !reflect.DeepEqual(m.steering, []string{"read @missing.go"}) {
		t.Fatalf("unmatched mention submission left stale state: menu %#v, steering %#v", m.menu, m.steering)
	}
}

func TestEnterSubmitsWhileMentionSearchIsLoading(t *testing.T) {
	m := newTestModel(t)
	fakeTurn(&m)
	m.input.SetValue("read @main.go")
	ctx, cancel := context.WithCancel(context.Background())
	t.Cleanup(cancel)
	m.mentions = fileMentions{loading: true, epoch: 7, cancel: cancel}
	m.openMenu()
	updated, _, _ := m.handleKey("enter")
	m = updated.(Model)
	if m.input.Value() != "" || !reflect.DeepEqual(m.steering, []string{"read @main.go"}) {
		t.Fatal("Enter did not send the prompt while discovery was running")
	}
	if ctx.Err() != context.Canceled || m.mentions.loading {
		t.Fatal("submission left discovery running")
	}
	updated, _ = m.Update(mentionFilesMsg{epoch: 7, files: indexMentionFiles([]string{"main.go"})})
	if got := updated.(Model); got.menu.height() != 0 || got.mentions.loaded {
		t.Fatal("late result was accepted after submission")
	}
}

func TestProseMentionDoesNotAcceptScatteredPathLetters(t *testing.T) {
	for _, tt := range []struct{ prompt, file string }{
		{"ping @alice", "app/logging/identity/cache/event.go"},
		{"decorate it with @property", "project/helpers/runtime/types.go"},
	} {
		t.Run(tt.prompt, func(t *testing.T) {
			m := newTestModel(t)
			fakeTurn(&m)
			m.input.SetValue(tt.prompt)
			m.mentions = fileMentions{loaded: true, files: indexMentionFiles([]string{tt.file})}
			m.refreshInput()
			updated, _, _ := m.handleKey("enter")
			m = updated.(Model)
			if m.input.Value() != "" || !reflect.DeepEqual(m.steering, []string{tt.prompt}) {
				t.Fatalf("prose mention was not sent intact: input %q, steering %v", m.input.Value(), m.steering)
			}
		})
	}
}

func TestUnmatchedMentionDoesNotClaimQueueOrInterrupt(t *testing.T) {
	for _, key := range []string{"tab", "esc"} {
		t.Run(key, func(t *testing.T) {
			m := newTestModel(t)
			fakeTurn(&m)
			ctx, cancel := context.WithCancel(context.Background())
			t.Cleanup(cancel)
			m.turn.cancel = cancel
			m.input.SetValue("explain @dataclass")
			m.mentions = fileMentions{loaded: true}
			m.refreshInput()
			updated, _, _ := m.handleKey(key)
			m = updated.(Model)
			if m.menu.height() != 0 {
				t.Fatal("no-match notice remained visible")
			}
			if key == "tab" && !reflect.DeepEqual(m.queued, []string{"explain @dataclass"}) {
				t.Fatalf("Tab did not queue the prompt: %v", m.queued)
			}
			if key == "esc" {
				// The notice takes no press of its own: the first Esc
				// already arms the interrupt, and the second fires it.
				updated, _, _ = m.handleKey(key)
				m = updated.(Model)
				if ctx.Err() != context.Canceled || m.interrupt.presses != 1 {
					t.Fatal("Esc did not interrupt on the second press")
				}
			}
		})
	}
}

func TestPartialMentionResultsRemainVisibleAndSelectable(t *testing.T) {
	for _, height := range []int{24, 6, 5} {
		t.Run(fmt.Sprint(height), func(t *testing.T) {
			m := newTestModel(t)
			m.height = height
			m.input.SetValue("read @ma")
			m.mentions = fileMentions{loading: true, epoch: 1}
			updated, _ := m.Update(mentionFilesMsg{epoch: 1, files: indexMentionFiles([]string{"main.go"}), err: errors.New("file search limit reached")})
			m = updated.(Model)
			view := strings.ToLower(plain(m.menu.render(m.width)))
			if !strings.Contains(view, "partial") || !strings.Contains(view, "main.go") {
				t.Fatalf("partial results not explained: %q", view)
			}
			if rows := strings.Count(m.View().Content, "\n") + 1; rows != height {
				t.Fatalf("frame has %d rows, want %d", rows, height)
			}
			updated, _, _ = m.handleKey("tab")
			if updated.(Model).input.Value() != "read @main.go " {
				t.Fatal("partial match was not selectable")
			}
		})
	}
}

type mentionRuntime struct {
	*fakeRuntime
	text chan string
}

func (r *mentionRuntime) Run(_ context.Context, text string, _ *agent.Inbox, _ func(agent.Event)) error {
	r.text <- text
	return nil
}

func TestMentionSendsAndRecallsPlainReference(t *testing.T) {
	m := newTestModel(t)
	r := &mentionRuntime{fakeRuntime: m.runtime.(*fakeRuntime), text: make(chan string, 1)}
	m.runtime = r
	m.input.SetValue("explain @main")
	m.mentions = fileMentions{loaded: true, files: indexMentionFiles([]string{"main.go"})}
	m.refreshInput()
	updated, _, _ := m.handleKey("enter")
	m = updated.(Model)
	updated, _, _ = m.handleKey("enter")
	m = updated.(Model)
	t.Cleanup(m.turn.cancel)
	select {
	case got := <-r.text:
		if got != "explain @main.go" {
			t.Fatalf("runtime prompt = %q", got)
		}
	case <-time.After(5 * time.Second):
		t.Fatal("completed mention never reached the runtime")
	}
	<-m.turn.events
	if got, ok := m.history.recall("", -1); !ok || got != "explain @main.go" {
		t.Fatalf("recalled mention = %q, %v", got, ok)
	}
}

func writeMentionFile(t *testing.T, root, name string) {
	t.Helper()
	name = filepath.Join(root, filepath.FromSlash(name))
	if err := os.MkdirAll(filepath.Dir(name), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(name, []byte("test\n"), 0o600); err != nil {
		t.Fatal(err)
	}
}

func TestMentionFuzzyRanking(t *testing.T) {
	m := newTestModel(t)
	m.mentions.files = indexMentionFiles([]string{"model.go.bak", "internal/ui/model.go", "modem.go", "model.go"})
	m.input.SetValue("@MODEL.GO")
	items := (mentionSource{}).Candidates(m, m.input.Value())
	if len(items) != 3 || items[2].Value != "model.go.bak" {
		t.Fatalf("exact matches did not rank first: %#v", items)
	}
	m.input.SetValue(`@i\u\mod`)
	items = (mentionSource{}).Candidates(m, m.input.Value())
	if len(items) != 1 || items[0].Value != "internal/ui/model.go" {
		t.Fatalf("fuzzy path did not match: %#v", items)
	}
	if !reflect.DeepEqual(m.mentions.files, indexMentionFiles([]string{"model.go.bak", "internal/ui/model.go", "modem.go", "model.go"})) {
		t.Fatal("filtering mutated the cached file list")
	}
}

func TestMentionResultsMatchFullSort(t *testing.T) {
	m := newTestModel(t)
	var paths []string
	for i := 500; i >= 0; i-- {
		paths = append(paths, fmt.Sprintf("Internal/Package%03d/Model.go", i), fmt.Sprintf("Model%03d.go", i))
	}
	m.mentions.files = indexMentionFiles(paths)
	for _, query := range []string{"", "model", "i/p/model", "missing"} {
		m.input.SetValue("@" + query)
		var want []string
		for _, path := range paths {
			if mentionScore(strings.ToLower(path), query) >= 0 {
				want = append(want, path)
			}
		}
		slices.SortFunc(want, func(a, b string) int {
			if delta := mentionScore(strings.ToLower(a), query) - mentionScore(strings.ToLower(b), query); delta != 0 {
				return delta
			}
			return strings.Compare(a, b)
		})
		want = want[:min(len(want), maxMentionMatches)]
		var got []string
		for _, item := range (mentionSource{}).Candidates(m, m.input.Value()) {
			got = append(got, item.Value)
		}
		if !slices.Equal(got, want) {
			t.Fatalf("bounded results for %q differ from full sort: got %v, want %v", query, got, want)
		}
	}
}
