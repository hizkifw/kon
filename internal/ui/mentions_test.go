package ui

import (
	"context"
	"os"
	"os/exec"
	"path/filepath"
	"reflect"
	"strings"
	"testing"
	"time"

	tea "charm.land/bubbletea/v2"
	"github.com/hizkifw/kon/internal/agent"
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
	m.busy = true
	m.input.SetValue("compare @mod")
	m.mentions = fileMentions{loaded: true, files: []string{"README.md", "internal/ui/model_test.go", "internal/ui/model.go"}}
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
	// Enter while the search is in flight must not send a partial prompt.
	updated, _, handled := m.handleKey("enter")
	m = updated.(Model)
	if !handled || m.busy || m.input.Value() != "read @ma" {
		t.Fatal("Enter submitted during discovery")
	}
	updated, _, _ = m.handleKey("esc")
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
	updated, _ = m.Update(mentionFilesMsg{epoch: 6, files: []string{"wrong.go"}})
	m = updated.(Model)
	if !m.mentions.loading {
		t.Fatal("stale discovery was accepted")
	}
	updated, _ = m.Update(mentionFilesMsg{epoch: 7, files: []string{"model.go", "main.go"}})
	m = updated.(Model)
	if len(m.menu.items) != 1 || m.menu.selected().Value != "main.go" {
		t.Fatalf("results ignored current query: %#v", m.menu)
	}
	updated, _ = m.Update(tea.KeyPressMsg{Code: tea.KeyEnter})
	m = updated.(Model)
	if m.input.Value() != "@main.go " || m.busy {
		t.Fatal("Enter did not accept without submitting")
	}
}

func TestMentionCursorMovementRefreshesPopup(t *testing.T) {
	m := newTestModel(t)
	m.input.SetValue("read @main.go next")
	m.setCursorOffset(len("read @ma"))
	m.mentions = fileMentions{loaded: true, files: []string{"main.go"}}
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
	m.mentions = fileMentions{loaded: true, files: []string{"a", "b", "c", "d", "e", "f", "g", "h", "i"}}
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
	m.busy = true
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
	m.mentions = fileMentions{loaded: true, files: []string{"main.go"}}
	m.refreshInput()
	updated, _, _ := m.handleKey("enter")
	m = updated.(Model)
	updated, _, _ = m.handleKey("enter")
	m = updated.(Model)
	t.Cleanup(m.runCancel)
	select {
	case got := <-r.text:
		if got != "explain @main.go" {
			t.Fatalf("runtime prompt = %q", got)
		}
	case <-time.After(5 * time.Second):
		t.Fatal("completed mention never reached the runtime")
	}
	<-m.runEvents
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

func TestMentionFilesRespectGitIgnoresAndScope(t *testing.T) {
	if _, err := exec.LookPath("git"); err != nil {
		t.Skip("git is not installed")
	}
	root := t.TempDir()
	git := func(args ...string) {
		t.Helper()
		cmd := exec.Command("git", append([]string{"-C", root}, args...)...)
		if out, err := cmd.CombinedOutput(); err != nil {
			t.Fatalf("git %v: %s: %v", args, out, err)
		}
	}
	git("init", "--quiet")
	for _, name := range []string{"tracked.go", "deleted.go", "untracked.go", "docs/space name.md", "docs/你好.md", "node_modules/ignored.js", "build.log"} {
		writeMentionFile(t, root, name)
	}
	if err := os.WriteFile(filepath.Join(root, ".gitignore"), []byte("node_modules/\n*.log\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	git("add", "tracked.go", "deleted.go", "docs")
	if err := os.Remove(filepath.Join(root, "deleted.go")); err != nil {
		t.Fatal(err)
	}
	files, err := listMentionFiles(context.Background(), root)
	want := []string{".gitignore", "docs/space name.md", "docs/你好.md", "tracked.go", "untracked.go"}
	if err != nil || !reflect.DeepEqual(files, want) {
		t.Fatalf("Git files = %v, %v; want %v", files, err, want)
	}
	files, err = listMentionFiles(context.Background(), filepath.Join(root, "docs"))
	if err != nil || !reflect.DeepEqual(files, []string{"space name.md", "你好.md"}) {
		t.Fatalf("subdirectory files = %v, %v", files, err)
	}
}

func TestMentionFilesWorkWithoutGit(t *testing.T) {
	root := t.TempDir()
	for _, name := range []string{"main.go", "docs/my file.md", ".github/workflow.yml", ".git/objects/hidden", "node_modules/hidden", ".venv/hidden"} {
		writeMentionFile(t, root, name)
	}
	t.Setenv("PATH", t.TempDir())
	files, err := listMentionFiles(context.Background(), root)
	want := []string{".github/workflow.yml", "docs/my file.md", "main.go"}
	if err != nil || !reflect.DeepEqual(files, want) {
		t.Fatalf("plain files = %v, %v; want %v", files, err, want)
	}
	ctx, cancel := context.WithCancel(context.Background())
	cancel()
	if _, err := walkMentionFiles(ctx, root); err == nil {
		t.Fatal("cancelled discovery succeeded")
	}
}

func TestMentionFuzzyRanking(t *testing.T) {
	m := newTestModel(t)
	m.mentions.files = []string{"model.go.bak", "internal/ui/model.go", "modem.go", "model.go"}
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
	if !reflect.DeepEqual(m.mentions.files, []string{"model.go.bak", "internal/ui/model.go", "modem.go", "model.go"}) {
		t.Fatal("filtering mutated the cached file list")
	}
}
