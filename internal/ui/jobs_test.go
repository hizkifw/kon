package ui

import (
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"testing"

	tea "charm.land/bubbletea/v2"
	"github.com/hizkifw/kon/internal/session"
	"github.com/hizkifw/kon/internal/tools"
)

// jobsModel is a sized model whose session has a finished command, 1, and
// a running one, 2, writing to output files in a temporary directory.
func jobsModel(t *testing.T) (Model, *fakeRuntime) {
	t.Helper()
	m := sizedModel(t, 100, 30)
	runtime := m.runtime.(*fakeRuntime)
	dir := t.TempDir()
	runtime.jobList = []tools.Job{
		{ID: 2, Command: "npm run dev", Output: filepath.Join(dir, "2")},
		{ID: 1, Command: "make test", Exit: "0", Output: filepath.Join(dir, "1")},
	}
	return m, runtime
}

func writeOutput(t *testing.T, job tools.Job, text string) {
	t.Helper()
	f, err := os.OpenFile(job.Output, os.O_CREATE|os.O_APPEND|os.O_WRONLY, 0o600)
	if err != nil {
		t.Fatal(err)
	}
	defer f.Close()
	if _, err := f.WriteString(text); err != nil {
		t.Fatal(err)
	}
}

// pollNow runs one read of the job files as its tick would, and applies it.
func pollNow(t *testing.T, m Model) Model {
	t.Helper()
	msg := m.pollJobs()()
	updated, _ := m.Update(msg)
	return updated.(Model)
}

func screen(m Model) string { return plain(m.View().Content) }

// sendKey sends a key press through Update, as the terminal would, so it
// reaches the top drawer.
func sendKey(t *testing.T, m Model, name string) Model {
	t.Helper()
	msg := map[string]tea.KeyPressMsg{
		"enter": {Code: tea.KeyEnter},
		"esc":   {Code: tea.KeyEscape},
		"up":    {Code: tea.KeyUp},
		"down":  {Code: tea.KeyDown},
		"home":  {Code: tea.KeyHome},
		"j":     {Code: 'j', Text: "j"},
		"k":     {Code: 'k', Text: "k"},
		"K":     {Code: 'K', Text: "K"},
	}[name]
	if msg.String() != name {
		t.Fatalf("no key message for %q", name)
	}
	m, _ = update(m, msg)
	return m
}

func TestJobsWithNoneSaysSo(t *testing.T) {
	m := sizedModel(t, 80, 24)
	updated, cmd := m.openJobs()
	m = updated.(Model)
	if cmd != nil || len(m.drawers) != 0 || m.message != "no background jobs" {
		t.Fatalf("drawers = %d, status = %q", len(m.drawers), m.message)
	}
}

func TestJobsListsJobsAndKillsOnASecondPress(t *testing.T) {
	m, runtime := jobsModel(t)
	updated, _ := m.openJobs()
	m = updated.(Model)
	got := screen(m)
	for _, want := range []string{" jobs", "⏎ open · ⇧K kill · esc close"} {
		if !strings.Contains(got, want) {
			t.Fatalf("list missing %q:\n%s", want, got)
		}
	}
	// Running jobs come first, under their own heading, then finished ones.
	var rows []string
	for _, line := range strings.Split(got, "\n") {
		if i := strings.LastIndex(line, "│"); i >= 0 && strings.TrimSpace(line[i:]) != "│" {
			rows = append(rows, strings.TrimSpace(line[i+len("│"):]))
		}
	}
	want := []string{"jobs", "running", "2  job       ● running  npm run dev", "finished", "1  job       ✓ done     make test", "⏎ open · ⇧K kill · esc close"}
	if strings.Join(rows, "\n") != strings.Join(want, "\n") {
		t.Fatalf("rows:\n%s\nwant:\n%s", strings.Join(rows, "\n"), strings.Join(want, "\n"))
	}
	// A finished job has nothing to kill, and moving to it skips the
	// heading between.
	m = sendKey(t, m, "j")
	if got := screen(m); strings.Contains(got, "⇧K kill") {
		t.Fatalf("kill offered for a finished job:\n%s", got)
	}
	m = sendKey(t, m, "k")
	m = sendKey(t, m, "K")
	if len(runtime.killed) != 0 || !strings.Contains(screen(m), "press ⇧K again to stop job 2") {
		t.Fatalf("killed = %v after one press:\n%s", runtime.killed, screen(m))
	}
	// Any other key disarms, so a later single press only asks again.
	m = sendKey(t, m, "down")
	m = sendKey(t, m, "up")
	m = sendKey(t, m, "K")
	if len(runtime.killed) != 0 {
		t.Fatalf("a disarmed kill ran: %v", runtime.killed)
	}
	sendKey(t, m, "K")
	if len(runtime.killed) != 1 || runtime.killed[0] != 2 {
		t.Fatalf("killed = %v after two presses", runtime.killed)
	}
}

func TestJobDrawerFollowsOutputLikeATerminal(t *testing.T) {
	m, runtime := jobsModel(t)
	job := runtime.jobList[0]
	writeOutput(t, job, "starting\r\nbuild\t10%\rbuild\t90%\npart")
	updated, _ := m.openJobs()
	m = updated.(Model)
	m = sendKey(t, m, "enter")
	if len(m.drawers) != 2 {
		t.Fatalf("enter opened %d drawers", len(m.drawers))
	}
	m = pollNow(t, m)
	got := screen(m)
	for _, want := range []string{" job 2 · ● running", "$ npm run dev", "starting", "build   90%", "part", "⇧K kill · esc back"} {
		if !strings.Contains(got, want) {
			t.Fatalf("job drawer missing %q:\n%s", want, got)
		}
	}
	if strings.Contains(got, "10%") {
		t.Fatalf("a redrawn line kept what it overwrote:\n%s", got)
	}
	// The unfinished line is completed by the next read, not repeated.
	writeOutput(t, job, "ial line\ndone\n")
	runtime.jobList[0].Exit = "0"
	m = pollNow(t, m)
	got = screen(m)
	if !strings.Contains(got, "partial line") || !strings.Contains(got, "done") || strings.Count(got, "part") != 1 {
		t.Fatalf("job drawer after more output:\n%s", got)
	}
	if !strings.Contains(got, " job 2 · ✓ done") || strings.Contains(got, "⇧K kill") {
		t.Fatalf("finished job still shows as running:\n%s", got)
	}
	m = sendKey(t, m, "esc")
	if len(m.drawers) != 1 || m.jobsView == nil || m.jobsView.watch != nil {
		t.Fatal("esc did not go back to the list")
	}
	m = sendKey(t, m, "esc")
	if len(m.drawers) != 0 || m.jobsView != nil {
		t.Fatal("esc did not close the list")
	}
}

func TestJobDrawerKeepsTheLastLinesOfLongOutput(t *testing.T) {
	m, runtime := jobsModel(t)
	var out strings.Builder
	for i := range jobScrollback + 10 {
		out.WriteString("line " + strconv.Itoa(i) + "\n")
	}
	writeOutput(t, runtime.jobList[1], out.String())
	updated, _ := m.openJobs()
	m = updated.(Model)
	m = sendKey(t, m, "j")
	m = sendKey(t, m, "enter")
	m = pollNow(t, m)
	w := m.jobsView.watch
	if len(w.lines) != jobScrollback || w.lines[0] != "line 10" {
		t.Fatalf("kept %d lines from %q", len(w.lines), w.lines[0])
	}
	m = sendKey(t, m, "home")
	if got := screen(m); !strings.Contains(got, "earlier output in") {
		t.Fatalf("no pointer to the cut output:\n%s", got)
	}
}

func TestJobDrawerFollowsASubagentsSession(t *testing.T) {
	m, runtime := jobsModel(t)
	store, err := session.New(t.TempDir(), "/tmp", "test", "system prompt")
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { store.Close() })
	if _, err := store.AppendMessage(session.TextMessage(session.RoleUser, "audit the parser")); err != nil {
		t.Fatal(err)
	}
	runtime.subagentPath = store.Path()
	runtime.jobList[0].Command = `kon run "audit the parser"`
	runtime.jobList[0].Session = store.ID().String()
	updated, _ := m.openJobs()
	m = updated.(Model)
	m = sendKey(t, m, "enter")
	m = pollNow(t, m)
	if got := screen(m); !strings.Contains(got, " subagent 2 · ● running") || !strings.Contains(got, "audit the parser") {
		t.Fatalf("subagent drawer:\n%s", got)
	}
	if _, err := store.AppendMessage(session.TextMessage(session.RoleAssistant, "found two bugs")); err != nil {
		t.Fatal(err)
	}
	m = pollNow(t, m)
	if got := screen(m); !strings.Contains(got, "found two bugs") {
		t.Fatalf("subagent drawer did not follow the session:\n%s", got)
	}
}

func TestJobsDropAReadForClosedDrawers(t *testing.T) {
	m, _ := jobsModel(t)
	updated, _ := m.openJobs()
	m = updated.(Model)
	msg := m.pollJobs()()
	m = sendKey(t, m, "esc")
	updated, cmd := m.Update(msg)
	if cmd != nil || updated.(Model).jobsView != nil {
		t.Fatal("a read for closed drawers kept polling")
	}
	if _, cmd := m.Update(jobsTickMsg{epoch: msg.(jobsPolledMsg).epoch}); cmd != nil {
		t.Fatal("a tick for closed drawers started a read")
	}
}

func TestClickingARowHighlightsThenOpensIt(t *testing.T) {
	m, _ := jobsModel(t)
	updated, _ := m.openJobs()
	m = updated.(Model)
	body := drawerBody(drawerRect(m.width, m.height, 0))
	// Rows are: running, job 2, finished, job 1. A heading takes no click.
	m, _ = update(m, tea.MouseClickMsg{X: body.x + 2, Y: body.y + 2, Button: tea.MouseLeft})
	if m.jobsView.list.list.index != 1 || len(m.drawers) != 1 {
		t.Fatalf("clicking a heading: index = %d, drawers = %d", m.jobsView.list.list.index, len(m.drawers))
	}
	click := tea.MouseClickMsg{X: body.x + 2, Y: body.y + 3, Button: tea.MouseLeft}
	m, _ = update(m, click)
	if m.jobsView.list.list.index != 3 || len(m.drawers) != 1 {
		t.Fatalf("first click: index = %d, drawers = %d", m.jobsView.list.list.index, len(m.drawers))
	}
	m, _ = update(m, click)
	if len(m.drawers) != 2 || m.jobsView.watch == nil || m.jobsView.watch.job.ID != 1 {
		t.Fatal("a click on the highlighted row did not open it")
	}
	// Esc in the hint row is a button too.
	hints := m.hints(m.topDrawer())
	esc := hints[len(hints)-1]
	body = drawerBody(drawerRect(m.width, m.height, 1))
	m, _ = update(m, tea.MouseClickMsg{X: body.x + esc.x, Y: body.y + body.h, Button: tea.MouseLeft})
	if len(m.drawers) != 1 {
		t.Fatalf("clicking esc back left %d drawers", len(m.drawers))
	}
}

func TestJobStatusSaysHowAJobEnded(t *testing.T) {
	for exit, want := range map[string]struct {
		text  string
		color any
	}{
		"":                               {"● running", colorBarFg},
		"0":                              {"✓ done", colorOK},
		"2":                              {"✗ failed, exit 2", colorFail},
		"killed: stopped by user":        {"■ stopped by you", colorWarn},
		"killed: kon exited":             {"■ stopped when kon exited", colorWarn},
		"lost: kon exited while running": {"■ lost when kon exited", colorWarn},
		"signal: terminated":             {"✗ ended by signal (terminated)", colorFail},
		"exec: no such file":             {"✗ exec: no such file", colorFail},
	} {
		text, color := jobStatus(tools.Job{Exit: exit})
		if text != want.text || color != want.color {
			t.Errorf("jobStatus(%q) = %q, %v; want %q, %v", exit, text, color, want.text, want.color)
		}
	}
}
