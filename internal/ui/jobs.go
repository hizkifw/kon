package ui

import (
	"bytes"
	"fmt"
	"image/color"
	"strconv"
	"strings"
	"time"

	tea "charm.land/bubbletea/v2"
	"github.com/charmbracelet/x/ansi"

	"github.com/hizkifw/kon/core/session"
	"github.com/hizkifw/kon/core/typedid"
	"github.com/hizkifw/kon/internal/codetools"
	"github.com/hizkifw/kon/internal/tui"
)

// jobScrollback is how many lines of a command's output its drawer keeps.
// Every change rewraps them all, so the cap bounds that work; the output
// file keeps the rest.
const jobScrollback = 5000

// jobOutputWindow is how much of a command's output a drawer reads at most
// at once: enough for jobScrollback lines of ordinary width, without reading
// all of a large file only to keep its end.
const jobOutputWindow = 2 << 20

// jobsInterval is how often the /jobs drawers read the job files.
const jobsInterval = 500 * time.Millisecond

// jobStatus says how a job stands, marked so it reads at a glance, with the
// color to show it in: a clean exit in the success color, one the user or
// kon stopped in the warning color, and a failure in the danger color.
func jobStatus(job codetools.Job) (string, color.Color) {
	exit := job.Exit
	switch {
	case exit == "":
		return "● running", colorBarFg
	case exit == "0":
		return "✓ done", colorOK
	case strings.Trim(exit, "0123456789") == "":
		return "✗ failed, exit " + exit, colorFail
	case exit == "killed: stopped by user":
		return "■ stopped by you", colorWarn
	case exit == "killed: kon exited":
		return "■ stopped when kon exited", colorWarn
	case strings.HasPrefix(exit, "lost: "):
		return "■ lost when kon exited", colorWarn
	case strings.HasPrefix(exit, "signal: "):
		// Something other than kon ended it, such as a kill from another
		// terminal or a crash.
		return "✗ ended by signal (" + strings.TrimPrefix(exit, "signal: ") + ")", colorFail
	}
	return "✗ " + exit, colorFail
}

// jobKind distinguishes a subagent from a plain command in listings.
func jobKind(job codetools.Job) string {
	if job.Session != "" {
		return "subagent"
	}
	return "job"
}

// jobTitle heads a job's drawer or preview.
func jobTitle(job codetools.Job) string {
	status, _ := jobStatus(job)
	return jobKind(job) + " " + strconv.Itoa(job.ID) + " · " + status
}

// stopJob stops job id and says so in the status line.
func (m *Model) stopJob(id int) {
	if err := m.runtime.KillJob(id); err != nil {
		m.say(toneDanger, err.Error())
		return
	}
	m.say(toneSuccess, "stopped job "+strconv.Itoa(id))
}

// jobsView is the /jobs drawers: the list of jobs, and the job opened from
// it on top. Both follow the job files while they are open.
type jobsView struct {
	// epoch tells this view's polls apart from those of one closed since.
	epoch int
	list  *drawer
	jobs  []codetools.Job
	watch *jobWatch
	// polling marks a read of the job files in flight; there is never more
	// than one.
	polling bool
}

// jobWatch is one job's drawer. It shows a subagent's own conversation, once
// its session can be read, and otherwise the command and its output.
type jobWatch struct {
	job        codetools.Job
	drawer     *drawer
	transcript transcript
	reader     jobReader
	// read marks that a poll has read this job at least once.
	read bool
	// lines is the output kept, at most jobScrollback complete lines, and
	// partial the line still being written. cut marks that earlier lines
	// were left out.
	lines   []string
	partial []byte
	cut     bool
	// replay carries a subagent's conversation from one poll to the next.
	replay replayState
}

// jobReader is where a job's reading stands. Only the poll in flight uses
// it, and the update loop takes it back with the poll's result.
type jobReader struct {
	offset int64
	// view follows a subagent's session once it is open; tried marks that
	// opening it was attempted, which scans the workspace's sessions, so it
	// is not repeated.
	view  *session.View
	tried bool
}

// jobRead is what one poll read of a job.
type jobRead struct {
	reader jobReader
	output []byte
	// skipped marks output left out before this read's bytes.
	skipped bool
	entries []session.Entry
	// opened marks that the subagent's session was opened by this read, so
	// entries hold all of it.
	opened bool
	err    error
}

type jobsTickMsg struct{ epoch int }

type jobsPolledMsg struct {
	epoch int
	jobs  []codetools.Job
	// watch is the job drawer this read was for, nil when none was open.
	watch *jobWatch
	read  jobRead
}

func jobsTick(epoch int) tea.Cmd {
	return tea.Tick(jobsInterval, func(time.Time) tea.Msg { return jobsTickMsg{epoch: epoch} })
}

// openJobs runs /jobs: it opens the list of the session's jobs in a drawer.
func (m Model) openJobs() (tea.Model, tea.Cmd) {
	m.input.Reset()
	jobs := m.runtime.Jobs()
	if len(jobs) == 0 {
		m.message = "no background jobs"
		return m, nil
	}
	m.jobsEpoch++
	v := &jobsView{epoch: m.jobsEpoch, jobs: jobs}
	v.list = &drawer{title: "jobs", list: &menu{}, actions: v.listActions, onClose: func(m *Model) { m.jobsView = nil }}
	v.fillList()
	m.jobsView = v
	m.openDrawer(v.list)
	return m, jobsTick(v.epoch)
}

// fillList lists the running jobs, then the finished ones, each newest
// first, keeping the same job highlighted as they change.
func (v *jobsView) fillList() {
	selected := v.selected()
	idWidth := len(strconv.Itoa(v.jobs[0].ID))
	statusWidth := 0
	for _, job := range v.jobs {
		status, _ := jobStatus(job)
		statusWidth = max(statusWidth, ansi.StringWidth(status))
	}
	var items []menuItem
	index := -1
	section := func(heading string, running bool) {
		first := true
		for _, job := range v.jobs {
			if (job.Exit == "") != running {
				continue
			}
			if first {
				items = append(items, menuItem{Label: heading, Heading: true})
				first = false
			}
			status, tint := jobStatus(job)
			if job.ID == selected.ID || index < 0 {
				index = len(items)
			}
			items = append(items, menuItem{
				Value:       strconv.Itoa(job.ID),
				Label:       fmt.Sprintf("%*d  %-8s", idWidth, job.ID, jobKind(job)),
				Badge:       status + strings.Repeat(" ", statusWidth-ansi.StringWidth(status)),
				BadgeColor:  tint,
				Description: oneLine(job.Command),
			})
		}
	}
	section("running", true)
	section("finished", false)
	v.list.list.items, v.list.list.index = items, index
}

// selected is the highlighted job, or the zero job when there is none.
func (v *jobsView) selected() codetools.Job {
	if list := v.list.list; list.index < len(list.items) {
		for _, job := range v.jobs {
			if strconv.Itoa(job.ID) == list.items[list.index].Value {
				return job
			}
		}
	}
	return codetools.Job{}
}

func (v *jobsView) listActions(*Model) []drawerAction {
	job := v.selected()
	if job.ID == 0 {
		return nil
	}
	actions := []drawerAction{{key: "enter", hint: "⏎", label: "open", run: func(m *Model) tea.Cmd {
		return m.openJob(job)
	}}}
	return append(actions, killAction(job)...)
}

// killAction offers to stop job while it runs.
func killAction(job codetools.Job) []drawerAction {
	if job.Exit != "" {
		return nil
	}
	return []drawerAction{{
		key: "K", hint: "⇧K", label: "kill",
		confirm: "press ⇧K again to stop " + jobKind(job) + " " + strconv.Itoa(job.ID),
		run: func(m *Model) tea.Cmd {
			m.stopJob(job.ID)
			return nil
		},
	}}
}

// openJob opens job's drawer over the list. It fills on the next read,
// which starts now unless one is already in flight: that one's answer then
// reads the job at once.
func (m *Model) openJob(job codetools.Job) tea.Cmd {
	v := m.jobsView
	w := &jobWatch{job: job, transcript: transcript{cwd: m.cwd}}
	w.drawer = &drawer{title: jobTitle(job), transcript: &w.transcript, onClose: func(m *Model) {
		if m.jobsView != nil {
			m.jobsView.watch = nil
		}
	}}
	w.drawer.actions = func(*Model) []drawerAction { return killAction(w.job) }
	w.showOutput()
	v.watch = w
	m.openDrawer(w.drawer)
	if v.polling {
		return nil
	}
	// The tick already scheduled carries the old epoch and is dropped.
	m.jobsEpoch++
	v.epoch = m.jobsEpoch
	return m.pollJobs()
}

// pollJobs reads the job files off the update loop.
func (m *Model) pollJobs() tea.Cmd {
	v := m.jobsView
	v.polling = true
	runtime, epoch, w := m.runtime, v.epoch, v.watch
	var reader jobReader
	var id int
	if w != nil {
		reader, id = w.reader, w.job.ID
	}
	return func() tea.Msg {
		msg := jobsPolledMsg{epoch: epoch, jobs: runtime.Jobs(), watch: w}
		if w != nil {
			for _, job := range msg.jobs {
				if job.ID == id {
					msg.read = readJob(runtime, job, reader)
				}
			}
		}
		return msg
	}
}

// readJob reads what job gained since reader last read it: its subagent's
// session once there is one to open, and its output until then.
func readJob(runtime Runtime, job codetools.Job, r jobReader) jobRead {
	if r.view == nil && !r.tried && job.Session != "" {
		r.tried = true
		if id, err := typedid.ParseSessionID(job.Session); err == nil {
			if view, err := runtime.OpenSubagent(id); err == nil {
				r.view = view
				return jobRead{reader: r, entries: view.ActivePath(), opened: true}
			}
		}
	}
	if r.view != nil {
		entries, err := r.view.Poll()
		return jobRead{reader: r, entries: entries, err: err}
	}
	data, start, err := job.ReadOutput(r.offset, jobOutputWindow)
	read := jobRead{output: data, skipped: start > r.offset, err: err}
	r.offset = start + int64(len(data))
	read.reader = r
	return read
}

// tickJobs starts the next read, unless the drawers have closed since.
func (m Model) tickJobs(msg jobsTickMsg) (tea.Model, tea.Cmd) {
	if m.jobsView == nil || msg.epoch != m.jobsView.epoch {
		return m, nil
	}
	return m, m.pollJobs()
}

// applyJobsPolled shows what a read found and schedules the next one.
func (m Model) applyJobsPolled(msg jobsPolledMsg) (tea.Model, tea.Cmd) {
	v := m.jobsView
	if v == nil || msg.epoch != v.epoch {
		return m, nil
	}
	v.polling = false
	if len(msg.jobs) > 0 {
		v.jobs = msg.jobs
		v.fillList()
	}
	if w := v.watch; w != nil && w == msg.watch {
		for _, job := range msg.jobs {
			if job.ID == w.job.ID {
				w.job = job
			}
		}
		w.drawer.title = jobTitle(w.job)
		m.applyJobRead(w, msg.read)
	}
	m.refreshDrawers()
	// A job opened while this read was in flight is read at once.
	if w := v.watch; w != nil && !w.read {
		return m, m.pollJobs()
	}
	return m, jobsTick(v.epoch)
}

func (m *Model) applyJobRead(w *jobWatch, read jobRead) {
	w.read = true
	w.reader = read.reader
	switch {
	case read.opened:
		w.transcript.reset()
		w.transcript.add(block{kind: blockContext, text: "$ " + oneLine(w.job.Command)})
		m.replay(&w.transcript, &w.replay, read.entries)
	case w.reader.view != nil:
		m.replay(&w.transcript, &w.replay, read.entries)
	case len(read.output) > 0:
		w.appendOutput(read.output, read.skipped)
		w.showOutput()
	}
	if read.err != nil {
		w.transcript.add(block{kind: blockError, text: tui.Sanitize(read.err.Error())})
	}
}

// appendOutput keeps the complete lines of data, and the line it leaves
// unfinished for the next read to complete.
func (w *jobWatch) appendOutput(data []byte, skipped bool) {
	if skipped {
		w.lines, w.partial, w.cut = nil, nil, true
	}
	buf := append(w.partial, data...)
	end := bytes.LastIndexByte(buf, '\n')
	for _, line := range bytes.Split(buf[:end+1], []byte("\n")) {
		w.lines = append(w.lines, terminalLine(line))
	}
	// Split leaves an empty piece after the final newline, which is the
	// start of the partial line rather than a line of its own.
	w.lines = w.lines[:len(w.lines)-1]
	w.partial = append([]byte(nil), buf[end+1:]...)
	if extra := len(w.lines) - jobScrollback; extra > 0 {
		w.lines = append([]string(nil), w.lines[extra:]...)
		w.cut = true
	}
}

// terminalLine is a line of output as a terminal would leave it: a carriage
// return starts the line over, as a progress bar redrawing itself does, so
// only what was written after the last one shows.
func terminalLine(line []byte) string {
	s := strings.TrimSuffix(string(line), "\r")
	s = s[strings.LastIndexByte(s, '\r')+1:]
	return tui.ExpandTabs(tui.Sanitize(s))
}

// showOutput rebuilds the drawer from the command and its kept output.
func (w *jobWatch) showOutput() {
	w.transcript.reset()
	if w.cut {
		w.transcript.add(block{kind: blockContext, text: "earlier output in " + abbreviateHome(w.job.Output)})
	}
	w.transcript.add(block{kind: blockContext, text: "$ " + oneLine(w.job.Command)})
	lines := w.lines
	if len(w.partial) > 0 {
		lines = append(lines[:len(lines):len(lines)], terminalLine(w.partial))
	}
	if len(lines) > 0 {
		w.transcript.add(block{kind: blockOutput, text: strings.Join(lines, "\n")})
	}
}
