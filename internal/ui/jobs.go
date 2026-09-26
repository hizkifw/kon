package ui

import (
	"strconv"
	"strings"

	tea "charm.land/bubbletea/v2"

	"github.com/hizkifw/kon/internal/tools"
	"github.com/hizkifw/kon/internal/typedid"
)

// jobTailLines is how much of a job's output /jobs shows.
const jobTailLines = 20

// jobStatus names a job's state for a picker row or a heading.
func jobStatus(job tools.Job) string {
	switch {
	case job.Exit == "":
		return "running"
	case strings.Trim(job.Exit, "0123456789") == "":
		return "exit " + job.Exit
	}
	reason, _, _ := strings.Cut(job.Exit, ":")
	return reason
}

// jobKind distinguishes a subagent from a plain command in listings.
func jobKind(job tools.Job) string {
	if job.Session != "" {
		return "subagent"
	}
	return "job"
}

// completeJobs offers the session's jobs, newest first, each previewing its
// output, or a subagent's own conversation, when highlighted. With running
// set it offers only jobs that are still running, for /kill.
func completeJobs(running bool) func(Model, string) []menuItem {
	return func(m Model, prefix string) []menuItem {
		var items []menuItem
		for _, job := range m.runtime.Jobs() {
			id := strconv.Itoa(job.ID)
			if !strings.HasPrefix(id, prefix) || (running && job.Exit != "") {
				continue
			}
			items = append(items, menuItem{
				Value:       id,
				Label:       id + " " + jobKind(job) + " · " + jobStatus(job),
				Description: oneLine(job.Command),
				Preview:     previewJob(m, job),
			})
		}
		return items
	}
}

// previewJob builds a job's preview lazily, when its row is highlighted.
func previewJob(m Model, job tools.Job) func() *transcript {
	return func() *transcript {
		preview := &transcript{}
		preview.add(block{kind: blockContext, text: jobKind(job) + " " + strconv.Itoa(job.ID) + " · " + jobStatus(job) + " · Esc to cancel"})
		if id, err := typedid.ParseSessionID(job.Session); err == nil {
			if entries, err := m.runtime.SubagentPreview(id, previewTurns); err == nil && len(entries) > 0 {
				m.applyHistoryTo(preview, entries)
				return preview
			}
		}
		preview.add(jobOutputBlock(job))
		return preview
	}
}

// jobOutputBlock is a job's command and the tail of its output.
func jobOutputBlock(job tools.Job) block {
	text := "$ " + oneLine(job.Command)
	if tail := sanitize(job.Tail(jobTailLines)); tail != "" {
		text += "\n" + tail
	}
	return block{kind: blockModels, text: text}
}

// listJobs runs /jobs: bare, it opens the picker; with an ID it shows that job's
// recent output in the transcript.
func (m Model) listJobs(args []string) (tea.Model, tea.Cmd) {
	all := m.runtime.Jobs()
	if len(args) == 0 {
		if len(all) == 0 {
			m.input.Reset()
			m.status = "no background jobs"
			return m, nil
		}
		m.input.SetValue("/jobs ")
		m.input.CursorEnd()
		m.refreshInput()
		return m, nil
	}
	m.input.Reset()
	for _, job := range all {
		if strconv.Itoa(job.ID) == args[0] {
			m.transcript.add(block{kind: blockContext, text: jobKind(job) + " " + args[0] + " · " + jobStatus(job)})
			m.transcript.add(jobOutputBlock(job))
			m.status = "output: " + abbreviateHome(job.Output)
			m.refreshTranscript(true)
			return m, nil
		}
	}
	m.status = "no job " + args[0]
	return m, nil
}

// killJob runs /kill: it stops a running job, whose exit the agent still
// hears about.
func (m Model) killJob(id string) (tea.Model, tea.Cmd) {
	m.input.Reset()
	n, err := strconv.Atoi(id)
	if err != nil {
		m.status = "no job " + id
		return m, nil
	}
	if err := m.runtime.KillJob(n); err != nil {
		m.status = err.Error()
		return m, nil
	}
	m.status = "stopped job " + id
	return m, nil
}
