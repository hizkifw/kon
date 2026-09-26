//go:build !windows

package tools

import (
	"bytes"
	"context"
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

func readJobFile(t *testing.T, dir string, id, name string) string {
	t.Helper()
	data, err := os.ReadFile(filepath.Join(dir, id, name))
	if err != nil {
		t.Fatal(err)
	}
	return strings.TrimSpace(string(data))
}

func TestJobRecordsExitAndNotifies(t *testing.T) {
	dir := filepath.Join(t.TempDir(), "s.jsonl.jobs")
	notices := make(chan string, 1)
	jobs := NewJobs(dir, "ses_x", func(n string) { notices <- n })
	defer jobs.Close()
	id, _, err := jobs.Start("echo hello; echo $KON_SESSION; exit 3", t.TempDir())
	if err != nil || id != 1 {
		t.Fatalf("Start = %d, %v", id, err)
	}
	select {
	case notice := <-notices:
		if !strings.HasPrefix(notice, "[kon notice] job 1 exited with code 3") || !strings.Contains(notice, "last lines:\nhello\nses_x") {
			t.Fatalf("notice = %q", notice)
		}
	case <-time.After(5 * time.Second):
		t.Fatal("no notice for a finished job")
	}
	if got := readJobFile(t, dir, "1", "exit"); got != "3" {
		t.Fatalf("exit = %q", got)
	}
	if got := readJobFile(t, dir, "1", "cmd"); !strings.HasPrefix(got, "echo hello") {
		t.Fatalf("cmd = %q", got)
	}
	if jobs.Running() != 0 {
		t.Fatalf("running = %d after exit", jobs.Running())
	}
}

func TestCloseKillsRunningJobsWithoutNotice(t *testing.T) {
	dir := t.TempDir()
	notified := false
	jobs := NewJobs(dir, "ses_x", func(string) { notified = true })
	if _, _, err := jobs.Start("sleep 30", t.TempDir()); err != nil {
		t.Fatal(err)
	}
	if jobs.Running() != 1 {
		t.Fatalf("running = %d", jobs.Running())
	}
	jobs.Close()
	if got := readJobFile(t, dir, "1", "exit"); got != "killed: kon exited" || notified {
		t.Fatalf("exit = %q, notified = %v", got, notified)
	}
	if _, _, err := jobs.Start("true", t.TempDir()); err == nil {
		t.Fatal("a closed supervisor started a job")
	}
}

func TestReopenedJobsMarkLostAndContinueNumbering(t *testing.T) {
	dir := t.TempDir()
	for _, id := range []string{"1", "4"} {
		if err := os.MkdirAll(filepath.Join(dir, id), 0o700); err != nil {
			t.Fatal(err)
		}
	}
	if err := os.WriteFile(filepath.Join(dir, "1", "exit"), []byte("0\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	jobs := NewJobs(dir, "ses_x", nil)
	defer jobs.Close()
	if got := readJobFile(t, dir, "1", "exit"); got != "0" {
		t.Fatalf("finished job rewritten: %q", got)
	}
	if got := readJobFile(t, dir, "4", "exit"); !strings.HasPrefix(got, "lost") {
		t.Fatalf("orphaned job exit = %q", got)
	}
	if id, _, err := jobs.Start("true", t.TempDir()); err != nil || id != 5 {
		t.Fatalf("next job = %d, %v", id, err)
	}
}

func TestShellBackgroundStartsJobAndSharesEnv(t *testing.T) {
	dir := t.TempDir()
	jobsDir := filepath.Join(dir, "jobs")
	jobs := NewJobs(jobsDir, "ses_x", nil)
	defer jobs.Close()
	executor := New(dir, false, jobs)
	result, failed := executor.Execute(context.Background(), "shell", raw(map[string]any{"command": "sleep 5", "timeout": 0}), nil)
	if failed || !strings.HasPrefix(result.Content, "background job 1 started") || !strings.Contains(result.Content, filepath.Join(jobsDir, "1", "output")) {
		t.Fatalf("background result = %q (failed %v)", result.Content, failed)
	}
	var details shellDetails
	if err := json.Unmarshal(result.Details, &details); err != nil || details.Job != 1 {
		t.Fatalf("details = %s", result.Details)
	}
	if d := (&shellTool{}).Describe(raw(map[string]any{"command": "sleep 5", "timeout": 0}), result.Content, false, result.Details, dir); d.Note != "background job 1" {
		t.Fatalf("display = %#v", d)
	}
	result, failed = executor.Execute(context.Background(), "shell", raw(map[string]any{"command": "ls $KON_JOBS", "timeout": 5}), nil)
	if failed || !strings.HasPrefix(result.Content, "1\n") {
		t.Fatalf("foreground shell cannot see the jobs: %q", result.Content)
	}
}

func TestShellBackgroundNeedsJobs(t *testing.T) {
	result, failed := New(t.TempDir(), false, nil).Execute(context.Background(), "shell", raw(map[string]any{"command": "true", "timeout": 0}), nil)
	if !failed || !strings.Contains(result.Content, "not available") {
		t.Fatalf("result = %q (failed %v)", result.Content, failed)
	}
}

func TestCappedWriterMarksTruncation(t *testing.T) {
	var out bytes.Buffer
	w := &cappedWriter{w: &out, limit: 4}
	for _, chunk := range []string{"abc", "def", "ghi"} {
		if n, err := w.Write([]byte(chunk)); n != len(chunk) || err != nil {
			t.Fatalf("Write = %d, %v", n, err)
		}
	}
	if got := out.String(); !strings.HasPrefix(got, "abcd\n[kon: output truncated") || strings.Count(got, "truncated") != 1 {
		t.Fatalf("output = %q", got)
	}
}

func TestJobsExportDepthAndJobDirectory(t *testing.T) {
	t.Setenv("KON_DEPTH", "1")
	dir := t.TempDir()
	exited := make(chan string, 1)
	jobs := NewJobs(dir, "ses_x", func(n string) { exited <- n })
	defer jobs.Close()
	if env := strings.Join(jobs.Env(), " "); !strings.Contains(env, "KON_DEPTH=2") {
		t.Fatalf("env = %s", env)
	}
	if _, _, err := jobs.Start(`printf '%s %s' "$KON_DEPTH" "$KON_JOB" > "$KON_JOB/session"`, t.TempDir()); err != nil {
		t.Fatal(err)
	}
	select {
	case <-exited:
	case <-time.After(5 * time.Second):
		t.Fatal("job did not exit")
	}
	list := jobs.List()
	if len(list) != 1 || list[0].Session != "2 "+filepath.Join(dir, "1") || list[0].Exit != "0" {
		t.Fatalf("jobs = %#v", list)
	}
	if err := jobs.Kill(1); err == nil {
		t.Fatal("killed a job that is not running")
	}
}

func TestUserKillIsNamedInTheNotice(t *testing.T) {
	dir := t.TempDir()
	notices := make(chan string, 1)
	jobs := NewJobs(dir, "ses_x", func(n string) { notices <- n })
	defer jobs.Close()
	id, _, err := jobs.Start("sleep 30", t.TempDir())
	if err != nil {
		t.Fatal(err)
	}
	if err := jobs.Kill(id); err != nil {
		t.Fatal(err)
	}
	select {
	case notice := <-notices:
		if !strings.HasPrefix(notice, "[kon notice] job 1 stopped by user after") {
			t.Fatalf("notice = %q", notice)
		}
	case <-time.After(5 * time.Second):
		t.Fatal("no notice for a killed job")
	}
	if got := readJobFile(t, dir, "1", "exit"); got != "killed: stopped by user" {
		t.Fatalf("exit = %q", got)
	}
}
