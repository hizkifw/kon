package tools

import (
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"slices"
	"strconv"
	"strings"
	"sync"
	"time"
)

// maxJobOutputBytes caps one background job's output file, so a chatty server
// left running cannot fill the disk. Output past the cap is dropped after a
// marker line.
const maxJobOutputBytes = 16 << 20

// jobNoticeLines is how much of a finished job's output its notice quotes, so
// the model can usually react without reading the file first.
const jobNoticeLines = 20

// Jobs supervises the background commands of one session. Each job is a
// directory of plain files, so the agent (and a person in another terminal)
// can list and inspect jobs with ordinary commands instead of a kon API:
//
//	<dir>/<id>/cmd     the command text
//	<dir>/<id>/pid     the process ID, which is also its process group ID
//	<dir>/<id>/output  combined stdout and stderr, capped at maxJobOutputBytes
//	<dir>/<id>/exit    the exit code, or why it stopped; absent while running
//	<dir>/<id>/session the session of a kon run subagent, written by it
//
// A job is started by one kon process and dies with it: Close kills every job
// still running, and a job directory left without an exit file by a kon that
// crashed is marked lost the next time the session is opened.
type Jobs struct {
	dir     string
	session string
	notify  func(string)

	mu      sync.Mutex
	next    int
	running map[int]*exec.Cmd
	// userKilled marks jobs the user stopped, so the notice can say so: a
	// model told only that a job died by a signal assumes it crashed.
	userKilled map[int]bool
	closed     bool
	wg         sync.WaitGroup
}

// NewJobs supervises jobs under dir for the session with the given ID. notify
// receives a notice for the model whenever a job exits on its own; it is
// called from the job's goroutine and must not block.
func NewJobs(dir, session string, notify func(string)) *Jobs {
	j := &Jobs{dir: dir, session: session, notify: notify, next: 1, running: map[int]*exec.Cmd{}, userKilled: map[int]bool{}}
	j.recover()
	return j
}

// recover continues numbering after the jobs a previous kon left behind and
// records that any it left running are gone.
func (j *Jobs) recover() {
	entries, err := os.ReadDir(j.dir)
	if err != nil {
		return
	}
	for _, entry := range entries {
		id, err := strconv.Atoi(entry.Name())
		if err != nil || !entry.IsDir() {
			continue
		}
		j.next = max(j.next, id+1)
		exit := filepath.Join(j.dir, entry.Name(), "exit")
		if _, err := os.Stat(exit); errors.Is(err, os.ErrNotExist) {
			_ = os.WriteFile(exit, []byte("lost: kon exited while running\n"), 0o600)
		}
	}
}

// Env is the environment every shell command runs with, so a command can find
// the jobs directory and a nested `kon run` its parent session and depth.
func (j *Jobs) Env() []string {
	if j == nil {
		return nil
	}
	return []string{"KON_JOBS=" + j.dir, "KON_SESSION=" + j.session, "KON_DEPTH=" + strconv.Itoa(Depth()+1)}
}

// Depth is how many kon agents this process runs beneath: 0 for one started
// by a person, and one more for each kon run started from an agent's shell.
func Depth() int {
	depth, err := strconv.Atoi(os.Getenv("KON_DEPTH"))
	if err != nil || depth < 0 {
		return 0
	}
	return depth
}

// Job is one background job as its files describe it.
type Job struct {
	ID      int
	Command string
	// Exit is the content of the exit file, empty while the job runs.
	Exit   string
	Output string
	// Session is the session of a kon run subagent, when the job is one.
	Session string
}

// List reads every job of the session from its files, newest first.
func (j *Jobs) List() []Job {
	if j == nil {
		return nil
	}
	entries, err := os.ReadDir(j.dir)
	if err != nil {
		return nil
	}
	var jobs []Job
	for _, entry := range entries {
		id, err := strconv.Atoi(entry.Name())
		if err != nil || !entry.IsDir() {
			continue
		}
		dir := filepath.Join(j.dir, entry.Name())
		read := func(name string) string {
			data, _ := os.ReadFile(filepath.Join(dir, name))
			return strings.TrimSpace(string(data))
		}
		jobs = append(jobs, Job{ID: id, Command: read("cmd"), Exit: read("exit"), Output: filepath.Join(dir, "output"), Session: read("session")})
	}
	slices.SortFunc(jobs, func(a, b Job) int { return b.ID - a.ID })
	return jobs
}

// Tail returns up to n trailing lines of a job's output.
func (job Job) Tail(n int) string { return fileTail(job.Output, n) }

// Kill stops a running job and everything it spawned, on the user's behalf.
// The agent still gets the exit notice, which says the user stopped it.
func (j *Jobs) Kill(id int) error {
	j.mu.Lock()
	defer j.mu.Unlock()
	cmd, ok := j.running[id]
	if !ok {
		return fmt.Errorf("job %d is not running", id)
	}
	if err := killProcess(cmd); err != nil {
		return err
	}
	j.userKilled[id] = true
	return nil
}

// Dir is the directory holding one subdirectory per job.
func (j *Jobs) Dir() string { return j.dir }

// Running reports how many jobs are still running.
func (j *Jobs) Running() int {
	if j == nil {
		return 0
	}
	j.mu.Lock()
	defer j.mu.Unlock()
	return len(j.running)
}

// Start runs command in the background through the shell backend and returns
// its job ID and process ID.
func (j *Jobs) Start(command, cwd string) (int, int, error) {
	j.mu.Lock()
	defer j.mu.Unlock()
	if j.closed {
		return 0, 0, errors.New("background jobs are closed")
	}
	id := j.next
	dir := filepath.Join(j.dir, strconv.Itoa(id))
	if err := os.MkdirAll(dir, 0o700); err != nil {
		return 0, 0, fmt.Errorf("create job directory: %w", err)
	}
	if err := os.WriteFile(filepath.Join(dir, "cmd"), []byte(command+"\n"), 0o600); err != nil {
		return 0, 0, err
	}
	output, err := os.OpenFile(filepath.Join(dir, "output"), os.O_CREATE|os.O_WRONLY|os.O_TRUNC, 0o600)
	if err != nil {
		return 0, 0, err
	}
	backend := shellCommand()
	cmd := exec.Command(backend.path, append(backend.args, command)...)
	cmd.Dir = cwd
	// KON_JOB lets a kon run subagent record its session beside the job.
	cmd.Env = append(os.Environ(), append(j.Env(), "KON_JOB="+dir)...)
	configureProcessGroup(cmd)
	// As in the foreground shell, output goes through a pipe the job owns, so
	// the cap applies and a grandchild holding the pipe cannot delay the exit
	// record by more than the drain window.
	pr, pw, err := os.Pipe()
	if err != nil {
		output.Close()
		return 0, 0, err
	}
	cmd.Stdout, cmd.Stderr = pw, pw
	start := time.Now()
	startErr := cmd.Start()
	_ = pw.Close()
	if startErr != nil {
		pr.Close()
		output.Close()
		return 0, 0, startErr
	}
	pid := cmd.Process.Pid
	if err := os.WriteFile(filepath.Join(dir, "pid"), []byte(strconv.Itoa(pid)+"\n"), 0o600); err != nil {
		_ = killProcess(cmd)
	}
	j.next++
	j.running[id] = cmd
	j.wg.Add(1)
	copied := make(chan struct{})
	go func() {
		defer close(copied)
		_, _ = io.Copy(&cappedWriter{w: output, limit: maxJobOutputBytes}, pr)
	}()
	go j.wait(id, command, cmd, start, pr, output, copied)
	return id, pid, nil
}

// wait records a job's exit and, unless kon itself stopped it, tells the model.
func (j *Jobs) wait(id int, command string, cmd *exec.Cmd, start time.Time, pr, output *os.File, copied chan struct{}) {
	defer j.wg.Done()
	err := cmd.Wait()
	elapsed := time.Since(start).Round(time.Second)
	drain := time.AfterFunc(shellDrainWindow, func() { _ = pr.Close() })
	<-copied
	drain.Stop()
	pr.Close()
	output.Close()

	status := "0"
	if err != nil {
		var exitErr *exec.ExitError
		switch {
		case errors.As(err, &exitErr) && exitErr.ExitCode() >= 0:
			status = strconv.Itoa(exitErr.ExitCode())
		default:
			status = err.Error()
		}
	}
	dir := filepath.Join(j.dir, strconv.Itoa(id))
	j.mu.Lock()
	delete(j.running, id)
	closed, userKilled := j.closed, j.userKilled[id]
	delete(j.userKilled, id)
	j.mu.Unlock()
	switch {
	case closed:
		status = "killed: kon exited"
	case userKilled:
		status = "killed: stopped by user"
	}
	_ = os.WriteFile(filepath.Join(dir, "exit"), []byte(status+"\n"), 0o600)
	if closed || j.notify == nil {
		return
	}
	j.notify(jobNotice(id, command, status, elapsed, filepath.Join(dir, "output")))
}

// jobNotice is the message the model receives when a job exits. It is framed
// as coming from kon so the model does not mistake it for the user speaking.
func jobNotice(id int, command, status string, elapsed time.Duration, output string) string {
	var outcome string
	switch {
	case status == "killed: stopped by user":
		outcome = "stopped by user"
	case strings.HasPrefix(status, "signal: "):
		outcome = "ended by " + status
	default:
		if _, err := strconv.Atoi(status); err == nil {
			outcome = "exited with code " + status
		} else {
			outcome = "stopped (" + status + ")"
		}
	}
	var b strings.Builder
	fmt.Fprintf(&b, "[kon notice] job %d %s after %s: %s\noutput: %s", id, outcome, elapsed, strings.ReplaceAll(command, "\n", "; "), output)
	if tail := fileTail(output, jobNoticeLines); tail != "" {
		fmt.Fprintf(&b, "\nlast lines:\n%s", tail)
	}
	return b.String()
}

// fileTail returns up to n trailing lines of the file at path.
func fileTail(path string, n int) string {
	f, err := os.Open(path)
	if err != nil {
		return ""
	}
	defer f.Close()
	const window = 8 << 10
	info, err := f.Stat()
	if err != nil {
		return ""
	}
	offset := max(0, info.Size()-window)
	data := make([]byte, info.Size()-offset)
	if _, err := f.ReadAt(data, offset); err != nil && !errors.Is(err, io.EOF) {
		return ""
	}
	lines, _ := tailLines(strings.TrimRight(string(data), "\n"), n)
	return strings.Join(lines, "\n")
}

// Close kills every running job and waits for their exits to be recorded. No
// notices are sent for them: the session they would report to is closing.
func (j *Jobs) Close() {
	if j == nil {
		return
	}
	j.mu.Lock()
	j.closed = true
	for _, cmd := range j.running {
		_ = killProcess(cmd)
	}
	j.mu.Unlock()
	j.wg.Wait()
}

// cappedWriter writes up to limit bytes, then one marker, then discards the
// rest while still reporting success so the copy keeps draining the pipe.
type cappedWriter struct {
	w       io.Writer
	limit   int
	written int
	capped  bool
}

func (c *cappedWriter) Write(p []byte) (int, error) {
	if room := c.limit - c.written; room > 0 {
		chunk := p[:min(len(p), room)]
		n, err := c.w.Write(chunk)
		c.written += n
		if err != nil {
			return n, err
		}
	}
	if c.written >= c.limit && !c.capped {
		c.capped = true
		_, _ = io.WriteString(c.w, "\n[kon: output truncated, job still running]\n")
	}
	return len(p), nil
}
