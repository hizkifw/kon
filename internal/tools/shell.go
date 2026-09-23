package tools

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"runtime"
	"strings"
	"sync"
	"time"

	"github.com/hizkifw/kon/internal/provider"
)

const (
	maxOutputBytes = 64 * 1024
	// maxShellTimeout caps the timeout the model may request for a shell
	// command. Every command must carry one; unbounded commands are not
	// supported (they will become background commands eventually).
	maxShellTimeout = 600 * time.Second
)

// shellInterruptGrace is how long a shell command may ignore the interrupt
// from a cancellation before it is killed. Tests shorten it.
var shellInterruptGrace = 10 * time.Second

// shellDrainWindow is how long the output reader may keep draining after the
// command's process has exited. It bounds the wait when a backgrounded
// grandchild inherited the output pipe and keeps it open.
var shellDrainWindow = 250 * time.Millisecond

// liveDisplayInterval paces the live display snapshots a running shell call
// publishes. The latest snapshot wins downstream, so this only bounds the
// reporting rate, not the freshness floor.
const liveDisplayInterval = 100 * time.Millisecond

// shellTool runs one shell command in the workspace with a mandatory timeout.
type shellTool struct {
	mu      sync.Mutex
	running *exec.Cmd // command currently running, if any
}

type shellDetails struct {
	ExitCode    *int   `json:"exit_code"`
	Duration    string `json:"duration"`
	OutputBytes int    `json:"output_bytes"`
}

func (t *shellTool) Definition() provider.Tool {
	return provider.Tool{
		Name:        "shell",
		Description: "Run a shell command in the current working directory. Every command must specify a timeout in whole seconds (1-600); the command is killed when the timeout expires.",
		Parameters:  json.RawMessage(`{"type":"object","properties":{"command":{"type":"string"},"timeout":{"type":"integer","minimum":1,"maximum":600,"description":"maximum wall-clock seconds the command may run"}},"required":["command","timeout"],"additionalProperties":false}`),
	}
}

// toolTailLines bounds how many trailing output lines the transcript echoes
// for one call, and bounds the live buffer a running call keeps for its
// streaming display.
const toolTailLines = maxToolLines

// Summarize renders the request line: the command with line breaks collapsed.
func (t *shellTool) Summarize(raw json.RawMessage, cwd string) string {
	args := struct {
		Command string `json:"command"`
	}{}
	if err := json.Unmarshal(raw, &args); err != nil {
		return FallbackSummary(raw)
	}
	return strings.ReplaceAll(args.Command, "\n", "; ")
}

// Describe renders the finished call from persisted details. Older sessions
// without details still use the trailing marker in the model-facing text.
func (t *shellTool) Describe(raw json.RawMessage, result string, failed bool, details json.RawMessage, cwd string) Display {
	summary := ""
	if len(raw) > 0 {
		summary = t.Summarize(raw, cwd)
	}
	var output, exit, took string
	var hasExit bool
	var meta shellDetails
	if len(details) > 0 && json.Unmarshal(details, &meta) == nil && meta.ExitCode != nil && meta.OutputBytes >= 0 && meta.OutputBytes <= len(result) {
		output = strings.TrimRight(result[:meta.OutputBytes], "\n")
		exit, took, hasExit = fmt.Sprint(*meta.ExitCode), meta.Duration, true
	} else {
		output, exit, took, hasExit = splitResult(result)
	}
	status := ""
	if hasExit {
		status = "exit " + exit
		if took != "" {
			status += " · took " + took
		}
	}
	state := StateDone
	if failed {
		state = StateFailed
	} else if hasExit && exit != "0" {
		state = StateFailed
	}
	if state == StateDone && output == "" {
		// Successful calls with nothing to echo carry their outcome on the
		// request line alone.
		return Display{State: state, Summary: summary, Note: status}
	}
	if state == StateFailed {
		// A failure's message is the primary result; the status line rides
		// along on the request line so the reason for failure stays adjacent
		// to the command.
		note := status
		if !hasExit {
			note = "failed"
		}
		lines, more := tailLines(output, toolTailLines)
		return Display{State: state, Summary: summary, Note: note, Lines: lines, More: more}
	}
	lines, more := tailLines(output, toolTailLines)
	return Display{State: state, Summary: summary, Lines: lines, More: more, Status: status, Quiet: true}
}

// liveDisplay builds a running-call snapshot from the writer's tail lines. The
// status line shows elapsed time against the command's timeout so a running
// command visibly ticks and its budget is known; the finished result replaces it
// with the exit-code status. The outcome note is empty: the call has no exit
// code yet.
func (t *shellTool) liveDisplay(raw json.RawMessage, env Env, lines []string, elapsed, timeout time.Duration) Display {
	return Display{
		State:   StateRunning,
		Summary: t.Summarize(raw, env.cwd),
		Lines:   lines,
		Status:  runningStatus(elapsed, timeout),
	}
}

// runningStatus renders a running command's progress line: elapsed time over its
// total budget, e.g. "12.0s / 30s". The ticking clock signals the command is
// alive and how long remains before the timeout. Elapsed always carries one
// decimal so the line reads as a stepping clock rather than jittering per frame.
func runningStatus(elapsed, timeout time.Duration) string {
	return fmt.Sprintf("%.1fs / %s", elapsed.Seconds(), timeout)
}

// splitResult separates a shell result into the output body and the trailing
// "exit code: N (took D)" marker the tool appends. hasExit reports whether a
// well-formed marker was found; without one the whole result is output.
func splitResult(text string) (output, exit, took string, hasExit bool) {
	const marker = "exit code: "
	at := strings.LastIndex(text, marker)
	if at < 0 {
		return text, "", "", false
	}
	rest, tail := text[at+len(marker):], ""
	if open := strings.LastIndex(rest, " (took "); open >= 0 && strings.HasSuffix(rest, ")") {
		rest, tail = rest[:open], rest[open+len(" (took "):len(rest)-1]
	}
	if rest == "" || strings.Trim(rest, "0123456789") != "" {
		return text, "", "", false
	}
	return strings.TrimRight(text[:at], "\n"), rest, tail, true
}

func (t *shellTool) Run(ctx context.Context, env Env, raw json.RawMessage) (Result, error) {
	var args struct {
		Command string `json:"command"`
		Timeout int    `json:"timeout"`
	}
	if err := decodeArgs(raw, &args); err != nil {
		return Result{}, err
	}
	if strings.TrimSpace(args.Command) == "" {
		return Result{}, errors.New("command must not be empty")
	}
	if args.Timeout <= 0 {
		return Result{}, fmt.Errorf("timeout is required: specify whole seconds between 1 and %d; commands without a timeout are not supported", int(maxShellTimeout/time.Second))
	}
	if time.Duration(args.Timeout)*time.Second > maxShellTimeout {
		return Result{}, fmt.Errorf("timeout must be at most %d seconds", int(maxShellTimeout/time.Second))
	}
	timeout := time.Duration(args.Timeout) * time.Second
	ctx, cancel := context.WithTimeout(ctx, timeout)
	defer cancel()
	var cmd *exec.Cmd
	if runtime.GOOS == "windows" {
		shell := os.Getenv("COMSPEC")
		if shell == "" {
			shell = "cmd.exe"
		}
		cmd = exec.CommandContext(ctx, shell, "/d", "/s", "/c", args.Command)
	} else {
		cmd = exec.CommandContext(ctx, "/bin/sh", "-c", args.Command)
	}
	cmd.Dir = env.cwd
	// The command runs in its own process group. Cancelling the context first
	// interrupts the group so the command can stop cleanly; if it ignores the
	// interrupt, it is killed once the grace period passes. WaitDelay is the
	// backstop that unblocks Wait even if a process survives both.
	configureProcessGroup(cmd)
	cmd.Cancel = func() error {
		if err := interruptProcess(cmd); err != nil {
			return err
		}
		time.AfterFunc(shellInterruptGrace, func() { _ = killProcess(cmd) })
		return nil
	}
	cmd.WaitDelay = shellInterruptGrace
	// Output is captured through a pipe the tool owns. Handing the child the
	// write end as an *os.File means exec passes the descriptor straight
	// through, without an intermediary copying goroutine: Wait reports the
	// moment the process exits instead of waiting for the pipe to drain. The
	// reader below drains it concurrently; a backgrounded grandchild that
	// inherits the descriptor and outlives the command cannot stall the
	// result past shellDrainWindow.
	pr, pw, pipeErr := os.Pipe()
	if pipeErr != nil {
		return Result{}, pipeErr
	}
	defer pr.Close()
	writer := &headTailWriter{limit: maxOutputBytes}
	cmd.Stdout, cmd.Stderr = pw, pw
	readerDone := make(chan struct{})
	go func() {
		defer close(readerDone)
		_, _ = io.Copy(writer, pr)
	}()
	// While the command runs, publish the tail of its output as the live
	// display, throttled to a frame-friendly rate. Snapshots are idempotent
	// and the latest wins downstream, so a burst between ticks coalesces. The
	// snapshot carries elapsed time against the timeout so the status line ticks
	// for as long as the command lives.
	start := time.Now()
	if env.report != nil {
		stop := make(chan struct{})
		reporterDone := make(chan struct{})
		go func() {
			defer close(reporterDone)
			ticker := time.NewTicker(liveDisplayInterval)
			defer ticker.Stop()
			for {
				// Exit promptly once stopped, without firing a final tick.
				select {
				case <-stop:
					return
				default:
				}
				select {
				case <-stop:
					return
				case <-ticker.C:
					elapsed := time.Since(start)
					env.Report(t.liveDisplay(raw, env, writer.Tail(toolTailLines), elapsed, timeout))
				}
			}
		}()
		// The reporter must be fully stopped before Run returns: its snapshots
		// flow to the same channel the result event will, so a straggler could
		// otherwise race the result or outlive the run's emit channel.
		defer func() { close(stop); <-reporterDone }()
	}
	startErr := cmd.Start()
	// The child received its own descriptor at fork; the parent must not keep
	// the write end, or the reader never sees EOF for ordinary commands.
	_ = pw.Close()
	if startErr != nil {
		_ = pr.Close()
		<-readerDone
		return Result{}, startErr
	}
	t.track(cmd)
	defer t.track(nil)
	err := cmd.Wait()
	elapsed := time.Since(start).Round(10 * time.Millisecond)
	// Give the reader a moment to pick up whatever the command wrote last,
	// then unblock it even if a grandchild still holds the write end.
	drain := time.AfterFunc(shellDrainWindow, func() { _ = pr.Close() })
	<-readerDone
	drain.Stop()
	output := normalizeShellOutput(writer.String())
	if ctx.Err() != nil {
		// The command did not finish on its own. Surface why, together with
		// whatever output it produced first, so the model can react.
		if errors.Is(ctx.Err(), context.DeadlineExceeded) {
			return Result{}, fmt.Errorf("timed out after %s\n%s", timeout, output)
		}
		return Result{}, fmt.Errorf("cancelled after %s\n%s", elapsed, output)
	}
	exitCode := 0
	if err != nil {
		var exitErr *exec.ExitError
		if !errors.As(err, &exitErr) {
			return Result{}, err
		}
		exitCode = exitErr.ExitCode()
	}
	details, _ := json.Marshal(shellDetails{ExitCode: &exitCode, Duration: elapsed.String(), OutputBytes: len(output)})
	return Result{Content: fmt.Sprintf("%sexit code: %d (took %s)", output, exitCode, elapsed), Details: details, IsError: exitCode != 0}, nil
}

// normalizeShellOutput ensures captured output ends with a newline so a
// trailing marker (exit code, error reason) starts on its own line.
func normalizeShellOutput(output string) string {
	if output != "" && !strings.HasSuffix(output, "\n") {
		return output + "\n"
	}
	return output
}

// Interrupt escalates cancellation of the command currently running in the
// shell tool. attempt 1 interrupts the command's process group: the polite
// Ctrl+C that lets it clean up and exit on its own terms. attempt 2 and above
// force-kill it, for a command that ignored the interrupt. It reports whether
// a command was running to receive the escalation.
func (t *shellTool) Interrupt(attempt int) bool {
	t.mu.Lock()
	cmd := t.running
	t.mu.Unlock()
	if cmd == nil {
		return false
	}
	if attempt <= 1 {
		return interruptProcess(cmd) == nil
	}
	return killProcess(cmd) == nil
}

// track records or clears the command currently running so Interrupt can
// escalate cancellation against it.
func (t *shellTool) track(cmd *exec.Cmd) {
	t.mu.Lock()
	defer t.mu.Unlock()
	t.running = cmd
}
