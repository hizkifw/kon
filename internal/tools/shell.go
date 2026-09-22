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

// shellTool runs one shell command in the workspace with a mandatory timeout.
type shellTool struct {
	mu      sync.Mutex
	running *exec.Cmd // command currently running, if any
}

func (t *shellTool) Definition() provider.Tool {
	return provider.Tool{
		Name:        "shell",
		Description: "Run a shell command in the current working directory. Every command must specify a timeout in whole seconds (1-600); the command is killed when the timeout expires.",
		Parameters:  json.RawMessage(`{"type":"object","properties":{"command":{"type":"string"},"timeout":{"type":"integer","minimum":1,"maximum":600,"description":"maximum wall-clock seconds the command may run"}},"required":["command","timeout"],"additionalProperties":false}`),
	}
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
	start := time.Now()
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
	return Result{Content: fmt.Sprintf("%sexit code: %d (took %s)", output, exitCode, elapsed)}, nil
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
