// Package tools contains kon's four deliberately small coding tools.
package tools

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"sync"
	"time"
	"unicode/utf8"

	"github.com/hizkifw/kon/internal/provider"
)

const (
	maxReadBytes   = 1024 * 1024
	maxReadLines   = 2000
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

type Executor struct {
	cwd string

	mu      sync.Mutex
	running *exec.Cmd // command currently running in the shell tool, if any
}

func New(cwd string) *Executor { return &Executor{cwd: cwd} }

func Definitions() []provider.Tool {
	return []provider.Tool{
		{
			Name: "read", Description: "Read a UTF-8 text file with one-based line offsets.",
			Parameters: json.RawMessage(`{"type":"object","properties":{"path":{"type":"string"},"offset":{"type":"integer","minimum":1},"limit":{"type":"integer","minimum":1,"maximum":2000}},"required":["path"],"additionalProperties":false}`),
		},
		{
			Name: "write", Description: "Create or replace a text file. Parent directories must already exist.",
			Parameters: json.RawMessage(`{"type":"object","properties":{"path":{"type":"string"},"content":{"type":"string"}},"required":["path","content"],"additionalProperties":false}`),
		},
		{
			Name: "edit", Description: "Replace exactly one occurrence of old_text in a text file.",
			Parameters: json.RawMessage(`{"type":"object","properties":{"path":{"type":"string"},"old_text":{"type":"string"},"new_text":{"type":"string"}},"required":["path","old_text","new_text"],"additionalProperties":false}`),
		},
		{
			Name:        "shell",
			Description: "Run a shell command in the current working directory. Every command must specify a timeout in whole seconds (1-600); the command is killed when the timeout expires.",
			Parameters:  json.RawMessage(`{"type":"object","properties":{"command":{"type":"string"},"timeout":{"type":"integer","minimum":1,"maximum":600,"description":"maximum wall-clock seconds the command may run"}},"required":["command","timeout"],"additionalProperties":false}`),
		},
	}
}

func (e *Executor) Execute(ctx context.Context, name string, arguments json.RawMessage) (string, bool) {
	var result string
	var err error
	switch name {
	case "read":
		result, err = e.read(arguments)
	case "write":
		result, err = e.write(arguments)
	case "edit":
		result, err = e.edit(arguments)
	case "shell":
		result, err = e.shell(ctx, arguments)
	default:
		err = fmt.Errorf("unknown tool %q", name)
	}
	if err != nil {
		return "error: " + err.Error(), true
	}
	return result, false
}

func decodeArgs(raw json.RawMessage, dst any) error {
	dec := json.NewDecoder(bytes.NewReader(raw))
	dec.DisallowUnknownFields()
	if err := dec.Decode(dst); err != nil {
		return fmt.Errorf("invalid arguments: %w", err)
	}
	var extra any
	if err := dec.Decode(&extra); !errors.Is(err, io.EOF) {
		if err == nil {
			return errors.New("invalid arguments: multiple JSON values")
		}
		return fmt.Errorf("invalid arguments: %w", err)
	}
	return nil
}

func (e *Executor) path(path string) (string, error) {
	if path == "" {
		return "", errors.New("path must not be empty")
	}
	if !filepath.IsAbs(path) {
		path = filepath.Join(e.cwd, path)
	}
	return filepath.Clean(path), nil
}

func (e *Executor) read(raw json.RawMessage) (string, error) {
	args := struct {
		Path   string `json:"path"`
		Offset int    `json:"offset"`
		Limit  int    `json:"limit"`
	}{Offset: 1, Limit: maxReadLines}
	if err := decodeArgs(raw, &args); err != nil {
		return "", err
	}
	if args.Offset < 1 || args.Limit < 1 || args.Limit > maxReadLines {
		return "", errors.New("offset must be at least 1 and limit must be between 1 and 2000")
	}
	path, err := e.path(args.Path)
	if err != nil {
		return "", err
	}
	b, err := os.ReadFile(path)
	if err != nil {
		return "", err
	}
	if len(b) > maxReadBytes {
		return "", fmt.Errorf("file is larger than %d bytes", maxReadBytes)
	}
	if bytes.IndexByte(b, 0) >= 0 || !utf8.Valid(b) {
		return "", errors.New("file is not UTF-8 text")
	}
	lines := strings.Split(string(b), "\n")
	start := args.Offset - 1
	if start >= len(lines) {
		return fmt.Sprintf("(offset %d is beyond end of file; %d lines)", args.Offset, len(lines)), nil
	}
	end := min(start+args.Limit, len(lines))
	var out strings.Builder
	for i := start; i < end; i++ {
		fmt.Fprintf(&out, "%6d  %s", i+1, lines[i])
		if i+1 < end {
			out.WriteByte('\n')
		}
	}
	if end < len(lines) {
		fmt.Fprintf(&out, "\n… %d more lines", len(lines)-end)
	}
	return out.String(), nil
}

func (e *Executor) write(raw json.RawMessage) (string, error) {
	var args struct {
		Path    string `json:"path"`
		Content string `json:"content"`
	}
	if err := decodeArgs(raw, &args); err != nil {
		return "", err
	}
	path, err := e.path(args.Path)
	if err != nil {
		return "", err
	}
	mode := os.FileMode(0o644)
	if info, statErr := os.Stat(path); statErr == nil {
		mode = info.Mode().Perm()
	} else if !errors.Is(statErr, os.ErrNotExist) {
		return "", statErr
	}
	if err := atomicWrite(path, []byte(args.Content), mode); err != nil {
		return "", err
	}
	return fmt.Sprintf("wrote %d bytes to %s", len(args.Content), path), nil
}

func (e *Executor) edit(raw json.RawMessage) (string, error) {
	var args struct {
		Path    string `json:"path"`
		OldText string `json:"old_text"`
		NewText string `json:"new_text"`
	}
	if err := decodeArgs(raw, &args); err != nil {
		return "", err
	}
	if args.OldText == "" {
		return "", errors.New("old_text must not be empty")
	}
	path, err := e.path(args.Path)
	if err != nil {
		return "", err
	}
	b, err := os.ReadFile(path)
	if err != nil {
		return "", err
	}
	count := bytes.Count(b, []byte(args.OldText))
	if count != 1 {
		return "", fmt.Errorf("old_text must occur exactly once; found %d occurrences", count)
	}
	info, err := os.Stat(path)
	if err != nil {
		return "", err
	}
	updated := bytes.Replace(b, []byte(args.OldText), []byte(args.NewText), 1)
	if err := atomicWrite(path, updated, info.Mode().Perm()); err != nil {
		return "", err
	}
	return fmt.Sprintf("edited %s", path), nil
}

func atomicWrite(path string, content []byte, mode os.FileMode) error {
	tmp, err := os.CreateTemp(filepath.Dir(path), ".kon-*")
	if err != nil {
		return err
	}
	tmpPath := tmp.Name()
	cleanup := func() {
		tmp.Close()
		_ = os.Remove(tmpPath)
	}
	if err := tmp.Chmod(mode); err != nil {
		cleanup()
		return err
	}
	if _, err := tmp.Write(content); err != nil {
		cleanup()
		return err
	}
	if err := tmp.Sync(); err != nil {
		cleanup()
		return err
	}
	if err := tmp.Close(); err != nil {
		_ = os.Remove(tmpPath)
		return err
	}
	if err := os.Rename(tmpPath, path); err != nil {
		_ = os.Remove(tmpPath)
		return err
	}
	return nil
}

func (e *Executor) shell(ctx context.Context, raw json.RawMessage) (string, error) {
	var args struct {
		Command string `json:"command"`
		Timeout int    `json:"timeout"`
	}
	if err := decodeArgs(raw, &args); err != nil {
		return "", err
	}
	if strings.TrimSpace(args.Command) == "" {
		return "", errors.New("command must not be empty")
	}
	if args.Timeout <= 0 {
		return "", fmt.Errorf("timeout is required: specify whole seconds between 1 and %d; commands without a timeout are not supported", int(maxShellTimeout/time.Second))
	}
	if time.Duration(args.Timeout)*time.Second > maxShellTimeout {
		return "", fmt.Errorf("timeout must be at most %d seconds", int(maxShellTimeout/time.Second))
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
	cmd.Dir = e.cwd
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
	// Output is captured through a pipe the executor owns. Handing the child
	// the write end as an *os.File means exec passes the descriptor straight
	// through, without an intermediary copying goroutine: Wait reports the
	// moment the process exits instead of waiting for the pipe to drain. The
	// reader below drains it concurrently; a backgrounded grandchild that
	// inherits the descriptor and outlives the command cannot stall the
	// result past shellDrainWindow.
	pr, pw, pipeErr := os.Pipe()
	if pipeErr != nil {
		return "", pipeErr
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
		return "", startErr
	}
	e.trackShell(cmd)
	defer e.trackShell(nil)
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
			return "", fmt.Errorf("timed out after %s\n%s", timeout, output)
		}
		return "", fmt.Errorf("cancelled after %s\n%s", elapsed, output)
	}
	exitCode := 0
	if err != nil {
		var exitErr *exec.ExitError
		if !errors.As(err, &exitErr) {
			return "", err
		}
		exitCode = exitErr.ExitCode()
	}
	return fmt.Sprintf("%sexit code: %d (took %s)", output, exitCode, elapsed), nil
}

// normalizeShellOutput ensures captured output ends with a newline so a
// trailing marker (exit code, error reason) starts on its own line.
func normalizeShellOutput(output string) string {
	if output != "" && !strings.HasSuffix(output, "\n") {
		return output + "\n"
	}
	return output
}

// trackShell records or clears the command currently running in the shell
// tool so KillShell can escalate an ignored interrupt to a kill.
func (e *Executor) trackShell(cmd *exec.Cmd) {
	e.mu.Lock()
	defer e.mu.Unlock()
	e.running = cmd
}

// KillShell force-kills the command currently running in the shell tool,
// together with everything it spawned, and reports whether a command was
// killed. It is the second, harder press of Ctrl+C for a command that
// ignores the interrupt.
func (e *Executor) KillShell() bool {
	e.mu.Lock()
	cmd := e.running
	e.mu.Unlock()
	if cmd == nil {
		return false
	}
	return killProcess(cmd) == nil
}

type headTailWriter struct {
	mu    sync.Mutex
	limit int
	total int
	all   bytes.Buffer
	head  []byte
	tail  []byte
}

func (w *headTailWriter) Write(p []byte) (int, error) {
	w.mu.Lock()
	defer w.mu.Unlock()
	n := len(p)
	w.total += n
	if w.all.Len()+n <= w.limit {
		_, _ = w.all.Write(p)
		return n, nil
	}
	half := w.limit / 2
	if w.head == nil {
		combined := append(w.all.Bytes(), p...)
		w.head = append([]byte(nil), combined[:min(half, len(combined))]...)
		w.tail = append([]byte(nil), combined[max(0, len(combined)-half):]...)
		w.all.Reset()
	} else {
		w.tail = append(w.tail, p...)
		if len(w.tail) > half {
			w.tail = append([]byte(nil), w.tail[len(w.tail)-half:]...)
		}
	}
	return n, nil
}

func (w *headTailWriter) String() string {
	w.mu.Lock()
	defer w.mu.Unlock()
	if w.head == nil {
		return w.all.String()
	}
	omitted := w.total - len(w.head) - len(w.tail)
	return string(w.head) + fmt.Sprintf("\n… %d bytes omitted …\n", omitted) + string(w.tail)
}

var _ io.Writer = (*headTailWriter)(nil)
