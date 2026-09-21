package tools

import (
	"context"
	"encoding/json"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"testing"
	"time"
)

func TestWriteEditRead(t *testing.T) {
	dir := t.TempDir()
	executor := New(dir)
	result, failed := executor.Execute(context.Background(), "write", raw(map[string]any{"path": "note.txt", "content": "alpha\nbeta\n"}))
	if failed || !strings.Contains(result, "wrote") {
		t.Fatalf("write = %q, failed=%v", result, failed)
	}
	_, failed = executor.Execute(context.Background(), "edit", raw(map[string]any{"path": "note.txt", "old_text": "beta", "new_text": "gamma"}))
	if failed {
		t.Fatal("edit failed")
	}
	result, failed = executor.Execute(context.Background(), "read", raw(map[string]any{"path": "note.txt", "offset": 2, "limit": 1}))
	if failed || !strings.Contains(result, "gamma") || strings.Contains(result, "alpha") {
		t.Fatalf("read = %q, failed=%v", result, failed)
	}
	b, err := os.ReadFile(filepath.Join(dir, "note.txt"))
	if err != nil || string(b) != "alpha\ngamma\n" {
		t.Fatalf("file = %q, err=%v", b, err)
	}
}

func TestEditRejectsAmbiguousAndUnknownArguments(t *testing.T) {
	dir := t.TempDir()
	if err := os.WriteFile(filepath.Join(dir, "x"), []byte("same same"), 0o644); err != nil {
		t.Fatal(err)
	}
	executor := New(dir)
	if _, failed := executor.Execute(context.Background(), "edit", raw(map[string]any{"path": "x", "old_text": "same", "new_text": "x"})); !failed {
		t.Fatal("ambiguous edit succeeded")
	}
	if _, failed := executor.Execute(context.Background(), "read", json.RawMessage(`{"path":"x","surprise":true}`)); !failed {
		t.Fatal("unknown argument succeeded")
	}
}

func TestShellCapturesExitCode(t *testing.T) {
	command := "printf hello"
	if runtime.GOOS == "windows" {
		command = "echo hello"
	}
	result, failed := New(t.TempDir()).Execute(context.Background(), "shell", raw(map[string]any{"command": command, "timeout": 10}))
	if failed || !strings.Contains(result, "hello") || !strings.Contains(result, "exit code: 0") {
		t.Fatalf("shell = %q, failed=%v", result, failed)
	}
	if !strings.Contains(result, "(took ") {
		t.Fatalf("shell result is missing the wall-clock duration: %q", result)
	}
}

func TestShellRequiresTimeout(t *testing.T) {
	executor := New(t.TempDir())
	cases := map[string]json.RawMessage{
		"missing":   json.RawMessage(`{"command":"true"}`),
		"zero":      raw(map[string]any{"command": "true", "timeout": 0}),
		"negative":  raw(map[string]any{"command": "true", "timeout": -5}),
		"too large": raw(map[string]any{"command": "true", "timeout": int(maxShellTimeout/time.Second) + 1}),
	}
	for name, args := range cases {
		result, failed := executor.Execute(context.Background(), "shell", args)
		if !failed {
			t.Fatalf("shell without a usable timeout (%s) succeeded: %q", name, result)
		}
		if !strings.Contains(result, "timeout") {
			t.Fatalf("shell (%s) error does not mention the timeout: %q", name, result)
		}
	}
}

func TestShellTimesOutWithPartialOutput(t *testing.T) {
	command := "printf before; sleep 5"
	if runtime.GOOS == "windows" {
		command = "echo | set /p=before& timeout /t 5 >nul"
	}
	result, failed := New(t.TempDir()).Execute(context.Background(), "shell", raw(map[string]any{"command": command, "timeout": 1}))
	if !failed {
		t.Fatal("timed-out command reported success")
	}
	if !strings.Contains(result, "timed out after 1s") {
		t.Fatalf("timeout error does not name the budget: %q", result)
	}
	if runtime.GOOS == "windows" {
		// The Windows one-liner above is too brittle to promise output from.
		return
	}
	if !strings.Contains(result, "before") {
		t.Fatalf("timed-out command lost its partial output: %q", result)
	}
}

func TestShellCancelInterruptsCommand(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("test relies on POSIX signal delivery")
	}
	dir := t.TempDir()
	executor := New(dir)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	var failed bool
	done := make(chan struct{})
	go func() {
		defer close(done)
		// A foreground child (not `sleep … &`: async children have SIGINT
		// ignored by POSIX) proves the interrupt reaches the process group.
		_, failed = executor.Execute(ctx, "shell", raw(map[string]any{
			"command": `trap 'echo handled > interrupt.txt' INT; sleep 31415`,
			"timeout": 600,
		}))
	}()
	time.Sleep(300 * time.Millisecond)
	cancel()
	select {
	case <-done:
	case <-time.After(10 * time.Second):
		t.Fatal("shell tool did not return after cancellation")
	}
	if !failed {
		t.Fatal("cancelled command did not report an error")
	}
	if _, err := os.Stat(filepath.Join(dir, "interrupt.txt")); err != nil {
		t.Fatalf("cancelled command did not receive the interrupt: %v", err)
	}
	// The interrupt must reach the shell's children too.
	if out, err := exec.Command("pgrep", "-f", "sleep 31415").Output(); err == nil && len(out) > 0 {
		t.Fatalf("cancelled command left a running child: %s", out)
	}
}

func TestShellKillsCommandThatIgnoresInterrupt(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("test relies on POSIX signal delivery")
	}
	grace := shellInterruptGrace
	shellInterruptGrace = 300 * time.Millisecond
	defer func() { shellInterruptGrace = grace }()
	executor := New(t.TempDir())
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	done := make(chan struct{})
	go func() {
		defer close(done)
		executor.Execute(ctx, "shell", raw(map[string]any{
			"command": `trap '' INT; while :; do :; done`,
			"timeout": 600,
		}))
	}()
	time.Sleep(300 * time.Millisecond)
	start := time.Now()
	cancel()
	select {
	case <-done:
	case <-time.After(10 * time.Second):
		t.Fatal("shell tool did not return after the interrupt grace")
	}
	if elapsed := time.Since(start); elapsed > 5*time.Second {
		t.Fatalf("kill did not follow the interrupt grace: %v", elapsed)
	}
}

func TestKillShellForceKillsRunningCommand(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("test relies on POSIX signal delivery")
	}
	executor := New(t.TempDir())
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	if executor.KillShell() {
		t.Fatal("KillShell reported a command while idle")
	}
	done := make(chan struct{})
	go func() {
		defer close(done)
		executor.Execute(ctx, "shell", raw(map[string]any{"command": "sleep 30", "timeout": 600}))
	}()
	time.Sleep(300 * time.Millisecond)
	if !executor.KillShell() {
		t.Fatal("KillShell did not find the running command")
	}
	select {
	case <-done:
	case <-time.After(10 * time.Second):
		t.Fatal("shell tool did not return after the force kill")
	}
	if executor.KillShell() {
		t.Fatal("KillShell reported a command after it exited")
	}
}

func TestShellReturnsWhenGrandchildHoldsOutput(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("test relies on POSIX signal delivery")
	}
	executor := New(t.TempDir())
	start := time.Now()
	// The backgrounded sleep inherits the output descriptor and ignores
	// SIGINT; it must not stall the result or lose the exit status.
	result, failed := executor.Execute(context.Background(), "shell", raw(map[string]any{
		"command": `sleep 2 & echo done`,
		"timeout": 600,
	}))
	elapsed := time.Since(start)
	if failed || !strings.Contains(result, "done") || !strings.Contains(result, "exit code: 0") {
		t.Fatalf("command with an orphaned grandchild = %q, failed=%v", result, failed)
	}
	if elapsed >= 2*time.Second {
		t.Fatalf("a grandchild holding the output pipe stalled the result: %v", elapsed)
	}
}

func raw(value any) json.RawMessage {
	b, _ := json.Marshal(value)
	return b
}
