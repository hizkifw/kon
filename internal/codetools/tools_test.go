package codetools

import (
	"context"
	"encoding/json"
	"errors"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"sync"
	"testing"
	"time"

	"github.com/hizkifw/kon/core/tool"
)

func TestWriteEditRead(t *testing.T) {
	dir := t.TempDir()
	executor := newExecutor(dir, false, nil)
	result, failed := executor.Execute(context.Background(), "write", raw(map[string]any{"path": "note.txt", "content": "alpha\nbeta\n"}), nil)
	if failed {
		t.Fatalf("write = %q, failed", result.Content)
	}
	_, failed = executor.Execute(context.Background(), "edit", raw(map[string]any{"path": "note.txt", "old_text": "beta", "new_text": "gamma"}), nil)
	if failed {
		t.Fatal("edit failed")
	}
	result, failed = executor.Execute(context.Background(), "read", raw(map[string]any{"path": "note.txt", "offset": 2, "limit": 1}), nil)
	if failed || !strings.Contains(result.Content, "gamma") || strings.Contains(result.Content, "alpha") {
		t.Fatalf("read = %q, failed=%v", result.Content, failed)
	}
	b, err := os.ReadFile(filepath.Join(dir, "note.txt"))
	if err != nil || string(b) != "alpha\ngamma\n" {
		t.Fatalf("file = %q, err=%v", b, err)
	}
}

// TestWriteEditKeepFileMode guards executable scripts: replacing a file's
// content must not drop its execute bits.
func TestWriteEditKeepFileMode(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Windows files have no POSIX permission bits")
	}
	dir := t.TempDir()
	path := filepath.Join(dir, "build.sh")
	if err := os.WriteFile(path, []byte("#!/bin/sh\necho old\n"), 0o755); err != nil {
		t.Fatal(err)
	}
	// Set the mode explicitly so the umask cannot weaken the starting point.
	if err := os.Chmod(path, 0o755); err != nil {
		t.Fatal(err)
	}
	executor := newExecutor(dir, false, nil)
	for _, call := range []struct {
		tool string
		args map[string]any
	}{
		{"edit", map[string]any{"path": "build.sh", "old_text": "old", "new_text": "new"}},
		{"write", map[string]any{"path": "build.sh", "content": "#!/bin/sh\necho newer\n"}},
	} {
		if result, failed := executor.Execute(context.Background(), call.tool, raw(call.args), nil); failed {
			t.Fatalf("%s = %q", call.tool, result.Content)
		}
		info, err := os.Stat(path)
		if err != nil {
			t.Fatal(err)
		}
		if got, want := info.Mode().Perm(), os.FileMode(0o755); got != want {
			t.Fatalf("%s left the file mode %v, want %v", call.tool, got, want)
		}
	}
}

func TestEditRejectsAmbiguousAndUnknownArguments(t *testing.T) {
	dir := t.TempDir()
	if err := os.WriteFile(filepath.Join(dir, "x"), []byte("same same"), 0o644); err != nil {
		t.Fatal(err)
	}
	executor := newExecutor(dir, false, nil)
	if _, failed := executor.Execute(context.Background(), "edit", raw(map[string]any{"path": "x", "old_text": "same", "new_text": "x"}), nil); !failed {
		t.Fatal("ambiguous edit succeeded")
	}
	if _, failed := executor.Execute(context.Background(), "read", json.RawMessage(`{"path":"x","surprise":true}`), nil); !failed {
		t.Fatal("unknown argument succeeded")
	}
}

func TestShellCapturesExitCode(t *testing.T) {
	command := "printf hello"
	if runtime.GOOS == "windows" {
		command = "echo hello"
	}
	result, failed := newExecutor(t.TempDir(), false, nil).Execute(context.Background(), "shell", raw(map[string]any{"command": command, "timeout": 10}), nil)
	if failed || !strings.Contains(result.Content, "hello") || !strings.Contains(result.Content, "exit code: 0") {
		t.Fatalf("shell = %q, failed=%v", result.Content, failed)
	}
	if !strings.Contains(result.Content, "(took ") {
		t.Fatalf("shell result is missing the wall-clock duration: %q", result.Content)
	}
	// A nonzero exit is a failed call, and the code survives both in the text
	// the model reads and in the details the transcript replays.
	command = "echo failing; exit 3"
	if runtime.GOOS == "windows" && shellName() == "cmd.exe" {
		command = "echo failing& exit 3"
	}
	result, failed = newExecutor(t.TempDir(), false, nil).Execute(context.Background(), "shell", raw(map[string]any{"command": command, "timeout": 10}), nil)
	if !failed || !result.IsError || !strings.Contains(result.Content, "failing") || !strings.Contains(result.Content, "exit code: 3") {
		t.Fatalf("failing shell = %q, failed=%v, IsError=%v", result.Content, failed, result.IsError)
	}
	var details shellDetails
	if err := json.Unmarshal(result.Details, &details); err != nil || details.ExitCode == nil || *details.ExitCode != 3 {
		t.Fatalf("failing shell details = %s, err=%v; want exit_code 3", result.Details, err)
	}
}

func TestShellRequiresTimeout(t *testing.T) {
	executor := newExecutor(t.TempDir(), false, nil)
	cases := map[string]json.RawMessage{
		// A missing timeout must not quietly become a background job.
		"missing":   json.RawMessage(`{"command":"true"}`),
		"negative":  raw(map[string]any{"command": "true", "timeout": -5}),
		"too large": raw(map[string]any{"command": "true", "timeout": int(maxShellTimeout/time.Second) + 1}),
	}
	for name, args := range cases {
		result, failed := executor.Execute(context.Background(), "shell", args, nil)
		if !failed {
			t.Fatalf("shell without a usable timeout (%s) succeeded: %q", name, result.Content)
		}
		if !strings.Contains(result.Content, "timeout") {
			t.Fatalf("shell (%s) error does not mention the timeout: %q", name, result.Content)
		}
	}
}

func TestShellTimesOutWithPartialOutput(t *testing.T) {
	command := "printf before; sleep 5"
	if runtime.GOOS == "windows" && shellName() == "cmd.exe" {
		command = "echo | set /p=before& timeout /t 5 >nul"
	}
	result, failed := newExecutor(t.TempDir(), false, nil).Execute(context.Background(), "shell", raw(map[string]any{"command": command, "timeout": 1}), nil)
	if !failed {
		t.Fatal("timed-out command reported success")
	}
	if !strings.Contains(result.Content, "timed out after 1s") {
		t.Fatalf("timeout error does not name the budget: %q", result.Content)
	}
	if runtime.GOOS == "windows" && shellName() == "cmd.exe" {
		// The cmd.exe one-liner above is too brittle to promise output from.
		return
	}
	if !strings.Contains(result.Content, "before") {
		t.Fatalf("timed-out command lost its partial output: %q", result.Content)
	}
}

func TestShellCancelInterruptsCommand(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("test relies on POSIX signal delivery")
	}
	dir := t.TempDir()
	executor := newExecutor(dir, false, nil)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	var failed bool
	done := make(chan struct{})
	go func() {
		defer close(done)
		// A foreground child (not `sleep … &`: async children have SIGINT
		// ignored by POSIX) proves the interrupt reaches the process group.
		// The subshell reports ready only after the trap is set, then becomes
		// the sleep, so no interrupt can land before either is in place.
		_, failed = executor.Execute(ctx, "shell", raw(map[string]any{
			"command": `trap 'echo handled > interrupt.txt' INT; (: > ready; exec sleep 31415)`,
			"timeout": 600,
		}), nil)
	}()
	waitUntil(t, "the command to start", fileExists(filepath.Join(dir, "ready")))
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
	if out := processesMatching(t, "sleep 31415"); out != "" {
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
	dir := t.TempDir()
	executor := newExecutor(dir, false, nil)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	// A failure would otherwise leave the loop spinning after the test exits.
	t.Cleanup(func() { _ = exec.Command("pkill", "-KILL", "-f", "kon-ignores-int").Run() })
	done := make(chan struct{})
	go func() {
		defer close(done)
		// A child of the shell ignores the interrupt and loops. WaitDelay
		// kills only the shell itself, so the child ends only if the grace
		// period kill reaches the whole process group. The trailing exit keeps
		// the shell from replacing itself with the child.
		executor.Execute(ctx, "shell", raw(map[string]any{
			"command": `sh -c 'trap "" INT; : > ready; while :; do :; done' kon-ignores-int; exit`,
			"timeout": 600,
		}), nil)
	}()
	waitUntil(t, "the command to start", fileExists(filepath.Join(dir, "ready")))
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
	waitUntil(t, "the grace period kill to end the child that ignores the interrupt", func() bool {
		return processesMatching(t, "kon-ignores-int") == ""
	})
}

func TestKillEscalationForceKillsRunningCommand(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("test relies on POSIX signal delivery")
	}
	dir := t.TempDir()
	executor := newExecutor(dir, false, nil)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	if executor.Interrupt(2) {
		t.Fatal("kill escalation reported a command while idle")
	}
	// A failure would otherwise leave the loop running after the test exits.
	t.Cleanup(func() { _ = exec.Command("pkill", "-KILL", "-f", "kon-survives-interrupt").Run() })
	done := make(chan struct{})
	go func() {
		defer close(done)
		// The command survives every interrupt, noting each one it catches, so
		// only a force kill can end it.
		executor.Execute(ctx, "shell", raw(map[string]any{
			"command": `trap ': > interrupted' INT; : > ready; while :; do sleep 1; done # kon-survives-interrupt`,
			"timeout": 600,
		}), nil)
	}()
	waitUntil(t, "the command to start", fileExists(filepath.Join(dir, "ready")))
	// Run tracks the command only once Start returns, which can trail the
	// shell's ready file, so the first press may briefly find nothing.
	waitUntil(t, "the interrupt to find the running command", func() bool { return executor.Interrupt(1) })
	waitUntil(t, "the command to catch the interrupt", fileExists(filepath.Join(dir, "interrupted")))
	select {
	case <-done:
		t.Fatal("the first interrupt ended a command that handles it")
	default:
	}
	if !executor.Interrupt(2) {
		t.Fatal("kill escalation did not find the running command")
	}
	select {
	case <-done:
	case <-time.After(10 * time.Second):
		t.Fatal("shell tool did not return after the force kill")
	}
	if executor.Interrupt(2) {
		t.Fatal("kill escalation reported a command after it exited")
	}
}

func TestShellReportsTickingProgressWhileRunning(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("test relies on POSIX shell timing")
	}
	executor := newExecutor(t.TempDir(), false, nil)
	var mu sync.Mutex
	var snapshots []Display
	report := func(snapshot any) {
		d := snapshot.(Display)
		if d.State != StateRunning || d.Status == "" {
			return
		}
		mu.Lock()
		snapshots = append(snapshots, d)
		mu.Unlock()
	}
	// A command that outlives several live-display ticks.
	result, failed := executor.Execute(context.Background(), "shell", raw(map[string]any{
		"command": "echo start; sleep 1",
		"timeout": 5,
	}), report)
	if failed || !strings.Contains(result.Content, "exit code: 0") {
		t.Fatalf("shell = %q, failed=%v", result.Content, failed)
	}
	mu.Lock()
	defer mu.Unlock()
	if len(snapshots) < 2 {
		t.Fatalf("expected repeated progress snapshots, got %+v", snapshots)
	}
	// Every snapshot renders elapsed over the 5s budget, and the clock advances.
	for _, d := range snapshots {
		if !strings.HasSuffix(d.Status, " / 5s") {
			t.Fatalf("progress status is missing the timeout budget: %q", d.Status)
		}
	}
	if first, last := snapshots[0].Status, snapshots[len(snapshots)-1].Status; first == last {
		t.Fatalf("progress clock did not advance: %q to %q", first, last)
	}
	// Each snapshot shows the command and its output so far: nothing before
	// the echo lands, and the echoed line once it has.
	sawOutput := false
	for _, d := range snapshots {
		if d.Summary != "echo start; sleep 1" {
			t.Fatalf("running snapshot summary = %q", d.Summary)
		}
		switch {
		case len(d.Lines) == 0:
		case len(d.Lines) == 1 && d.Lines[0] == "start":
			sawOutput = true
		default:
			t.Fatalf("running snapshot lines = %q, want the output so far", d.Lines)
		}
	}
	if !sawOutput {
		t.Fatalf("no running snapshot showed the command's output; last = %+v", snapshots[len(snapshots)-1])
	}
	// The finished result carries the exit-code status instead of the clock.
	shell := &shellTool{}
	done := shell.Describe(raw(map[string]any{"command": "echo start; sleep 1"}), result.Content, false, nil, "/tmp")
	if !strings.HasPrefix(done.Status, "exit 0 · took ") {
		t.Fatalf("finished status = %q", done.Status)
	}
}

func TestShellReturnsWhenGrandchildHoldsOutput(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("test relies on POSIX signal delivery")
	}
	executor := newExecutor(t.TempDir(), false, nil)
	start := time.Now()
	// The backgrounded sleep inherits the output descriptor and ignores
	// SIGINT; it must not stall the result or lose the exit status.
	result, failed := executor.Execute(context.Background(), "shell", raw(map[string]any{
		"command": `sleep 2 & echo done`,
		"timeout": 600,
	}), nil)
	elapsed := time.Since(start)
	if failed || !strings.Contains(result.Content, "done") || !strings.Contains(result.Content, "exit code: 0") {
		t.Fatalf("command with an orphaned grandchild = %q, failed=%v", result.Content, failed)
	}
	if elapsed >= 2*time.Second {
		t.Fatalf("a grandchild holding the output pipe stalled the result: %v", elapsed)
	}
}

// waitUntil polls cond until it holds, failing after a generous timeout. The
// signal tests synchronize on what a command has done, never on how long it
// ought to have taken.
func waitUntil(t *testing.T, what string, cond func() bool) {
	t.Helper()
	deadline := time.Now().Add(10 * time.Second)
	for !cond() {
		if time.Now().After(deadline) {
			t.Fatalf("timed out waiting for %s", what)
		}
		time.Sleep(5 * time.Millisecond)
	}
}

func fileExists(path string) func() bool {
	return func() bool {
		_, err := os.Stat(path)
		return err == nil
	}
}

// processesMatching lists the processes whose command line contains pattern.
// Without pgrep the test skips, rather than passing a check it never made.
func processesMatching(t *testing.T, pattern string) string {
	t.Helper()
	pgrep, err := exec.LookPath("pgrep")
	if err != nil {
		t.Skip("pgrep is not installed, so surviving processes cannot be checked")
	}
	out, err := exec.Command(pgrep, "-f", pattern).Output()
	var exit *exec.ExitError
	if errors.As(err, &exit) && exit.ExitCode() == 1 {
		// pgrep exits 1 when nothing matches.
		return ""
	}
	if err != nil {
		t.Fatalf("pgrep: %v", err)
	}
	return string(out)
}

func raw(value any) json.RawMessage {
	b, _ := json.Marshal(value)
	return b
}

// newExecutor runs kon's built-in tools the way a session does.
func newExecutor(cwd string, vision bool, jobs *Jobs) *tool.Executor {
	return tool.NewExecutor(Registry(jobs), cwd, vision)
}
