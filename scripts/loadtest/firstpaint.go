//go:build unix

package main

import (
	"bytes"
	"cmp"
	"fmt"
	"os"
	"os/exec"
	"regexp"
	"runtime"
	"slices"
	"strings"
	"syscall"
	"time"
)

// promptMarker is the input placeholder from internal/ui. It is drawn with
// the rest of the first frame, so seeing it means the frame reached the
// terminal.
const promptMarker = "Ask kon"

// escapeSeq matches the terminal control sequences kon writes: CSI, OSC,
// the string sequences (DCS, SOS, PM, APC), and two-byte escapes. Removing
// them leaves the frame's visible text.
var escapeSeq = regexp.MustCompile(`\x1b(\[[0-?]*[ -/]*[@-~]|\][^\x07\x1b]*(\x07|\x1b\\)|[PX^_][^\x1b]*\x1b\\|.)`)

// runFirstPaint launches the full-screen UI the given number of times, each
// on a fresh pseudo-terminal, and times each launch from exec to the first
// frame. Reading the pty directly, rather than through tmux, keeps a
// multiplexer's redraw out of the number. It reports the median launch.
func runFirstPaint(kon string, env []string, dir string, launches int) (result, error) {
	env = plainTerminal(env)
	var walls, cpus []time.Duration
	var rsss []int64
	for range launches {
		r, err := paintOnce(kon, env, dir)
		if err != nil {
			return result{}, err
		}
		walls, cpus, rsss = append(walls, r.wall), append(cpus, r.cpu), append(rsss, r.maxRSS)
	}
	slices.Sort(walls)
	return result{
		wall:   median(walls),
		cpu:    median(cpus),
		maxRSS: median(rsss),
		detail: fmt.Sprintf("median of %d; min %s p90 %s", launches,
			walls[0].Round(100*time.Microsecond), walls[len(walls)*9/10].Round(100*time.Microsecond)),
	}, nil
}

// paintOnce starts kon as a terminal would, waits for its first frame, and
// kills it there, so the child's rusage covers exactly the work before the
// frame. kon's locks are flocks, which the kernel releases with the process.
func paintOnce(kon string, env []string, dir string) (result, error) {
	master, slave, err := openPTY(160, 50)
	if err != nil {
		return result{}, err
	}
	defer master.Close()
	cmd := exec.Command(kon)
	cmd.Env, cmd.Dir = env, dir
	cmd.Stdin, cmd.Stdout, cmd.Stderr = slave, slave, slave
	cmd.SysProcAttr = &syscall.SysProcAttr{Setsid: true, Setctty: true}
	start := time.Now()
	err = cmd.Start()
	slave.Close()
	if err != nil {
		return result{}, err
	}
	wall, err := awaitPrompt(master, start)
	cmd.Process.Kill()
	cmd.Wait()
	if err != nil {
		return result{}, err
	}
	maxRSS := cmd.ProcessState.SysUsage().(*syscall.Rusage).Maxrss
	if runtime.GOOS == "linux" {
		maxRSS <<= 10
	}
	return result{
		wall:   wall,
		cpu:    cmd.ProcessState.UserTime() + cmd.ProcessState.SystemTime(),
		maxRSS: maxRSS,
	}, nil
}

// awaitPrompt reads the pty until the prompt appears and returns how long
// after start the read that completed it returned.
func awaitPrompt(master *os.File, start time.Time) (time.Duration, error) {
	if err := master.SetReadDeadline(start.Add(10 * time.Second)); err != nil {
		return 0, err
	}
	var out []byte
	buf := make([]byte, 64<<10)
	for {
		n, err := master.Read(buf)
		elapsed := time.Since(start)
		if err != nil {
			return 0, fmt.Errorf("no prompt on screen: %w: %q", err, out)
		}
		out = append(out, buf[:n]...)
		if bytes.Contains(escapeSeq.ReplaceAll(out, nil), []byte(promptMarker)) {
			return elapsed, nil
		}
	}
}

// plainTerminal sets a common TERM and leaves tmux out: inside tmux, color
// detection runs `tmux info` before the first frame, which would make the
// number depend on where the load test was started.
func plainTerminal(env []string) []string {
	env = slices.DeleteFunc(slices.Clone(env), func(kv string) bool {
		return strings.HasPrefix(kv, "TMUX=") || strings.HasPrefix(kv, "TERM=")
	})
	return append(env, "TERM=xterm-256color")
}

func median[T cmp.Ordered](xs []T) T {
	xs = slices.Clone(xs)
	slices.Sort(xs)
	return xs[len(xs)/2]
}
