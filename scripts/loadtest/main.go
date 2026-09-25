//go:build unix

// Command loadtest measures kon's CPU and memory under load. It serves a mock
// OpenAI-compatible model in process and drives a built kon binary through
// fixed scenarios: long streamed replies of different markdown shapes and
// long tool loops. Headless scenarios run `kon run` and read the child's
// rusage. TUI scenarios, enabled with -tui, run kon in a private tmux server
// and sample /proc once a second, so they need Linux and tmux.
//
//	make build && go run ./scripts/loadtest [-tui] [-run regexp]
package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"net/http"
	"net/http/httptest"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"runtime"
	"strconv"
	"strings"
	"sync/atomic"
	"syscall"
	"time"
)

// reply describes what the mock model sends for one run.
type reply struct {
	chunks    int           // text deltas in the final reply
	delay     time.Duration // pause between deltas; zero streams as fast as possible
	burst     int           // deltas sent together before each delay, as a network read often delivers several
	shape     string        // doc, paragraph, or code; see token
	toolTurns int           // turns that call tools before the final reply
	parallel  int           // tool calls per tool turn
	toolCmd   string        // shell command each tool call runs
}

type scenario struct {
	name  string
	tui   bool
	secs  int // TUI sampling window
	reply reply
}

var scenarios = []scenario{
	{name: "startup", reply: reply{chunks: 10}},
	{name: "stream-doc-50k", reply: reply{chunks: 50_000}},
	{name: "stream-doc-200k", reply: reply{chunks: 200_000}},
	{name: "stream-code-50k", reply: reply{chunks: 50_000, shape: "code"}},
	{name: "tools-100", reply: reply{chunks: 100, toolTurns: 100}},
	{name: "tools-25x8", reply: reply{chunks: 100, toolTurns: 25, parallel: 8}},
	{name: "tools-bigout-30", reply: reply{chunks: 100, toolTurns: 30, toolCmd: "seq 1 200000"}},
	// About 100 words a second, a fast real model. CPU should hold flat
	// across the window; a rising trend means per-frame cost grows with the
	// reply.
	{name: "tui-idle", tui: true, secs: 5},
	{name: "tui-doc", tui: true, secs: 15, reply: reply{chunks: 1500, delay: 10 * time.Millisecond}},
	{name: "tui-paragraph", tui: true, secs: 15, reply: reply{chunks: 1500, delay: 10 * time.Millisecond, shape: "paragraph"}},
	{name: "tui-burst", tui: true, secs: 15, reply: reply{chunks: 1500, delay: 50 * time.Millisecond, burst: 5}},
	{name: "tui-code", tui: true, secs: 15, reply: reply{chunks: 1500, delay: 10 * time.Millisecond, shape: "code"}},
	{name: "tui-tools", tui: true, secs: 8, reply: reply{chunks: 100, toolTurns: 100}},
}

func main() {
	kon := flag.String("kon", "bin/kon", "kon binary to measure")
	tui := flag.Bool("tui", false, "also run TUI scenarios (Linux and tmux)")
	pattern := flag.String("run", "", "only run scenarios matching this regexp")
	flag.Parse()
	filter, err := regexp.Compile(*pattern)
	if err != nil {
		fatal(err)
	}
	if *kon, err = filepath.Abs(*kon); err != nil {
		fatal(err)
	}
	root, err := os.MkdirTemp("", "kon-loadtest-")
	if err != nil {
		fatal(err)
	}
	defer os.RemoveAll(root)

	mock := &mockModel{}
	server := httptest.NewServer(mock)
	defer server.Close()
	env, err := isolate(root, server.URL)
	if err != nil {
		fatal(err)
	}
	// The first launch migrates the empty data directory; keep that out of
	// the startup numbers.
	mock.reply.Store(&reply{chunks: 1})
	if _, err := runHeadless(*kon, env, root); err != nil {
		fatal(err)
	}

	fmt.Printf("%-18s %9s %9s %6s %9s  %s\n", "scenario", "wall", "cpu", "cpu%", "max rss", "detail")
	for _, s := range scenarios {
		if !filter.MatchString(s.name) || s.tui && !*tui {
			continue
		}
		mock.reply.Store(&s.reply)
		var r result
		if s.tui {
			r, err = runTUI(*kon, env, root, s)
		} else {
			r, err = runHeadless(*kon, env, root)
		}
		if err != nil {
			fatal(fmt.Errorf("%s: %w", s.name, err))
		}
		fmt.Printf("%-18s %9s %9s %5.0f%% %7.1fMB  %s\n", s.name,
			r.wall.Round(time.Millisecond), r.cpu.Round(time.Millisecond),
			100*r.cpu.Seconds()/r.wall.Seconds(), float64(r.maxRSS)/(1<<20), r.detail)
	}
}

// isolate writes a config pointing at the mock model and returns the
// environment that confines kon's config and data to root.
func isolate(root, url string) ([]string, error) {
	configDir := filepath.Join(root, "config", "kon")
	if err := os.MkdirAll(configDir, 0o700); err != nil {
		return nil, err
	}
	config := fmt.Sprintf(`{"default_model":"mock","models":[{"name":"mock","model":"mock","base_url":%q,"context_window_tokens":10000000}]}`, url)
	if err := os.WriteFile(filepath.Join(configDir, "config.json"), []byte(config), 0o600); err != nil {
		return nil, err
	}
	return append(os.Environ(),
		"XDG_CONFIG_HOME="+filepath.Join(root, "config"),
		"XDG_DATA_HOME="+filepath.Join(root, "data"),
	), nil
}

type result struct {
	wall, cpu time.Duration
	maxRSS    int64 // bytes
	// detail is the user/sys split for a headless run, or the CPU percent of
	// each sampled second for a TUI run.
	detail string
}

func runHeadless(kon string, env []string, dir string) (result, error) {
	cmd := exec.Command(kon, "run", "go")
	cmd.Env, cmd.Dir = env, dir
	var stderr strings.Builder
	cmd.Stderr = &stderr
	start := time.Now()
	if err := cmd.Run(); err != nil {
		return result{}, fmt.Errorf("%w: %s", err, stderr.String())
	}
	wall := time.Since(start)
	user, sys := cmd.ProcessState.UserTime(), cmd.ProcessState.SystemTime()
	maxRSS := cmd.ProcessState.SysUsage().(*syscall.Rusage).Maxrss
	// Linux reports ru_maxrss in kilobytes; the BSDs and macOS in bytes.
	if runtime.GOOS == "linux" {
		maxRSS <<= 10
	}
	return result{
		wall:   wall,
		cpu:    user + sys,
		maxRSS: maxRSS,
		detail: fmt.Sprintf("user %s sys %s", user.Round(time.Millisecond), sys.Round(time.Millisecond)),
	}, nil
}

// runTUI starts kon in a tmux server of its own, so a developer's sessions
// are never touched, sends the prompt, and samples the process each second.
func runTUI(kon string, env []string, dir string, s scenario) (result, error) {
	socket := fmt.Sprintf("kon-loadtest-%d-%s", os.Getpid(), s.name)
	tmux := func(args ...string) *exec.Cmd {
		cmd := exec.Command("tmux", append([]string{"-L", socket}, args...)...)
		cmd.Env = env
		return cmd
	}
	// exec makes kon the pane's own process, so pane_pid is kon's pid.
	start := tmux("new-session", "-d", "-x", "160", "-y", "50", "-c", dir, "exec "+shellQuote(kon))
	if out, err := start.CombinedOutput(); err != nil {
		return result{}, fmt.Errorf("start tmux: %w: %s", err, out)
	}
	defer tmux("kill-server").Run()
	out, err := tmux("list-panes", "-F", "#{pane_pid}").Output()
	if err != nil {
		return result{}, err
	}
	pid := strings.TrimSpace(string(out))
	time.Sleep(time.Second) // let the first frame settle
	if s.reply.chunks > 0 || s.reply.toolTurns > 0 {
		if err := tmux("send-keys", "go", "Enter").Run(); err != nil {
			return result{}, err
		}
	}
	first, err := cpuTime(pid)
	if err != nil {
		return result{}, err
	}
	var r result
	var samples []string
	prev := first
	for range s.secs {
		time.Sleep(time.Second)
		now, err := cpuTime(pid)
		if err != nil {
			return result{}, err
		}
		samples = append(samples, strconv.Itoa(int(100*(now-prev).Seconds())))
		prev = now
		if rss, err := rssBytes(pid); err == nil && rss > r.maxRSS {
			r.maxRSS = rss
		}
	}
	r.wall = time.Duration(s.secs) * time.Second
	r.cpu = prev - first
	r.detail = "per second: " + strings.Join(samples, " ")
	return r, nil
}

// clockTicks is USER_HZ, which Linux fixes at 100 for /proc.
const clockTicks = 100

func cpuTime(pid string) (time.Duration, error) {
	data, err := os.ReadFile("/proc/" + pid + "/stat")
	if err != nil {
		return 0, err
	}
	// The command name may contain spaces, so count fields after its ')'.
	fields := strings.Fields(string(data[strings.LastIndexByte(string(data), ')')+1:]))
	utime, _ := strconv.ParseInt(fields[11], 10, 64)
	stime, _ := strconv.ParseInt(fields[12], 10, 64)
	return time.Duration(utime+stime) * time.Second / clockTicks, nil
}

func rssBytes(pid string) (int64, error) {
	data, err := os.ReadFile("/proc/" + pid + "/status")
	if err != nil {
		return 0, err
	}
	for _, line := range strings.Split(string(data), "\n") {
		if rest, ok := strings.CutPrefix(line, "VmRSS:"); ok {
			kb, err := strconv.ParseInt(strings.TrimSuffix(strings.TrimSpace(rest), " kB"), 10, 64)
			return kb << 10, err
		}
	}
	return 0, fmt.Errorf("no VmRSS for pid %s", pid)
}

func shellQuote(s string) string {
	return "'" + strings.ReplaceAll(s, "'", `'\''`) + "'"
}

// mockModel streams Chat Completions replies. It decides whether to call
// tools from how many tool results the request already carries, so one
// handler serves a whole multi-turn run.
type mockModel struct {
	reply atomic.Pointer[reply]
}

func (m *mockModel) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	if !strings.HasSuffix(r.URL.Path, "/chat/completions") {
		http.NotFound(w, r)
		return
	}
	var request struct {
		Messages []struct {
			Role string `json:"role"`
		} `json:"messages"`
	}
	if err := json.NewDecoder(r.Body).Decode(&request); err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}
	results := 0
	for _, message := range request.Messages {
		if message.Role == "tool" {
			results++
		}
	}
	reply := *m.reply.Load()
	parallel := max(reply.parallel, 1)
	w.Header().Set("Content-Type", "text/event-stream")
	flusher := w.(http.Flusher)
	send := func(v any) {
		data, _ := json.Marshal(v)
		fmt.Fprintf(w, "data: %s\n\n", data)
		flusher.Flush()
	}
	delta := func(d map[string]any, finish any) map[string]any {
		return map[string]any{"choices": []any{map[string]any{"index": 0, "delta": d, "finish_reason": finish}}}
	}
	if results/parallel < reply.toolTurns {
		command := reply.toolCmd
		if command == "" {
			command = "seq 1 2000"
		}
		args, _ := json.Marshal(map[string]any{"command": command, "timeout": 60})
		for i := range parallel {
			send(delta(map[string]any{"tool_calls": []any{map[string]any{
				"index": i, "id": fmt.Sprintf("call-%d-%d", results, i), "type": "function",
				"function": map[string]any{"name": "shell", "arguments": string(args)},
			}}}, nil))
		}
		send(delta(map[string]any{}, "tool_calls"))
	} else {
		for i := range reply.chunks {
			send(delta(map[string]any{"content": token(reply.shape, i)}, nil))
			if reply.delay > 0 && (i+1)%max(reply.burst, 1) == 0 {
				time.Sleep(reply.delay)
			}
		}
		send(delta(map[string]any{}, "stop"))
	}
	send(map[string]any{"choices": []any{}, "usage": map[string]any{"prompt_tokens": 1000, "completion_tokens": reply.chunks}})
	io.WriteString(w, "data: [DONE]\n\n")
}

var (
	docTokens  = strings.SplitAfter("## Heading\n\nA paragraph with **bold**, *emphasis* and `code` that wraps across a narrow terminal.\n\n- first item\n- second [link](https://example.com)\n\n```go\nfunc main() {\n\tfmt.Println(1)\n}\n```\n\n> a quote\n\n", " ")
	codeTokens = strings.SplitAfter("\tif err := run(ctx, cfg); err != nil {\n\t\treturn fmt.Errorf(\"run: %w\", err)\n\t}\n", " ")
)

// token returns the i-th delta of a reply. doc separates markdown blocks with
// blank lines like typical model output; paragraph never breaks a line; code
// is one fenced block that never closes. The last two never give the
// markdown stream a blank line to settle at.
func token(shape string, i int) string {
	switch shape {
	case "paragraph":
		return strings.ReplaceAll(docTokens[i%len(docTokens)], "\n", " ")
	case "code":
		if i == 0 {
			return "```go\n"
		}
		return codeTokens[i%len(codeTokens)]
	}
	return docTokens[i%len(docTokens)]
}

func fatal(err error) {
	fmt.Fprintln(os.Stderr, "loadtest:", err)
	os.Exit(1)
}
