package ui

import (
	"fmt"
	"strings"
	"testing"

	"github.com/charmbracelet/x/ansi"
	"github.com/hizkifw/kon/internal/tools"
)

// plain renders a transcript with ANSI escapes and trailing slab padding
// stripped, so assertions see layout regardless of the color profile detected
// during tests.
func plain(s string) string {
	lines := strings.Split(sanitize(s), "\n")
	for i, line := range lines {
		lines[i] = strings.TrimRight(line, " ")
	}
	return strings.Join(lines, "\n")
}

// toolCallBlock builds a running tool-call block with its owned display
// resolved for cwd.
func toolCallBlock(name, args, cwd string) block {
	return block{kind: blockTool, name: name, args: args, display: tools.Describe(name, []byte(args), "", false, cwd)}
}

// toolDoneBlock builds a finished tool-result block with its owned display
// resolved for cwd.
func toolDoneBlock(name, args, result string, failed bool, cwd string) block {
	return block{kind: blockResult, name: name, args: args, display: tools.Describe(name, []byte(args), result, failed, cwd)}
}

func TestNormalizeTextTrimsAndCollapsesBlanks(t *testing.T) {
	cases := []struct{ in, want string }{
		{"\n\nhello  \nworld   \n\n\n  \n", "hello\nworld"},
		{"a\r\nb\r\n", "a\nb"},
		{"  indented\n\ttabbed", "  indented\n\ttabbed"},
		{"one\n\n\n\ntwo", "one\n\ntwo"},
		{"", ""},
	}
	for _, tc := range cases {
		if got := normalizeText(tc.in); got != tc.want {
			t.Errorf("normalizeText(%q) = %q, want %q", tc.in, got, tc.want)
		}
	}
}

func TestShellOutputCarriageReturnsDoNotMangle(t *testing.T) {
	var tr transcript
	tr.cwd = "/tmp"
	tr.add(toolCallBlock("shell", `{"command":"prog"}`, "/tmp"))
	// CR-LF and spinner-style \r output should render as distinct lines.
	tr.add(toolDoneBlock("shell", `{"command":"prog"}`, "first  \r\nsecond\rthird\nexit code: 0", false, "/tmp"))
	got := plain(tr.render(80))
	lines := strings.Split(got, "\n")
	at := map[string]int{}
	for i, line := range lines {
		for _, want := range []string{"first", "second", "third"} {
			if strings.Contains(line, want) {
				at[want] = i
			}
		}
	}
	if at["first"] < 0 || at["second"] < 0 || at["third"] < 0 {
		t.Fatalf("shell output lost lines: %q", got)
	}
	if !(at["first"] < at["second"] && at["second"] < at["third"]) {
		t.Fatalf("shell output lines out of order: %q", got)
	}
}

func TestTranscriptGroupsConsecutiveToolCalls(t *testing.T) {
	var tr transcript
	tr.cwd = "/tmp"
	tr.add(toolCallBlock("read", `{"path":"/tmp/a.go"}`, "/tmp"))
	tr.add(toolDoneBlock("read", `{"path":"/tmp/a.go"}`, "     1  x\n… 11 more lines", false, "/tmp"))
	tr.add(toolCallBlock("read", `{"path":"/tmp/b.go"}`, "/tmp"))
	tr.add(toolDoneBlock("read", `{"path":"/tmp/b.go"}`, "     1  y\n     2  z\n     3  w\n     4  v\n     5  u", false, "/tmp"))
	got := plain(tr.render(80))
	lines := strings.Split(got, "\n")
	a, b := -1, -1
	for i, line := range lines {
		switch {
		case strings.Contains(line, "a.go"):
			a = i
		case strings.Contains(line, "b.go"):
			b = i
		}
	}
	if a < 0 || b != a+1 {
		t.Fatalf("expected stacked tool calls, got %q", got)
	}
	if !strings.Contains(got, "· 12 lines") || !strings.Contains(got, "· 5 lines") {
		t.Fatalf("line counts missing: %q", got)
	}
}

func TestRunningToolHasNoResultYet(t *testing.T) {
	var tr transcript
	tr.cwd = "/tmp"
	tr.add(toolCallBlock("shell", `{"command":"sleep 5"}`, "/tmp"))
	got := plain(tr.render(80))
	if !strings.Contains(got, "●") || !strings.Contains(got, "sleep 5") {
		t.Fatalf("running tool line missing: %q", got)
	}
	if strings.Contains(got, "exit") {
		t.Fatalf("running tool showed a result: %q", got)
	}
}

func TestShellResultTrimsToTailAndExitCode(t *testing.T) {
	var output []string
	for i := 0; i < 20; i++ {
		output = append(output, fmt.Sprintf("line-%02d", i))
	}
	var tr transcript
	tr.add(toolCallBlock("shell", `{"command":"./flaky"}`, "/tmp"))
	content := strings.Join(output, "\n") + "\nexit code: 3 (took 4.2s)"
	tr.add(toolDoneBlock("shell", `{"command":"./flaky"}`, content, true, "/tmp"))
	got := plain(tr.render(80))
	if !strings.Contains(got, "line-19") || strings.Contains(got, "line-05") {
		t.Fatalf("shell output was not trimmed to the tail: %q", got)
	}
	if !strings.Contains(got, "exit 3") || !strings.Contains(got, "took 4.2s") || !strings.Contains(got, "✗") {
		t.Fatalf("failing exit code not surfaced: %q", got)
	}
}

func TestSuccessfulReadAndEditOmitResultEcho(t *testing.T) {
	var tr transcript
	tr.cwd = "/tmp"
	tr.add(toolCallBlock("read", `{"path":"/tmp/a.go"}`, "/tmp"))
	tr.add(toolDoneBlock("read", `{"path":"/tmp/a.go"}`, "     1  a\n    10  j", false, "/tmp"))
	tr.add(toolCallBlock("edit", `{"path":"/tmp/a.go","old_text":"x","new_text":"y"}`, "/tmp"))
	tr.add(toolDoneBlock("edit", `{"path":"/tmp/a.go","old_text":"x","new_text":"y"}`, "edited /tmp/a.go", false, "/tmp"))
	got := plain(tr.render(80))
	lines := strings.Split(got, "\n")
	if len(lines) != 2 {
		t.Fatalf("expected one line per call, got %d: %q", len(lines), got)
	}
	if !strings.Contains(got, "· 10 lines") || !strings.Contains(got, "-1 +1 lines") {
		t.Fatalf("summaries missing: %q", got)
	}
}

func TestFailedToolShowsErrorOutput(t *testing.T) {
	var tr transcript
	tr.add(toolCallBlock("read", `{"path":"missing.txt"}`, "/tmp"))
	tr.add(toolDoneBlock("read", `{"path":"missing.txt"}`, "error: open missing.txt: no such file or directory", true, "/tmp"))
	got := plain(tr.render(80))
	if !strings.Contains(got, "✗") || !strings.Contains(got, "no such file or directory") {
		t.Fatalf("failed tool not shown: %q", got)
	}
}

func TestToolGroupSeparatedFromMessages(t *testing.T) {
	var tr transcript
	tr.cwd = "/tmp"
	tr.add(block{kind: blockUser, text: "do it"})
	tr.add(toolCallBlock("read", `{"path":"/tmp/a.go"}`, "/tmp"))
	tr.add(toolDoneBlock("read", `{"path":"/tmp/a.go"}`, "     1  a\n     2  b\n     3  c", false, "/tmp"))
	got := plain(tr.render(80))
	lines := strings.Split(got, "\n")
	user, tool := -1, -1
	for i, line := range lines {
		switch {
		case strings.Contains(line, "do it"):
			user = i
		case strings.Contains(line, "a.go"):
			tool = i
		}
	}
	if user < 0 || tool < user+1 {
		t.Fatalf("expected a blank line between the message and the tool group: %q", got)
	}
}

func TestMessageSlabWrapsToWidth(t *testing.T) {
	var tr transcript
	tr.add(block{kind: blockUser, text: strings.Repeat("word ", 40)})
	got := plain(tr.render(40))
	if !strings.Contains(got, "word") {
		t.Fatalf("user message missing: %q", got)
	}
	for _, line := range strings.Split(got, "\n") {
		if len([]rune(line)) > 40 {
			t.Fatalf("line exceeded width: %q", line)
		}
	}
}

func TestContextRendersAsSeparator(t *testing.T) {
	var tr transcript
	tr.add(block{kind: blockContext, text: "compacted ~12.3k tokens"})
	got := plain(tr.render(60))
	if !strings.Contains(got, "compacted ~12.3k tokens") || !strings.Contains(got, "─") {
		t.Fatalf("separator = %q", got)
	}
}

// TestBannerLeadsEveryTranscript guards the welcome banner: it sits at the top
// of the transcript, padded from the viewport edge by one blank line, before any
// content; it remains there over later blocks and a live stream, and is never
// captured as a transcript block.
func TestBannerLeadsEveryTranscript(t *testing.T) {
	var tr transcript
	tr.cwd = "/tmp"
	tr.banner = welcomeBanner
	empty := plain(tr.render(80))
	if !strings.HasPrefix(empty, "\n ┌──┐") || !strings.Contains(empty, "harness for foxes") {
		t.Fatalf("empty transcript did not lead with a padded banner: %q", empty)
	}
	if len(tr.blocks) != 0 {
		t.Fatal("banner leaked into the transcript blocks")
	}
	tr.appendStream("hello")
	streaming := plain(tr.render(80))
	if !strings.HasPrefix(streaming, "\n ┌──┐") || !strings.Contains(streaming, "hello") {
		t.Fatalf("banner did not lead the streaming transcript: %q", streaming)
	}
	tr.finishStream()
	tr.add(block{kind: blockUser, text: "question"})
	settled := plain(tr.render(80))
	if !strings.HasPrefix(settled, "\n ┌──┐") || !strings.Contains(settled, "question") {
		t.Fatalf("banner did not lead the settled transcript: %q", settled)
	}
	if strings.Index(settled, "harness for foxes") > strings.Index(settled, "question") {
		t.Fatalf("banner rendered below the conversation: %q", settled)
	}
}

// TestBannerStaysAtTopAcrossWidths ensures the banner remains the first visible
// line of the cached display lines at every width, so it stays pinned top-left.
func TestBannerStaysAtTopAcrossWidths(t *testing.T) {
	var tr transcript
	tr.banner = welcomeBanner
	tr.add(block{kind: blockUser, text: "question"})
	for _, width := range []int{40, 80, 120} {
		lines := tr.linesFor(width)
		if len(lines) < 2 || lines[0] != "" || !strings.HasPrefix(plain(lines[1]), " ┌──┐") {
			t.Fatalf("width %d: banner is not the first visible line: %q", width, plain(strings.Join(lines, "\n")))
		}
	}
}

// TestBannerHidesWhenTooNarrow ensures a terminal too narrow for the figure
// shows nothing rather than a wrapped or truncated mark.
func TestBannerHidesWhenTooNarrow(t *testing.T) {
	var tr transcript
	tr.banner = welcomeBanner
	if got := plain(tr.render(10)); got != "" {
		t.Fatalf("narrow render = %q, want empty", got)
	}
}

// TestBannerLinesFitWidth pins that every banner line fits the viewport so the
// mark is never clipped at the right edge.
func TestBannerLinesFitWidth(t *testing.T) {
	var tr transcript
	tr.banner = welcomeBanner
	for _, width := range []int{40, 80, 120} {
		for _, line := range tr.linesFor(width) {
			if got := ansi.StringWidth(line); got > width {
				t.Fatalf("width %d: banner line width = %d: %q", width, got, line)
			}
		}
	}
}

func TestRenderRebuildsWhenWidthChanges(t *testing.T) {
	var tr transcript
	tr.add(block{kind: blockUser, text: "hello"})
	if tr.render(80) == "" {
		t.Fatal("render was empty")
	}
	if tr.dirty || tr.width != 80 {
		t.Fatalf("first render did not cache: dirty=%v width=%d", tr.dirty, tr.width)
	}
	// Same width serves the cache even if blocks changed underneath.
	tr.blocks = nil
	if got := plain(tr.render(80)); !strings.Contains(got, "hello") {
		t.Fatalf("cached render lost content: %q", got)
	}
	// A width change rebuilds.
	if got := plain(tr.render(40)); got != "" {
		t.Fatalf("width change did not rebuild the cache: %q", got)
	}
}

// TestIncrementalRenderMatchesFullRender drives a transcript whose tool runs
// are re-rendered between each append (start, then result, then the next call
// terminating the run) and checks the incrementally built output against a
// from-scratch render at every step.
func TestIncrementalRenderMatchesFullRender(t *testing.T) {
	var tr transcript
	tr.cwd = "/tmp"
	steps := []block{
		{kind: blockUser, text: "do the thing"},
		{kind: blockAssistant, text: "on it"},
		toolCallBlock("shell", `{"command":"go build"}`, "/tmp"),
		toolDoneBlock("shell", `{"command":"go build"}`, "ok\nexit code: 0 (took 1.0s)", false, "/tmp"),
		toolCallBlock("read", `{"path":"/tmp/a.go"}`, "/tmp"),
		toolDoneBlock("read", `{"path":"/tmp/a.go"}`, "     1  x\n… 11 more lines", false, "/tmp"),
		{kind: blockThinking, text: "hmm"},
		{kind: blockError, text: "boom"},
		{kind: blockContext, text: "compacted ~1.2k tokens"},
		{kind: blockModel, text: "switched to gpt-5"},
		{kind: blockModels, text: "a\nb"},
		toolCallBlock("shell", `{"command":"go test"}`, "/tmp"),
		toolDoneBlock("shell", `{"command":"go test"}`, "fail\nexit code: 2", true, "/tmp"),
		{kind: blockAssistant, text: "done"},
	}
	for i, b := range steps {
		tr.add(b)
		got := plain(tr.render(60))
		fresh := transcript{cwd: "/tmp", blocks: append([]block(nil), tr.blocks...)}
		want := plain(fresh.render(60))
		if got != want {
			t.Fatalf("step %d (%s): incremental render diverged\n got: %q\nwant: %q", i, b.name, got, want)
		}
	}
}

// TestLineCacheMatchesRenderedLines drives appends and stream deltas and checks
// that the cached display lines always equal the lines of a from-scratch render.
// The cache keeps its stable prefix across frames, so this guards the slice
// bookkeeping and the blank separator before the live tail.
func TestLineCacheMatchesRenderedLines(t *testing.T) {
	var tr transcript
	tr.cwd = "/tmp"
	check := func(step string) {
		t.Helper()
		got := strings.Join(tr.linesFor(60), "\n")
		fresh := transcript{cwd: "/tmp", blocks: append([]block(nil), tr.blocks...), stream: append([]byte(nil), tr.stream...), thinking: tr.thinking}
		want := fresh.render(60)
		if got != want {
			t.Fatalf("%s: cached lines diverged\n got: %q\nwant: %q", step, plain(got), plain(want))
		}
	}
	tr.add(block{kind: blockUser, text: "do the thing"})
	check("after user")
	tr.add(toolCallBlock("read", `{"path":"/tmp/a.go"}`, "/tmp"))
	check("tool running")
	tr.add(toolDoneBlock("read", `{"path":"/tmp/a.go"}`, "     1  x\n… 11 more lines", false, "/tmp"))
	check("tool done")
	tr.appendStream("streaming ")
	check("stream start")
	tr.appendStream("more text")
	check("stream grow")
	tr.finishStream()
	check("stream finish")
	tr.appendThinking("reasoning")
	check("thinking")
	tr.finishStream()
	check("thinking finish")
	tr.add(block{kind: blockAssistant, text: "done"})
	check("final")
}

// TestStreamRenderMatchesFullRender feeds stream and thinking deltas in small
// chunks and checks the incremental live renderer against a from-scratch render
// after every delta, across widths. This is the output-equivalence guard for the
// tail-only live render.
func TestStreamRenderMatchesFullRender(t *testing.T) {
	inputs := []string{
		"",
		"hello",
		"line one\nline two",
		"a\n\n\nb",
		"\n\nleading\n",
		"trailing\n\n",
		strings.Repeat("word ", 60),
		"mixed\nsingle\n\n\n\ndouble\r\ncrlf\rspin",
		// ANSI and control bytes must be stripped at the transcript boundary
		// (see stripANSI) so the plain-text wrapper never sees them. The
		// expected value is computed on the stripped input, so this also fails
		// if stripping ever diverges between the live and buffered paths.
		"\x1b[31mstyled\x1b[0m plain \x07after\x1b]8;;http://x\x1b\\link\x1b[m",
		"tab\there \x1b[Kend",
	}
	for _, in := range inputs {
		for _, width := range []int{8, 20, 41, 80} {
			for _, chunk := range []int{1, 3, 11} {
				var tr transcript
				tr.cwd = "/tmp"
				for i := 0; i < len(in); i += chunk {
					end := min(i+chunk, len(in))
					tr.appendStream(in[i:end])
					got := plain(strings.Join(tr.linesFor(width), "\n"))
					fresh := transcript{cwd: "/tmp", stream: []byte(tr.stream)}
					want := plain(fresh.render(width))
					if got != want {
						t.Fatalf("stream width=%d chunk=%d input=%q\n got=%q\nwant=%q", width, chunk, in, got, want)
					}
				}
			}
		}
	}
	for _, in := range inputs {
		for _, width := range []int{8, 41} {
			var tr transcript
			tr.cwd = "/tmp"
			for i := 0; i < len(in); i++ {
				tr.appendThinking(in[i : i+1])
				got := plain(strings.Join(tr.linesFor(width), "\n"))
				fresh := transcript{cwd: "/tmp", thinking: tr.thinking}
				want := plain(fresh.render(width))
				if got != want {
					t.Fatalf("thinking width=%d input=%q\n got=%q\nwant=%q", width, in, got, want)
				}
			}
		}
	}
}

// TestStreamWidthChangeRebuilds ensures a resize during streaming re-renders the
// live message at the new width.
func TestStreamWidthChangeRebuilds(t *testing.T) {
	var tr transcript
	tr.cwd = "/tmp"
	tr.appendStream(strings.Repeat("word ", 30))
	tr.linesFor(80)
	narrow := plain(strings.Join(tr.linesFor(30), "\n"))
	fresh := transcript{cwd: "/tmp", stream: []byte(tr.stream)}
	if want := plain(fresh.render(30)); narrow != want {
		t.Fatalf("narrowed stream diverged\n got=%q\nwant=%q", narrow, want)
	}
}

// TestStripANSI pins the transcript-boundary stripping: escapes and control
// bytes are removed, newlines and tabs survive, plain text passes through
// untouched, and a sequence split across deltas is dropped whole because the
// machine's state carries over.
func TestStripANSI(t *testing.T) {
	cases := []struct{ in, want string }{
		{"plain", "plain"},
		{"\x1b[31mred\x1b[0m", "red"},
		{"a\x1b[0mb", "ab"},
		{"\x1b]8;;http://x\x1b\\link\x1b[m", "link"},
		{"bell\x07end", "bellend"},
		{"keep\nnewlines\r\nand\ttabs", "keep\nnewlines\r\nand\ttabs"},
		{"\x1b", ""},                           // bare escape ends the delta
		{"\x1b[", ""},                          // unterminated CSI ends the delta
		{"\x1b]8;;unterminated", ""},           // unterminated OSC ends the delta
		{"\x1bMtwo-byte", "two-byte"},          // ESC M consumes only itself
		{"\x1b(Bintermediate", "intermediate"}, // ESC ( B is a three-byte escape
	}
	for _, tc := range cases {
		var s ansiStripper
		if got := s.strip(tc.in); got != tc.want {
			t.Errorf("stripANSI(%q) = %q, want %q", tc.in, got, tc.want)
		}
	}
	// The equivalence guard must see the same bytes whether an escape arrives
	// whole or split across deltas; splitting at every offset is the strong
	// form of that check.
	const in = "a \x1b[31mb \x1b]0;tc\x07c"
	var whole transcript
	whole.cwd = "/tmp"
	whole.appendStream(in)
	full := string(whole.stream)
	for cut := 0; cut <= len(in); cut++ {
		var tr transcript
		tr.cwd = "/tmp"
		tr.appendStream(in[:cut])
		tr.appendStream(in[cut:])
		if got := string(tr.stream); got != full {
			t.Fatalf("split at %d: stream = %q, want %q", cut, got, full)
		}
	}
	if strings.ContainsAny(full, "\x1b\x07") {
		t.Fatalf("escape bytes reached the buffered stream: %q", full)
	}
	if got := plain(strings.Join(whole.linesFor(40), "\n")); strings.Contains(got, "\x1b") {
		t.Fatalf("escape reached the rendered output: %q", got)
	}
}

// TestNoLabelsInMessageBlocks guards the label-free transcript: message,
// thinking, and error blocks render only their contents, with no role or model
// title lines above them.
func TestNoLabelsInMessageBlocks(t *testing.T) {
	var tr transcript
	tr.cwd = "/tmp"
	tr.add(block{kind: blockUser, text: "do it"})
	tr.add(block{kind: blockThinking, text: "hmm"})
	tr.add(block{kind: blockAssistant, text: "done"})
	tr.add(block{kind: blockError, text: "boom"})
	for _, label := range []string{"you", "kon", "thinking", "error"} {
		if got := plain(tr.render(40)); strings.Contains(got, label) {
			t.Fatalf("label %q leaked into the transcript: %q", label, got)
		}
	}
}

// TestSlabBackgroundSpansWidth guards the full-width background fill. The pad
// after the styled text must be measured with ANSI-aware width, not raw string
// length, or the background stops at the end of the text.
func TestSlabBackgroundSpansWidth(t *testing.T) {
	for _, width := range []int{20, 40, 79} {
		lines := append(
			messageSlab("short", colorUserBg, colorUserFg, width),
			thinkingLines("short", width)...,
		)
		lines = append(lines, messageSlab(strings.Repeat("word ", 30), colorAgentBg, colorAgentFg, width)...)
		live := newMarkdownLive(colorAgentBg, colorAgentFg, width)
		live.append(strings.Repeat("words with **bold** and `code`. ", 20))
		lines = append(lines, live.currentLines()...)
		for _, line := range lines {
			if got := ansi.StringWidth(line); got != width {
				t.Fatalf("width %d: line width = %d, background does not span the viewport: %q", width, got, line)
			}
		}
	}
}

// TestIncrementalRenderSurvivesWidthChanges ensures that rebuilding after a
// width change yields the same output as a fresh render at that width.
func TestIncrementalRenderSurvivesWidthChanges(t *testing.T) {
	var tr transcript
	tr.cwd = "/tmp"
	tr.add(block{kind: blockUser, text: strings.Repeat("word ", 30)})
	tr.add(toolCallBlock("read", `{"path":"/tmp/a.go"}`, "/tmp"))
	tr.add(toolDoneBlock("read", `{"path":"/tmp/a.go"}`, "     1  a", false, "/tmp"))
	for _, width := range []int{40, 80, 30, 80, 120} {
		got := plain(tr.render(width))
		fresh := transcript{cwd: "/tmp", blocks: append([]block(nil), tr.blocks...)}
		want := plain(fresh.render(width))
		if got != want {
			t.Fatalf("width %d: incremental render diverged\n got: %q\nwant: %q", width, got, want)
		}
	}
}
