package ui

import (
	"fmt"
	"strings"
	"testing"

	"charm.land/lipgloss/v2"
	"github.com/charmbracelet/x/ansi"
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

func TestToolSummaryFormatsArguments(t *testing.T) {
	cases := []struct {
		name, args, want string
	}{
		{"read", `{"path":"/tmp/main.go"}`, "main.go"},
		{"read", `{"path":"/tmp/main.go","offset":40}`, "main.go from line 40"},
		{"write", `{"path":"/tmp/main.go","content":"hi"}`, "main.go · 2B"},
		{"edit", `{"path":"/tmp/main.go","old_text":"a\nb\nc","new_text":"x"}`, "main.go · -3 +1 lines"},
		{"shell", `{"command":"go test\n./..."}`, "go test; ./..."},
		{"unknown", `{"a":1}`, `{"a":1}`},
		{"unknown", `not json`, "not json"},
	}
	for _, tc := range cases {
		if got := toolSummary(tc.name, tc.args, "/tmp"); got != tc.want {
			t.Errorf("toolSummary(%s) = %q, want %q", tc.name, got, tc.want)
		}
	}
}

func TestPrettyPathPrefersCwdRelative(t *testing.T) {
	if got := prettyPath("/tmp/proj/main.go", "/tmp/proj"); got != "main.go" {
		t.Fatalf("prettyPath = %q", got)
	}
	if got := prettyPath("/tmp/other/main.go", "/tmp/proj"); got != "/tmp/other/main.go" {
		t.Fatalf("prettyPath = %q", got)
	}
}

func TestReadNoteCountsRowsAndMoreLines(t *testing.T) {
	cases := []struct{ text, want string }{
		{"     1  package ui\n… 44 more lines", "45 lines"},
		{"     1  only\n     2  two", "2 lines"},
		{"     7  x", "7 lines"},
		{"     1  ", "empty file"},
		{"(offset 100 is beyond end of file; 12 lines)", ""},
	}
	for _, tc := range cases {
		if got := readNote(tc.text); got != tc.want {
			t.Errorf("readNote(%q) = %q, want %q", tc.text, got, tc.want)
		}
	}
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

func TestNormalizeOutputPreservesInternalBlanks(t *testing.T) {
	cases := []struct{ in, want string }{
		{"  \nline one  \nline two\n\r\n", "line one\nline two"},
		{"a\n\n\nb", "a\n\n\nb"},
		{"start\r\nmid\r\n", "start\nmid"},
	}
	for _, tc := range cases {
		if got := normalizeOutput(tc.in); got != tc.want {
			t.Errorf("normalizeOutput(%q) = %q, want %q", tc.in, got, tc.want)
		}
	}
}

func TestShellOutputCarriageReturnsDoNotMangle(t *testing.T) {
	var tr transcript
	tr.add(block{kind: blockTool, name: "shell", args: `{"command":"prog"}`})
	// CR-LF and spinner-style \r output should render as distinct lines.
	tr.add(block{kind: blockResult, name: "shell", text: "first  \r\nsecond\rthird\n", exit: "0"})
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

func TestSplitExitCode(t *testing.T) {
	code, took, output := splitExitCode("line one\nline two\nexit code: 2 (took 1.5s)")
	if code != "2" || took != "1.5s" || output != "line one\nline two" {
		t.Fatalf("splitExitCode = %q, %q, %q", code, took, output)
	}
	code, took, output = splitExitCode("line one\nexit code: 2")
	if code != "2" || took != "" || output != "line one" {
		t.Fatalf("splitExitCode without a duration = %q, %q, %q", code, took, output)
	}
	if _, _, output := splitExitCode("no marker here"); output != "no marker here" {
		t.Fatalf("splitExitCode without a marker = %q", output)
	}
	if _, _, output := splitExitCode("exit code: "); output != "exit code: " {
		t.Fatalf("splitExitCode with an empty code = %q", output)
	}
}

func TestTranscriptGroupsConsecutiveToolCalls(t *testing.T) {
	var tr transcript
	tr.cwd = "/tmp"
	tr.add(block{kind: blockTool, name: "read", args: `{"path":"/tmp/a.go"}`})
	tr.add(block{kind: blockResult, name: "read", note: "12 lines"})
	tr.add(block{kind: blockTool, name: "read", args: `{"path":"/tmp/b.go"}`})
	tr.add(block{kind: blockResult, name: "read", note: "5 lines"})
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
	tr.add(block{kind: blockTool, name: "shell", args: `{"command":"sleep 5"}`})
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
	tr.add(block{kind: blockTool, name: "shell", args: `{"command":"./flaky"}`})
	tr.add(block{kind: blockResult, name: "shell", text: strings.Join(output, "\n"), exit: "3", took: "4.2s", failed: true})
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
	tr.add(block{kind: blockTool, name: "read", args: `{"path":"/tmp/a.go"}`})
	tr.add(block{kind: blockResult, name: "read", note: "10 lines"})
	tr.add(block{kind: blockTool, name: "edit", args: `{"path":"/tmp/a.go","old_text":"x","new_text":"y"}`})
	tr.add(block{kind: blockResult, name: "edit"})
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
	tr.add(block{kind: blockTool, name: "read", args: `{"path":"missing.txt"}`})
	tr.add(block{kind: blockResult, name: "read", text: "error: open missing.txt: no such file or directory", failed: true})
	got := plain(tr.render(80))
	if !strings.Contains(got, "✗") || !strings.Contains(got, "no such file or directory") {
		t.Fatalf("failed tool not shown: %q", got)
	}
}

func TestToolGroupSeparatedFromMessages(t *testing.T) {
	var tr transcript
	tr.cwd = "/tmp"
	tr.add(block{kind: blockUser, text: "do it"})
	tr.add(block{kind: blockTool, name: "read", args: `{"path":"/tmp/a.go"}`})
	tr.add(block{kind: blockResult, name: "read", note: "3 lines"})
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
	if !strings.Contains(got, "you") {
		t.Fatalf("user label missing: %q", got)
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
		{kind: blockTool, name: "shell", args: `{"command":"go build"}`},
		{kind: blockResult, name: "shell", text: "ok", exit: "0", took: "1.0s"},
		{kind: blockTool, name: "read", args: `{"path":"/tmp/a.go"}`},
		{kind: blockResult, name: "read", note: "12 lines"},
		{kind: blockThinking, text: "hmm"},
		{kind: blockError, text: "boom"},
		{kind: blockContext, text: "compacted ~1.2k tokens"},
		{kind: blockModel, text: "switched to gpt-5"},
		{kind: blockModels, text: "a\nb"},
		{kind: blockTool, name: "shell", args: `{"command":"go test"}`},
		{kind: blockResult, name: "shell", text: "fail", exit: "2", failed: true},
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
	tr.add(block{kind: blockTool, name: "read", args: `{"path":"/tmp/a.go"}`})
	check("tool running")
	tr.add(block{kind: blockResult, name: "read", note: "12 lines"})
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

// TestLabelRendersFaint verifies the thinking label keeps its intended color.
// messageSlab/thinkingLines paint through linePainter, and labelFg must be set
// for the label or it renders unstyled (the regression this guards).
func TestLabelRendersFaint(t *testing.T) {
	want := lipgloss.NewStyle().Foreground(colorFaint).Italic(true).Render("thinking")
	label := thinkingLines("body", 40)[0]
	if !strings.Contains(label, want) {
		t.Fatalf("thinking label lost its color: label=%q want styled %q", label, want)
	}
}

// TestSlabBackgroundSpansWidth guards the full-width background fill. The pad
// after the styled text must be measured with ANSI-aware width, not raw string
// length, or the background stops at the end of the text.
func TestSlabBackgroundSpansWidth(t *testing.T) {
	for _, width := range []int{20, 40, 79} {
		lines := append(
			messageSlab("you", "short", colorUserBg, colorUserFg, colorUserLabel, width),
			thinkingLines("short", width)...,
		)
		lines = append(lines, messageSlab("kon", strings.Repeat("word ", 30), colorAgentBg, colorAgentFg, colorAgentLabel, width)...)
		live := newMessageStream("kon", colorAgentBg, colorAgentFg, colorAgentLabel, width)
		live.append(strings.Repeat("word ", 30))
		lines = append(lines, live.Lines()...)
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
	tr.add(block{kind: blockTool, name: "read", args: `{"path":"/tmp/a.go"}`})
	tr.add(block{kind: blockResult, name: "read", note: "1 line"})
	for _, width := range []int{40, 80, 30, 80, 120} {
		got := plain(tr.render(width))
		fresh := transcript{cwd: "/tmp", blocks: append([]block(nil), tr.blocks...)}
		want := plain(fresh.render(width))
		if got != want {
			t.Fatalf("width %d: incremental render diverged\n got: %q\nwant: %q", width, got, want)
		}
	}
}
