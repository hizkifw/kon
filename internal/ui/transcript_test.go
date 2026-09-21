package ui

import (
	"fmt"
	"strings"
	"testing"
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
