package tools

import (
	"encoding/json"
	"fmt"
	"strings"
	"testing"
)

func sprintf(format string, args ...any) string { return fmt.Sprintf(format, args...) }

func dargs(args map[string]any) json.RawMessage {
	b, err := json.Marshal(args)
	if err != nil {
		panic(err)
	}
	return b
}

func TestShellSummarizeCollapsesLineBreaks(t *testing.T) {
	shell := &shellTool{}
	got := shell.Summarize(dargs(map[string]any{"command": "go test\n./..."}), "/tmp")
	if got != "go test; ./..." {
		t.Fatalf("Summarize = %q", got)
	}
}

func TestShellDescribeParsesMarkerAndTrimsTail(t *testing.T) {
	shell := &shellTool{}
	var lines []string
	for i := 0; i < 20; i++ {
		lines = append(lines, sprintf("line-%02d", i))
	}
	content := strings.Join(lines, "\n") + "\nexit code: 3 (took 4.2s)"
	d := shell.Describe(dargs(map[string]any{"command": "./flaky"}), content, true, "/tmp")
	if d.State != StateFailed || d.Note != "exit 3 · took 4.2s" {
		t.Fatalf("failed shell display = %#v", d)
	}
	if len(d.Lines) != maxToolLines || d.Lines[len(d.Lines)-1] != "line-19" || d.Lines[0] != "line-14" {
		t.Fatalf("tail = %v", d.Lines)
	}
	if d.More != 14 {
		t.Fatalf("More = %d, want 14", d.More)
	}
}

func TestShellDescribeSuccessWithNoOutputCarriesNoteOnly(t *testing.T) {
	shell := &shellTool{}
	d := shell.Describe(dargs(map[string]any{"command": "true"}), "exit code: 0 (took 10ms)", false, "/tmp")
	if d.State != StateDone || d.Note != "exit 0 · took 10ms" || len(d.Lines) != 0 {
		t.Fatalf("empty success display = %#v", d)
	}
}

func TestShellDescribeWithoutMarkerShowsAllOutput(t *testing.T) {
	shell := &shellTool{}
	d := shell.Describe(dargs(map[string]any{"command": "x"}), "plain text", true, "/tmp")
	if d.State != StateFailed || d.Note != "failed" || d.Lines[0] != "plain text" {
		t.Fatalf("marker-less failure display = %#v", d)
	}
}

func TestReadDescribeCollapsesToNote(t *testing.T) {
	read := readTool{}
	content := "     1  package ui\n     2  x\n… 43 more lines"
	d := read.Describe(dargs(map[string]any{"path": "/tmp/proj/a.go"}), content, false, "/tmp/proj")
	if d.State != StateDone || d.Note != "45 lines" || len(d.Lines) != 0 || d.Summary != "a.go" {
		t.Fatalf("read display = %#v", d)
	}
	// Unsummarizable results render as body lines.
	notice := "(offset 100 is beyond end of file; 12 lines)"
	d = read.Describe(dargs(map[string]any{"path": "/tmp/a.go"}), notice, false, "/tmp")
	if d.Note != "" || d.Lines[0] != notice {
		t.Fatalf("read notice display = %#v", d)
	}
	// Failures show the error.
	d = read.Describe(dargs(map[string]any{"path": "/tmp/a.go"}), "error: open /tmp/a.go: no such file or directory", true, "/tmp")
	if d.State != StateFailed || d.Lines[0] != "error: open /tmp/a.go: no such file or directory" {
		t.Fatalf("read failure display = %#v", d)
	}
}

func TestWriteEditDescribeOwnSummaries(t *testing.T) {
	write := writeTool{}
	d := write.Describe(dargs(map[string]any{"path": "/tmp/proj/a.go", "content": "hi"}), "wrote 2 bytes to /tmp/proj/a.go", false, "/tmp/proj")
	if d.Summary != "a.go · 2B" || d.State != StateDone || len(d.Lines) != 0 {
		t.Fatalf("write display = %#v", d)
	}
	edit := editTool{}
	d = edit.Describe(dargs(map[string]any{"path": "/tmp/proj/a.go", "old_text": "a\nb", "new_text": "x"}), "edited /tmp/proj/a.go", false, "/tmp/proj")
	if d.Summary != "a.go · -2 +1 lines" || len(d.Lines) != 0 {
		t.Fatalf("edit display = %#v", d)
	}
	d = edit.Describe(dargs(map[string]any{"path": "/tmp/a.go", "old_text": "x", "new_text": "y"}), "error: old_text must occur exactly once; found 0 occurrences", true, "/tmp")
	if d.State != StateFailed || d.Lines[0] != "error: old_text must occur exactly once; found 0 occurrences" {
		t.Fatalf("edit failure display = %#v", d)
	}
}

func TestTailLinesDropsTrailingBlanksAndCounts(t *testing.T) {
	lines, more := tailLines("a\nb\nc\nd", 2)
	if more != 2 || len(lines) != 2 || lines[0] != "c" || lines[1] != "d" {
		t.Fatalf("tailLines = %v, %d", lines, more)
	}
	lines, more = tailLines("a\n\n\nb", 10)
	if more != 0 || len(lines) != 4 || lines[0] != "a" || lines[3] != "b" {
		t.Fatalf("tailLines with a blank run = %v, %d", lines, more)
	}
	lines, more = tailLines("a\r\nb\r", 10)
	if more != 0 || len(lines) != 2 || lines[1] != "b" {
		t.Fatalf("tailLines CR = %v, %d", lines, more)
	}
	lines, _ = tailLines("out\n", 6)
	if len(lines) != 1 || lines[0] != "out" {
		t.Fatalf("tailLines trailing newline = %v", lines)
	}
}

func TestFallbackSummaryMalformedArgs(t *testing.T) {
	if got := FallbackSummary(json.RawMessage(`not json`)); got != "not json" {
		t.Fatalf("FallbackSummary = %q", got)
	}
}
