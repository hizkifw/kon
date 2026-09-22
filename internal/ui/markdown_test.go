package ui

import (
	"strings"
	"testing"

	"github.com/charmbracelet/x/ansi"
)

// TestMarkdownAssistantRendersStructure checks that an assistant message
// renders markdown structure (headings, lists, code) rather than flat text.
func TestMarkdownAssistantRendersStructure(t *testing.T) {
	var tr transcript
	tr.cwd = "/tmp"
	tr.add(block{kind: blockAssistant, text: "# Title\n\ntext with **bold**\n\n- one\n- two\n\n```go\nx := 1\n```"})
	got := plain(strings.Join(tr.linesFor(60), "\n"))
	for _, want := range []string{"Title", "text with bold", "• one", "• two", "x := 1"} {
		if !strings.Contains(got, want) {
			t.Fatalf("rendered transcript missing %q:\n%s", want, got)
		}
	}
	// The list markers and heading must be present; raw markdown syntax must
	// not leak through.
	for _, unwanted := range []string{"# Title", "**bold**", "```"} {
		if strings.Contains(got, unwanted) {
			t.Fatalf("rendered transcript leaked markdown syntax %q:\n%s", unwanted, got)
		}
	}
}

// TestMarkdownStreamMatchesSettledBlock is the integration's core invariant:
// a message streamed through the live markdown renderer must equal the same
// message rendered as a settled block, so folding a finished stream into
// history does not shift the text.
func TestMarkdownStreamMatchesSettledBlock(t *testing.T) {
	inputs := []string{
		"plain single line",
		"# Heading\n\nparagraph\n\n- a\n- b",
		"text with **bold** and `code` and *emph*",
		"> quoted\n\n> more",
		"```go\nfunc main() {}\n```",
		"a very long paragraph that will wrap across several lines at a narrow width for sure",
		"",
	}
	for _, in := range inputs {
		for _, width := range []int{8, 20, 40, 80} {
			var streamed transcript
			streamed.cwd = "/tmp"
			for i := 0; i < len(in); i += 3 {
				streamed.appendStream(in[i:min(i+3, len(in))])
			}
			var settled transcript
			settled.cwd = "/tmp"
			settled.add(block{kind: blockAssistant, text: in})
			got := plain(strings.Join(streamed.linesFor(width), "\n"))
			want := plain(strings.Join(settled.linesFor(width), "\n"))
			if got != want {
				t.Fatalf("width=%d input=%q\nstream=%q\nsettle=%q", width, in, got, want)
			}
		}
	}
}

// TestMarkdownLinesFitViewport guards the width contract end to end: every
// line the transcript paints is exactly the viewport width (so the slab
// background spans it) and none is truncated with an ellipsis. Markdown wraps
// to the slab's content width, and block prefixes (list markers, quote bars)
// are accounted for, so no line ever overflows into truncation.
func TestMarkdownLinesFitViewport(t *testing.T) {
	docs := []string{
		strings.Repeat("prose words here ", 30),
		"# " + strings.Repeat("heading ", 20),
		"- " + strings.Repeat("list ", 30),
		"1. " + strings.Repeat("ordered ", 20),
		"- [ ] " + strings.Repeat("task ", 20),
		"- outer " + strings.Repeat("x ", 40) + "\n  - nested " + strings.Repeat("y ", 40),
		"> " + strings.Repeat("quoted ", 30),
		"| a | b |\n|---|---|\n| " + strings.Repeat("cell ", 30) + " | z |",
		"```\n" + strings.Repeat("code chars here ", 20) + "\n```",
	}
	for _, width := range []int{20, 40, 60, 100} {
		for _, doc := range docs {
			var tr transcript
			tr.cwd = "/tmp"
			tr.add(block{kind: blockAssistant, text: doc})
			for _, line := range tr.linesFor(width) {
				if strings.Contains(line, "…") {
					t.Fatalf("width=%d doc=%q: line truncated: %q", width, doc[:12], plain(line))
				}
				if got := ansi.StringWidth(line); got != width {
					t.Fatalf("width=%d doc=%q: line width %d: %q", width, doc[:12], got, plain(line))
				}
			}
		}
	}
}

// TestMarkdownStreamFinalizeMatchesRender checks that finishing a stream folds
// into a block whose render equals the streamed view.
func TestMarkdownStreamFinalizeMatchesRender(t *testing.T) {
	in := "Intro with **bold**.\n\n1. first\n2. second\n\n> note"
	for _, width := range []int{20, 60} {
		var tr transcript
		tr.cwd = "/tmp"
		for i := 0; i < len(in); i += 2 {
			tr.appendStream(in[i:min(i+2, len(in))])
		}
		live := plain(strings.Join(tr.linesFor(width), "\n"))
		tr.finishStream()
		after := plain(strings.Join(tr.linesFor(width), "\n"))
		if live != after {
			t.Fatalf("width=%d: finalize shifted output\n live=%q\nafter=%q", width, live, after)
		}
	}
}
