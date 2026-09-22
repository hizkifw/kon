package ui

import (
	"strings"
	"testing"
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
