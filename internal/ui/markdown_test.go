package ui

import (
	"fmt"
	"image/color"
	"strings"
	"testing"
	"unicode/utf8"

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

// TestMarkdownLinksClickable guards link presentation end to end: the URL is
// visible in the line text, the label and URL spans carry OSC 8 hyperlink
// sequences (so terminals that support them make the link clickable), and
// the OSC 8 sequences do not disturb the line width.
func TestMarkdownLinksClickable(t *testing.T) {
	doc := "Read the [release notes](https://github.com/example/project/releases/tag/v2.1.0) and the docs at <https://example.com/docs>."
	for _, width := range []int{40, 80} {
		var tr transcript
		tr.cwd = "/tmp"
		tr.add(block{kind: blockAssistant, text: doc})
		var visible strings.Builder
		oscSeen := false
		for _, line := range tr.linesFor(width) {
			visible.WriteString(plain(line))
			visible.WriteString(" ")
			if strings.Contains(line, "\x1b]8;;https://github.com/example/project/releases/tag/v2.1.0\x1b\\") {
				oscSeen = true
			}
			if got := ansi.StringWidth(line); got != width {
				t.Fatalf("width=%d: line width %d: %q", width, got, plain(line))
			}
		}
		if !oscSeen {
			t.Fatalf("width=%d: no OSC 8 hyperlink emitted", width)
		}
		got := visible.String()
		for _, want := range []string{"release notes", "https://github.com/example/project/releases/tag/v2.1.0", "https://example.com/docs"} {
			if !strings.Contains(got, want) {
				t.Fatalf("width=%d: visible text missing %q:\n%s", width, want, got)
			}
		}
	}
}

// TestMarkdownTableTinted checks that a table paints with the row tints
// (header and alternate-row backgrounds), that the tints span the tabular
// region without punching holes, and that every painted line still fills the
// viewport exactly.
func TestMarkdownTableTinted(t *testing.T) {
	doc := "| Name | Qty |\n|:--|--:|\n| alice | 12 |\n| bob | 3 |\n| cara | 4 |"
	for _, width := range []int{20, 40} {
		var tr transcript
		tr.cwd = "/tmp"
		tr.add(block{kind: blockAssistant, text: doc})
		lines := tr.linesFor(width)
		joined := strings.Join(lines, "\n")
		if !strings.Contains(joined, bgSeq(colorTableHeaderBg)) {
			t.Fatalf("width=%d: header background not painted:\n%q", width, joined)
		}
		if !strings.Contains(joined, bgSeq(colorTableRowBg)) {
			t.Fatalf("width=%d: alternate-row background not painted:\n%q", width, joined)
		}
		// The header text and body text are still visible and aligned.
		for _, want := range []string{"Name", "Qty", "alice", "bob", "cara"} {
			if !strings.Contains(plain(joined), want) {
				t.Fatalf("width=%d: table text missing %q:\n%s", width, want, plain(joined))
			}
		}
		for _, line := range lines {
			if got := ansi.StringWidth(line); got != width {
				t.Fatalf("width=%d: painted line width %d: %q", width, got, plain(line))
			}
		}
	}
}

// TestMarkdownTableHighlightRectangular checks that the row highlight is a
// consistent rectangle: every table line's tinted cells form one contiguous run
// starting at the left edge, and that run is the same width on the header, the
// body rows, and the wrapped continuation lines. This is what keeps a table's
// highlight aligned instead of ragged or short of the right edge.
func TestMarkdownTableHighlightRectangular(t *testing.T) {
	doc := "| Name | Description |\n|---|---|\n| alice | a fairly long description that wraps |\n| bob | short |\n| cara | another long description that also wraps here |"
	for _, width := range []int{24, 40} {
		var tr transcript
		tr.cwd = "/tmp"
		tr.add(block{kind: blockAssistant, text: doc})
		wantLo, wantHi := -1, -1
		for _, line := range tr.linesFor(width) {
			lo, hi := highlightRun(line)
			if hi <= 0 {
				continue // an untinted row or a non-table line
			}
			if wantLo < 0 {
				wantLo, wantHi = lo, hi
				continue
			}
			if lo != wantLo || hi != wantHi {
				t.Fatalf("width=%d: highlight run [%d,%d) != [%d,%d) on line %q",
					width, lo, hi, wantLo, wantHi, plain(line))
			}
		}
		if wantHi < 1 {
			t.Fatalf("width=%d: no table highlight found", width)
		}
	}
}

// highlightRun returns the half-open cell range [lo,hi) covered by the table
// row background. lo is always 0 for a table line; hi is the number of
// contiguous highlighted cells, or -1 when the line carries no table tint.
func highlightRun(line string) (lo, hi int) {
	tinted := func(bg string) bool {
		return bg == bgSeq(colorTableHeaderBg) || bg == bgSeq(colorTableRowBg)
	}
	lo, hi = -1, 0
	cell := 0
	bg := ""
	for i := 0; i < len(line); {
		if line[i] == 0x1b && i+1 < len(line) && (line[i+1] == '[' || line[i+1] == ']') {
			if line[i+1] == ']' {
				// OSC 8 hyperlink: skip to BEL or ST.
				j := i + 2
				for j < len(line) && line[j] != 0x07 && !(line[j] == 0x1b && j+1 < len(line) && line[j+1] == '\\') {
					j++
				}
				i = j + 1
				continue
			}
			j := i + 2
			for j < len(line) && line[j] != 'm' {
				j++
			}
			params := line[i+2 : j]
			switch {
			case strings.Contains(params, "48;2;"):
				bg = params[strings.Index(params, "48;2;"):]
			case params == "" || params == "0" || strings.Contains(params, "49"):
				bg = ""
			}
			i = j + 1
			continue
		}
		r, size := utf8.DecodeRuneInString(line[i:])
		w := ansi.StringWidth(string(r))
		if tinted(bg) {
			if lo < 0 {
				lo = cell
			}
			hi = cell + w
		}
		cell += w
		i += size
	}
	return lo, hi
}

// bgSeq returns the SGR fragment that selects c as a background color, as
// lipgloss emits it in truecolor mode.
func bgSeq(c color.Color) string {
	r, g, b, _ := c.RGBA()
	return fmt.Sprintf("48;2;%d;%d;%d", r>>8, g>>8, b>>8)
}

// TestMarkdownLinkNoControlInjection checks that control bytes in a link
// destination cannot break out of the OSC 8 sequence and inject terminal
// escapes: every ESC in the painted line must begin a well-formed sequence
// (CSI "\x1b[", OSC "\x1b]", or the ST terminator "\x1b\\"), never a bare
// escape. Percent-encoded control bytes are left verbatim in the URL, so
// they stay inert text rather than becoming terminal control.
func TestMarkdownLinkNoControlInjection(t *testing.T) {
	var tr transcript
	tr.cwd = "/tmp"
	tr.add(block{kind: blockAssistant, text: "go [here](https://x.example/a%07b%1bc) now"})
	for _, line := range tr.linesFor(60) {
		for i := 0; i < len(line); i++ {
			if line[i] != 0x1b {
				continue
			}
			if i+1 >= len(line) {
				t.Fatalf("trailing ESC byte: %q", line)
			}
			switch line[i+1] {
			case '[', ']', '\\':
			default:
				t.Fatalf("stray ESC byte followed by %q at %d: %q", line[i+1], i, line)
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
