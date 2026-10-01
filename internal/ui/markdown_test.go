package ui

import (
	"bufio"
	"fmt"
	"image/color"
	"io"
	"strings"
	"testing"
	"testing/iotest"
	"time"
	"unicode/utf8"

	"github.com/charmbracelet/x/ansi"
	"github.com/hizkifw/kon/internal/markdown"
	"github.com/hizkifw/kon/internal/tui"
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

// TestMarkdownCharacterReferencesCannotEmitEscapes feeds an assistant message
// that spells control characters as character references through both the
// streamed and the settled render. Decoded verbatim, they would reach the
// terminal as live escapes after sanitize had already passed the text.
func TestMarkdownCharacterReferencesCannotEmitEscapes(t *testing.T) {
	text := "clear &#x1b;[2J title &#27;]0;pwned&#7; csi &#x9b;2J"
	var streamed, settled transcript
	streamed.appendStream(tui.Sanitize(text))
	settled.add(block{kind: blockAssistant, text: tui.Sanitize(text)})
	for name, tr := range map[string]*transcript{"streamed": &streamed, "settled": &settled} {
		out := strings.Join(tr.linesFor(60), "\n")
		for _, escape := range []string{"\x1b[2J", "\x1b]0;", "\x07", "\u009b"} {
			if strings.Contains(out, escape) {
				t.Fatalf("%s render emitted %q:\n%q", name, escape, out)
			}
		}
		if !strings.Contains(plain(out), "title ]0;pwned csi 2J") {
			t.Fatalf("%s render lost the surrounding text:\n%s", name, plain(out))
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
		"Intro with **bold**.\n\n1. first\n2. second\n\n> note",
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
			// Finishing the stream folds it into history, which must reproduce
			// the settled block exactly.
			streamed.finishStream()
			if after := plain(strings.Join(streamed.linesFor(width), "\n")); after != want {
				t.Fatalf("width=%d input=%q: finalize shifted output\nafter=%q\nsettle=%q", width, in, after, want)
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
		if !strings.Contains(got, "release notes") {
			t.Fatalf("width=%d: visible text missing the label:\n%s", width, got)
		}
		// A long URL wraps across lines at its punctuation, so compare the
		// URLs with the wrap and padding whitespace taken out.
		unwrapped := strings.Join(strings.Fields(got), "")
		for _, want := range []string{"https://github.com/example/project/releases/tag/v2.1.0", "https://example.com/docs"} {
			if !strings.Contains(unwrapped, want) {
				t.Fatalf("width=%d: visible text missing %q:\n%s", width, want, got)
			}
		}
	}
}

// TestMarkdownTableTinted checks that a table paints with the row tints
// (header and alternate-row backgrounds) and that the tints span the tabular
// region without punching holes. TestMarkdownLinesFitViewport covers the
// width of every painted line.
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
			if lo, hi, contiguous := highlightRun(line); hi > 0 && (lo != 1 || !contiguous) {
				t.Fatalf("width=%d: tint [%d,%d) contiguous=%v, want one run from column 1 on line %q",
					width, lo, hi, contiguous, plain(line))
			}
		}
	}
}

// TestMarkdownTableHighlightRectangular checks that the row highlight is a
// consistent rectangle: every table line's tinted cells form one contiguous run
// starting at the left content edge, and that run is the same width on the
// header, the body rows, and the wrapped continuation lines. This is what keeps
// a table's highlight aligned instead of ragged or short of the right edge.
func TestMarkdownTableHighlightRectangular(t *testing.T) {
	doc := "| Name | Description |\n|---|---|\n| alice | a fairly long description that wraps |\n| bob | short |\n| cara | another long description that also wraps here |"
	for _, width := range []int{24, 40} {
		var tr transcript
		tr.cwd = "/tmp"
		tr.add(block{kind: blockAssistant, text: doc})
		wantLo, wantHi := -1, -1
		for _, line := range tr.linesFor(width) {
			lo, hi, contiguous := highlightRun(line)
			if hi <= 0 {
				continue // an untinted row or a non-table line
			}
			// Column 0 is the slab's own padding cell, so the tint begins at
			// the first content column.
			if lo != 1 || !contiguous {
				t.Fatalf("width=%d: highlight run [%d,%d) contiguous=%v, want one run from column 1 on line %q",
					width, lo, hi, contiguous, plain(line))
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
// row background, or hi == 0 when the line carries no table tint. The range
// spans the first to the last tinted cell, so contiguous reports whether every
// cell inside it is tinted; a gap would show as a hole in the row.
func highlightRun(line string) (lo, hi int, contiguous bool) {
	tinted := func(bg string) bool {
		return bg == bgSeq(colorTableHeaderBg) || bg == bgSeq(colorTableRowBg)
	}
	lo, hi, contiguous = -1, 0, true
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
			} else if cell > hi {
				contiguous = false
			}
			hi = cell + w
		}
		cell += w
		i += size
	}
	return lo, hi, contiguous
}

// bgSeq returns the SGR fragment that selects c as a background color, as
// lipgloss emits it in truecolor mode.
func bgSeq(c color.Color) string {
	r, g, b, _ := c.RGBA()
	return fmt.Sprintf("48;2;%d;%d;%d", r>>8, g>>8, b>>8)
}

// fgSeq returns the SGR fragment that selects c as a foreground color, as
// lipgloss emits it in truecolor mode.
func fgSeq(c color.Color) string {
	r, g, b, _ := c.RGBA()
	return fmt.Sprintf("38;2;%d;%d;%d", r>>8, g>>8, b>>8)
}

// TestMarkdownLinkNoControlInjection checks that control characters in a link
// destination never reach the OSC 8 target, where a BEL, ESC, or C1 ST would
// end the sequence early and run the rest of the URL as terminal input.
func TestMarkdownLinkNoControlInjection(t *testing.T) {
	var tr transcript
	tr.cwd = "/tmp"
	tr.add(block{kind: blockAssistant, text: "go [here](https://x.example/a\x07b\x1bc\u009cd) now"})
	out := strings.Join(tr.linesFor(80), "\n")
	if want := "\x1b]8;;https://x.example/abcd\x1b\\"; !strings.Contains(out, want) {
		t.Fatalf("link target not stripped to %q:\n%q", want, out)
	}
}

// TestPrintMarkdownMatchesRender checks that printing a document streamed in
// one byte at a time shows the same lines a from-scratch render does, wrapped
// or not.
func TestPrintMarkdownMatchesRender(t *testing.T) {
	doc := "# Heading\n\nprose with **bold** and a [link](https://example.com) " +
		strings.Repeat("that wraps ", 12) + "\n\n- one\n- [x] two\n\n> quoted\n\n" +
		"| a | b |\n|---|---|\n| 1 | 2 |\n\n---\n\n```go\nfunc main() {}\n```"
	for _, width := range []int{0, 20, 80} {
		var out strings.Builder
		if err := PrintMarkdown(&out, iotest.OneByteReader(strings.NewReader(doc)), width); err != nil {
			t.Fatal(err)
		}
		var want strings.Builder
		for _, line := range markdown.Render(doc, markdown.Theme{}, width) {
			want.WriteString(line.Text + "\n")
		}
		if got := ansi.Strip(out.String()); got != want.String() {
			t.Fatalf("width=%d\n got=%q\nwant=%q", width, got, want.String())
		}
	}
}

// TestPrintMarkdownStreamsClosedBlocks checks that a block prints once it
// closes rather than when input ends, which is what lets a reply piped in
// from a model render while it is written.
func TestPrintMarkdownStreamsClosedBlocks(t *testing.T) {
	in, feed := io.Pipe()
	out, printed := io.Pipe()
	t.Cleanup(func() { feed.Close(); out.Close() })
	done := make(chan error, 1)
	go func() {
		done <- PrintMarkdown(printed, in, 40)
		printed.Close()
	}()
	// A printer that held blocks until input ended would block the reads
	// below for good; fail it instead.
	timeout := time.AfterFunc(5*time.Second, func() { out.Close() })
	defer timeout.Stop()
	lines := bufio.NewScanner(out)
	next := func(want string) {
		t.Helper()
		if !lines.Scan() {
			t.Fatalf("output ended before %q: %v", want, lines.Err())
		}
		if got := ansi.Strip(lines.Text()); got != want {
			t.Fatalf("printed %q, want %q", got, want)
		}
	}
	// Two blank lines separate the paragraphs, as where kon run's separator
	// follows a message that already ended in a blank line.
	if _, err := io.WriteString(feed, "# Title\n\nfirst paragraph\n\n\nsecond"); err != nil {
		t.Fatal(err)
	}
	next("Title")
	next("")
	next("first paragraph")
	feed.Close()
	next("")
	next("second")
	if lines.Scan() {
		t.Fatalf("printed %q after the last block", lines.Text())
	}
	if err := <-done; err != nil {
		t.Fatal(err)
	}
}

// TestMarkdownPrinterLooksOncePerInterval checks that blocks closed less than
// printInterval after the printer last looked wait for a later write or for
// Close, and are never lost.
func TestMarkdownPrinterLooksOncePerInterval(t *testing.T) {
	var out strings.Builder
	printer := NewMarkdownPrinter(&out, 40)
	clock := time.Unix(0, 0)
	printer.now = func() time.Time { return clock }
	printed := func() string { return ansi.Strip(out.String()) }
	write := func(text string, after time.Duration) {
		t.Helper()
		clock = clock.Add(after)
		if _, err := printer.Write([]byte(text)); err != nil {
			t.Fatal(err)
		}
	}

	write("first\n\n", 0)
	if got := printed(); got != "first\n" {
		t.Fatalf("after the first write: %q, want the first paragraph", got)
	}
	write("second\n\n", printInterval/2)
	if got := printed(); got != "first\n" {
		t.Fatalf("within the interval: %q, want the second paragraph held back", got)
	}
	write("third", printInterval/2)
	if got := printed(); got != "first\n\nsecond\n" {
		t.Fatalf("after the interval: %q, want the second paragraph", got)
	}
	if err := printer.Close(); err != nil {
		t.Fatal(err)
	}
	if got := printed(); got != "first\n\nsecond\n\nthird\n" {
		t.Fatalf("after Close: %q, want every paragraph", got)
	}
}

// TestPrintMarkdownLeavesProseUnstyled checks that plain prose prints in the
// terminal's own color, with no escapes, and that empty input prints nothing.
func TestPrintMarkdownLeavesProseUnstyled(t *testing.T) {
	for in, want := range map[string]string{"just some words": "just some words\n", "": ""} {
		var out strings.Builder
		if err := PrintMarkdown(&out, strings.NewReader(in), 80); err != nil {
			t.Fatal(err)
		}
		if out.String() != want {
			t.Fatalf("PrintMarkdown(%q) = %q, want %q", in, out.String(), want)
		}
	}
}
