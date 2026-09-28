package markdown

import (
	"math/rand"
	"slices"
	"strings"
	"testing"
	"unicode"
)

// sourceDocs mix every construct the renderer maps, for the tests that check
// runs across whole documents.
var sourceDocs = []string{
	"# Title\n\nSome **bold _and em_** with `code` and [a link](https://x.test \"t\").\n\n- [ ] task\n  - nested `x`\n\n> quote\n> > deeper\n\n```go\nfunc main() {}\n```\n\n| a | b |\n| - | - |\n| **c** | d |\n\n---\n\n1. one\n2. two\n\n<https://x.test> &amp; \\* [undefined][r]",
	"Paragraph with **bold text and `code block in` the middle** of it, then snake_case_name and a \\*literal\\* star.",
	"alpha beta gamma delta epsilon zeta eta theta iota kappa lambda mu nu xi omicron pi",
	"1. first\n2. second\n\n   more text\n3. third\n\n> - quoted item\n> - another\n\n## Closing\n\ntext &rarr; arrow and <kbd>Ctrl</kbd> key",
	"soft\nbreak and hard  \nbreak, a `span\nacross` lines, ![alt text](i.png) and ~~gone~~",
}

// referenceDoc has links defined apart from where they are used. Excerpt
// leaves the definitions behind, so the tests that re-render an excerpt or a
// stream's frozen text keep away from it.
const referenceDoc = "See [ref][r], [short], and [full][r].\n\n[r]: https://r.test\n[short]: https://s.test"

// showRuns renders a line's text with each run in brackets: ⟨text⟩ matches
// the source byte for byte, ⟪text⟫ was decoded from an escape or reference.
// Text outside brackets is the renderer's own.
func showRuns(l Line) string {
	var b strings.Builder
	at := 0
	for _, r := range l.Runs {
		b.WriteString(l.Text[at:r.At])
		open, close := "⟨", "⟩"
		if !r.exact() {
			open, close = "⟪", "⟫"
		}
		b.WriteString(open + l.Text[r.At:r.At+r.Len] + close)
		at = r.At + r.Len
	}
	b.WriteString(l.Text[at:])
	return b.String()
}

func TestSourceRuns(t *testing.T) {
	cases := []struct {
		name  string
		in    string
		width int
		want  []string
	}{
		{"paragraph", "plain words", 40, []string{"⟨plain words⟩"}},
		{"emphasis", "a **bold** b", 40, []string{"⟨a ⟩⟨bold⟩⟨ b⟩"}},
		{"escapes", "a \\*b\\* c", 40, []string{"⟨a ⟩⟪*⟫⟨b⟩⟪*⟫⟨ c⟩"}},
		{"entity", "x &amp; y", 40, []string{"⟨x ⟩⟪&⟫⟨ y⟩"}},
		{"link destination is the renderer's", "see [docs](https://x.test) now", 40, []string{"⟨see ⟩⟨docs⟩ (https://x.test)⟨ now⟩"}},
		{"autolink", "<https://x.test>", 40, []string{"⟨https://x.test⟩"}},
		{"code span", "run `make check` now", 40, []string{"⟨run ⟩⟨make check⟩⟨ now⟩"}},
		{"code span across a line", "`alpha\nbeta`", 40, []string{"⟨alpha beta⟩"}},
		{"code span across a CRLF", "`alpha\r\nbeta`", 40, []string{"⟨alpha⟩⟪ ⟫⟨beta⟩"}},
		{"image alt", "![alt](x.png)", 40, []string{"⟨alt⟩"}},
		{"raw HTML", "a <kbd>b</kbd>", 40, []string{"⟨a <kbd>b</kbd>⟩"}},
		{"reference link", "[ref][r] x\n\n[r]: https://r.test", 40, []string{"⟨ref⟩ (https://r.test)⟨ x⟩"}},
		{"soft break is the renderer's", "one\ntwo", 40, []string{"⟨one⟩ ⟨two⟩"}},
		{"wrapped", "alpha beta gamma", 11, []string{"⟨alpha beta⟩", "⟨gamma⟩"}},
		{"hard split", "abcdefghij", 4, []string{"⟨abcd⟩", "⟨efgh⟩", "⟨ij⟩"}},
		{"heading", "## Title", 40, []string{"⟨Title⟩"}},
		{"bullets", "- one\n- two", 40, []string{"• ⟨one⟩", "• ⟨two⟩"}},
		{"ordered", "1. a\n2. b", 40, []string{"1. ⟨a⟩", "2. ⟨b⟩"}},
		{"task", "- [ ] todo", 40, []string{"• [ ] ⟨todo⟩"}},
		{"nested", "- a\n  - b", 40, []string{"• ⟨a⟩", "  • ⟨b⟩"}},
		{"item continuation", "- one two three", 9, []string{"• ⟨one two⟩", "  ⟨three⟩"}},
		{"quote", "> quoted\n>\n> more", 40, []string{"▏ ⟨quoted⟩", "▏", "▏ ⟨more⟩"}},
		{"code block", "```go\nx := 1\n\ty\n```", 40, []string{"⟨x := 1⟩", "⟨\ty⟩"}},
		{"code block wraps", "```\nabcdef\n```", 3, []string{"⟨abc⟩", "⟨def⟩"}},
		{"table", "| a | bb |\n| - | - |\n| ccc | d |", 40, []string{" ⟨a⟩    ⟨bb⟩ ", " ⟨ccc⟩  ⟨d⟩  "}},
		{"narrow table", "| a | b |\n| - | - |\n| c | d |", 4, []string{"⟨a⟩  ⟨b⟩", "⟨c⟩  ⟨d⟩"}},
		{"rule", "a\n\n---\n\nb", 5, []string{"⟨a⟩", "", "─────", "", "⟨b⟩"}},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			var got []string
			for _, l := range RenderWithSource(c.in, testTheme, c.width) {
				got = append(got, showRuns(l))
			}
			if !equalSlices(got, c.want) {
				t.Fatalf("\n got=%q\nwant=%q", got, c.want)
			}
		})
	}
}

// TestSourceRunsPointAtTheirSource checks every run against the source it
// names: in bounds, in order, and for a byte-for-byte run the same bytes, but
// for the line endings and tabs the renderer shows as spaces.
func TestSourceRunsPointAtTheirSource(t *testing.T) {
	spaces := strings.NewReplacer("\n", " ", "\t", " ")
	for _, doc := range append(slices.Clone(sourceDocs), referenceDoc) {
		for _, width := range []int{0, 12, 40, 80} {
			for row, l := range RenderWithSource(doc, testTheme, width) {
				at := 0
				for _, r := range l.Runs {
					if r.At < at || r.Len <= 0 || r.At+r.Len > len(l.Text) || r.Source < 0 || r.SourceLen <= 0 || r.Source+r.SourceLen > len(doc) {
						t.Fatalf("width=%d line %d %q: run %+v out of place", width, row, l.Text, r)
					}
					at = r.At + r.Len
					if !r.exact() {
						continue
					}
					if text, src := l.Text[r.At:at], doc[r.Source:r.Source+r.Len]; spaces.Replace(text) != spaces.Replace(src) {
						t.Fatalf("width=%d line %d: run %+v shows %q for source %q", width, row, r, text, src)
					}
				}
			}
		}
	}
}

// TestRenderWithSourceMatchesRender checks that rendering with the source map
// shows exactly what Render shows, so the map fits what is on screen, and
// that Render leaves the map out.
func TestRenderWithSourceMatchesRender(t *testing.T) {
	for _, doc := range append(slices.Clone(sourceDocs), referenceDoc) {
		for _, width := range []int{0, 12, 40} {
			plain, mapped := Render(doc, testTheme, width), RenderWithSource(doc, testTheme, width)
			if got, want := linesEqual(mapped), linesEqual(plain); got != want {
				t.Fatalf("doc=%q width=%d\n got=%s\nwant=%s", doc, width, got, want)
			}
			for _, l := range plain {
				if l.Runs != nil {
					t.Fatalf("doc=%q width=%d: Render mapped %q", doc, width, l.Text)
				}
			}
		}
	}
}

// TestSelectionSourceToExcerpt drives the whole copy path: a selection marked
// with ⟦ and ⟧ in the rendered text maps to a source range, and Excerpt cuts
// that range out.
func TestSelectionSourceToExcerpt(t *testing.T) {
	cases := []struct {
		name     string
		source   string
		width    int
		rendered string
		want     string
	}{
		{"the first example", "Paragraph with **bold text and `code block in` the middle** of it", 80,
			"Paragraph with bold text and code block⟦ in the middle of⟧ it", "**` in` the middle** of"},
		{"a word", "Some **bold** words", 40, "Some ⟦bold⟧ words", "**bold**"},
		{"part of a link", "[my link here](https://x.test)", 40, "my ⟦link⟧ here (https://x.test)", "[link](https://x.test)"},
		{"into a destination", "[docs](https://x.test) more", 40, "⟦docs (https://x⟧.test) more", "[docs](https://x.test)"},
		{"from a bullet", "- one\n- two", 40, "⟦• one\n• tw⟧o", "- one\n- tw"},
		{"across a wrap", "alpha beta gamma", 11, "alpha ⟦beta\ngam⟧ma", "beta gam"},
		{"escaped text", "a \\*b\\* c", 40, "a ⟦*b*⟧ c", "\\*b\\*"},
		{"inside an entity", "x &rarr; y", 40, "x ⟦→⟧ y", "&rarr;"},
		{"code lines", "```go\nfunc main() {\n\treturn\n}\n```", 40, "func ⟦main() {\n\treturn⟧\n}", "main() {\n\treturn"},
		{"quote to paragraph", "> quoted text\n\nafter it", 40, "▏ quoted ⟦text\n\nafter⟧ it", "> text\n\nafter"},
		{"table rows", "| a | b |\n| - | - |\n| c | d |\n| e | f |", 40, " a  b \n c  ⟦d \n e⟧  f ", "| a | b |\n| - | - |\n| c | d |\n| e | f |"},
		{"only decoration", "- one", 40, "⟦• ⟧one", ""},
		{"part of a CRLF code span", "`alpha\r\nbeta`", 40, "a⟦l⟧pha beta", "`l`"},
		// A wrapped table shows each column's first line before any column's
		// second, so the screen and the source disagree on order.
		{"wrapped table cells", "| aaa bbb | ccc ddd |\n| - | - |\n| x | y |", 12,
			" aaa   ⟦ccc  \n bbb⟧   ddd  \n x     y    ", "| aaa bbb | ccc ddd |\n| - | - |"},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			rendered, from, to := unmark(t, c.rendered)
			lines := RenderWithSource(c.source, testTheme, c.width)
			if got := strings.Join(lineTexts(lines), "\n"); got != rendered {
				t.Fatalf("rendered\n%q\nnot\n%q", got, rendered)
			}
			startRow, startCol := rowCol(rendered, from)
			endRow, endCol := rowCol(rendered, to)
			got := ""
			if start, end, ok := SelectionSource(lines, startRow, startCol, endRow, endCol); ok {
				got = Excerpt(c.source, start, end)
			}
			if got != c.want {
				t.Fatalf("got %q, want %q", got, c.want)
			}
		})
	}
}

// rowCol turns an offset in lines joined by newlines into a line and a byte
// in it.
func rowCol(joined string, off int) (row, col int) {
	row = strings.Count(joined[:off], "\n")
	return row, off - (strings.LastIndex(joined[:off], "\n") + 1)
}

// TestSelectionExcerptShowsTheSelection checks, over many selections, that
// the excerpt of a selection renders all the source text the selection
// showed. The excerpt can show more, since it keeps what makes the text read
// the same: a table's header, a link taken whole.
func TestSelectionExcerptShowsTheSelection(t *testing.T) {
	// mapped returns the text of lines that came from the source, from
	// (startRow, startCol) up to (endRow, endCol), without whitespace.
	mapped := func(lines []Line, startRow, startCol, endRow, endCol int) string {
		var b strings.Builder
		for row := startRow; row <= endRow; row++ {
			from, until := 0, len(lines[row].Text)
			if row == startRow {
				from = startCol
			}
			if row == endRow {
				until = endCol
			}
			for _, r := range lines[row].Runs {
				if a, e := max(r.At, from), min(r.At+r.Len, until); a < e {
					b.WriteString(lines[row].Text[a:e])
				}
			}
		}
		return strings.Map(func(r rune) rune {
			if unicode.IsSpace(r) {
				return -1
			}
			return r
		}, b.String())
	}
	rng := rand.New(rand.NewSource(1))
	for _, doc := range sourceDocs {
		for _, width := range []int{12, 30, 60, 0} {
			lines := RenderWithSource(doc, testTheme, width)
			type pos struct{ row, col int }
			var cuts []pos
			for row, l := range lines {
				for col := range l.Text {
					cuts = append(cuts, pos{row, col})
				}
				cuts = append(cuts, pos{row, len(l.Text)})
			}
			for range 300 {
				a, b := cuts[rng.Intn(len(cuts))], cuts[rng.Intn(len(cuts))]
				if b.row < a.row || b.row == a.row && b.col < a.col {
					a, b = b, a
				}
				selected := mapped(lines, a.row, a.col, b.row, b.col)
				start, end, ok := SelectionSource(lines, a.row, a.col, b.row, b.col)
				if !ok {
					if selected != "" {
						t.Fatalf("doc=%q selection %v-%v shows %q but maps to nothing", doc, a, b, selected)
					}
					continue
				}
				excerpt := Excerpt(doc, start, end)
				again := RenderWithSource(excerpt, testTheme, 0)
				if shown := mapped(again, 0, 0, len(again)-1, len(again[len(again)-1].Text)); !strings.Contains(shown, selected) {
					t.Fatalf("doc=%q selection %v-%v shows %q; its excerpt %q shows %q", doc, a, b, selected, excerpt, shown)
				}
			}
		}
	}
}
