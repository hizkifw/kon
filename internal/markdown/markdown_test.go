package markdown

import (
	"math/rand"
	"strings"
	"testing"
)

// testTheme is the identity mapping: styles pass through unchanged.
var testTheme = Theme{}

func lineTexts(lines []Line) []string {
	out := make([]string, len(lines))
	for i, l := range lines {
		out[i] = l.Text
	}
	return out
}

// TestHeadingStyled checks that a heading line carries a StyleHeading lead
// span (which renderers apply to the whole line) and that inline styling
// inside a heading is preserved alongside it.
func TestHeadingStyled(t *testing.T) {
	lines := Render("# Title", testTheme, 40)
	if len(lines) != 1 {
		t.Fatalf("heading lines=%d want 1", len(lines))
	}
	if len(lines[0].Spans) == 0 || lines[0].Spans[0].Style != StyleHeading {
		t.Fatalf("heading lead span = %v, want StyleHeading marker", lines[0].Spans)
	}
	if lines[0].Spans[0].Text != "" {
		t.Fatalf("heading marker should be zero-width, got %q", lines[0].Spans[0].Text)
	}

	// A heading with inline emphasis keeps the emphasis span after the marker.
	lines = Render("## Sub *em* end", testTheme, 40)
	found := false
	for _, sp := range lines[0].Spans {
		if sp.Style == StyleEmph && sp.Text == "em" {
			found = true
		}
	}
	if !found {
		t.Fatalf("heading inline emphasis lost: %v", lines[0].Spans)
	}
}

// TestRenderBasics covers each block kind's from-scratch output.
func TestRenderBasics(t *testing.T) {
	cases := []struct {
		name  string
		in    string
		want  []string
		width int
	}{
		{"empty", "", []string{""}, 40},
		{"paragraph", "hello world", []string{"hello world"}, 40},
		{"paragraph wrap", "the quick brown fox jumps over the lazy dog", []string{
			"the quick brown fox jumps over the",
			"lazy dog",
		}, 36},
		{"heading", "# Title", []string{"Title"}, 40},
		{"heading deep", "### Smaller title", []string{"Smaller title"}, 40},
		{"rule", "---", []string{strings.Repeat("─", 40)}, 40},
		{"fence", "```go\nfmt.Println()\n```", []string{"fmt.Println()"}, 40},
		{"fence unclosed", "```\nstill typing", []string{"still typing"}, 40},
		{"indented code", "    indented code", []string{"indented code"}, 40},
		{"quote", "> quoted words", []string{"▏ quoted words"}, 40},
		{"bullet", "- one\n- two", []string{"• one", "• two"}, 40},
		{"ordered", "1. one\n2. two", []string{"1. one", "2. two"}, 40},
		{"loose list", "- one\n\n- two", []string{"• one", "", "• two"}, 40}, // interior blank kept
		{"nested list", "- a\n  - b", []string{"• a", "  • b"}, 40},
		{"para in list", "- first para\n  continued\n- second", []string{
			"• first para continued",
			"• second",
		}, 40},
		{"soft break joins", "one\ntwo", []string{"one two"}, 40},
		{"hard break keeps", "one  \ntwo", []string{"one", "two"}, 40},
		{"two paragraphs", "first\n\nsecond", []string{"first", "", "second"}, 40},
		{"heading then para", "# Title\n\nbody text", []string{"Title", "", "body text"}, 40},
		{"para then list", "intro\n\n- a\n- b", []string{"intro", "", "• a", "• b"}, 40},
		{"list then para", "- a\n- b\n\nafter", []string{"• a", "• b", "", "after"}, 40},
		{"para then table", "intro\n\n| a | b |\n|---|---|\n| 1 | 2 |", []string{"intro", "", "a  b", "1  2"}, 40},
		{"para then quote", "intro\n\n> quoted", []string{"intro", "", "▏ quoted"}, 40},
		{"para then fence", "intro\n\n```go\nx()\n```", []string{"intro", "", "x()"}, 40},
		{"quote two paras", "> one\n>\n> two", []string{"▏ one", "▏", "▏ two"}, 40},
		{"three blocks", "# H\n\npara\n\n- a", []string{"H", "", "para", "", "• a"}, 40},
		{"setext", "Title\n=====", []string{"Title"}, 40},
		{"strikethrough para", "~~gone~~", []string{"gone"}, 40},
		{"table", "| a | b |\n|---|---|\n| 1 | 2 |", []string{"a  b", "1  2"}, 40},
		{"task", "- [x] done\n- [ ] not", []string{"• [✓] done", "• [ ] not"}, 40},
		{"html block", "<div>raw</div>", []string{"<div>raw</div>"}, 40},
		{"inline html kept", "a <b>bold</b> c", []string{"a <b>bold</b> c"}, 40},
		{"cjk wrap", "你好世界再见", []string{"你好世界再", "见"}, 10},
		{"emoji", "emoji 😀 test", []string{"emoji 😀 test"}, 40},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			got := lineTexts(Render(tc.in, testTheme, tc.width))
			if !equalSlices(got, tc.want) {
				t.Fatalf("input=%q\n got=%q\nwant=%q", tc.in, got, tc.want)
			}
		})
	}
}

// streamView returns the full current view: frozen lines plus the live tail.
func streamView(s *Stream) []Line {
	frozen := s.Lines()
	pending := s.Pending()
	out := make([]Line, 0, len(frozen)+len(pending))
	out = append(out, frozen...)
	out = append(out, pending...)
	if len(out) == 0 {
		out = append(out, Plain(""))
	}
	return out
}

// TestStreamConvergesToRender is the core invariant: feeding a document as a
// sequence of deltas and then rendering must produce exactly the from-scratch
// render, with frozen prefix lines byte-identical along the way. The
// unfinished-stream view (Frozen + Pending) and the Finish()ed view must both
// equal Render of the same text.
func TestStreamConvergesToRender(t *testing.T) {
	docs := []string{
		"# Heading\n\ndefinition of the plan.\n\n```go\ncode()\n```\n\n- item one\n- item two\n\n> quoted\n\nfinal paragraph.",
		"just one paragraph",
		"# One\n\n## Two\n\n### Three\n\ntext under",
		"- a\n- b\n- c\n",
		"para one\n\npara two\n\npara three",
		"> quote line 1\n> quote line 2\n\nafter quote",
		"```\nunclosed fence tail",
		"word",
		"",
		"| a | b |\n|---|---|\n| 1 | 2 |\n\nafter table",
		"你好世界\n\n第二段",
		"1. first\n2. second\n   - nested\n3. third",
	}
	for _, doc := range docs {
		for _, width := range []int{20, 40, 80} {
			for _, chunk := range []int{1, 3, 7} {
				want := lineTexts(Render(doc, testTheme, width))
				s := NewStream(testTheme, width)
				for i := 0; i < len(doc); i += chunk {
					end := min(i+chunk, len(doc))
					s.Write(doc[i:end])
					// Unfinished view must equal Render of the same bytes.
					got := lineTexts(streamView(s))
					wantNow := lineTexts(Render(doc[:end], testTheme, width))
					if !equalSlices(got, wantNow) {
						t.Fatalf("mid-stream doc=%q width=%d chunk=%d\n got=%q\nwant=%q", doc[:end], width, chunk, got, wantNow)
					}
				}
				// Finish must not change the view: after Finish, Pending is
				// empty and Lines carries everything. An empty document
				// still renders as one empty line (Render's contract).
				s.Finish()
				got := lineTexts(s.Lines())
				if len(got) == 0 {
					got = []string{""}
				}
				if !equalSlices(got, want) {
					t.Fatalf("doc=%q width=%d chunk=%d\n got=%q\nwant=%q", doc, width, chunk, got, want)
				}
			}
		}
	}
}

// TestStreamFrozenPrefixStable asserts the append-only property: lines already
// published as frozen never change as more deltas arrive, and the frozen
// prefix only ever grows.
func TestStreamFrozenPrefixStable(t *testing.T) {
	doc := "# Title\n\nFirst paragraph with several words.\n\n- list one\n- list two\n\n```go\nx()\n```\n\nAfter the fence."
	s := NewStream(testTheme, 40)
	var published []string
	for i := 0; i < len(doc); i++ {
		s.Write(doc[i : i+1])
		frozen := lineTexts(s.Lines())
		if len(frozen) < len(published) {
			t.Fatalf("frame %d: published lines shrank: %d < %d", i, len(frozen), len(published))
		}
		for j := range published {
			if frozen[j] != published[j] {
				t.Fatalf("frame %d: frozen line %d changed\n old=%q\n new=%q", i, j, published[j], frozen[j])
			}
		}
		published = frozen
	}
}

// TestStreamPartialTailVisible checks the live tail shows growing text.
func TestStreamPartialTailVisible(t *testing.T) {
	s := NewStream(testTheme, 40)
	s.Write("hello wor")
	got := lineTexts(s.Pending())
	if len(got) == 0 || !strings.HasPrefix(got[len(got)-1], "hello wor") {
		t.Fatalf("partial tail missing: %q", got)
	}
	s.Write("ld")
	got = lineTexts(s.Pending())
	if len(got) == 0 || !strings.HasPrefix(got[len(got)-1], "hello world") {
		t.Fatalf("grown tail missing: %q", got)
	}
}

// TestBoundaryMonotonic ensures the freeze boundary never moves backwards.
func TestBoundaryMonotonic(t *testing.T) {
	doc := "one\n\ntwo\n\nthree\n\nfour"
	s := NewStream(testTheme, 40)
	last := 0
	for i := 0; i < len(doc); i++ {
		s.Write(doc[i : i+1])
		if s.Boundary() < last {
			t.Fatalf("boundary moved backwards at frame %d: %d < %d", i, s.Boundary(), last)
		}
		last = s.Boundary()
	}
}

// TestStripperDropsOSC8 removes OSC 8 hyperlink sequences, including ones
// split across deltas.
func TestStripperDropsOSC8(t *testing.T) {
	cases := []struct {
		name string
		in   string
		want string
	}{
		{"bel", "a\x1b]8;;http://x\x07b", "ab"},
		{"st", "a\x1b]8;;http://x\x1b\\b", "ab"},
		{"plain", "no escapes", "no escapes"},
		{"esc alone", "a\x1b[0mb", "a\x1b[0mb"}, // CSI is not OSC; kept
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			var s oscStripper
			if got := s.strip(tc.in); got != tc.want {
				t.Fatalf("got=%q want=%q", got, tc.want)
			}
		})
	}
	for _, chunk := range []int{1, 2, 3} {
		var s oscStripper
		var b strings.Builder
		in := "a\x1b]8;;http://x\x1b\\b"
		for i := 0; i < len(in); i += chunk {
			end := min(i+chunk, len(in))
			b.WriteString(s.strip(in[i:end]))
		}
		if got := b.String(); got != "ab" {
			t.Fatalf("chunk=%d got=%q want=%q", chunk, got, "ab")
		}
	}
}

// TestBlockSpacing checks that adjacent top-level blocks are separated by a
// blank line (and a single block is not padded), covering headings, lists,
// tables, quotes, code, and rules.
func TestBlockSpacing(t *testing.T) {
	cases := []struct {
		name string
		in   string
		want []string
	}{
		{
			"heading body list table quote fence",
			"# H\n\npara\n\n- a\n- b\n\n| x |\n|---|\n| 1 |\n\n> q\n\n```\nc\n```",
			[]string{"H", "", "para", "", "• a", "• b", "", "x", "1", "", "▏ q", "", "c"},
		},
		{"single block no pad", "only one paragraph", []string{"only one paragraph"}},
		{"rule between paras", "a\n\n---\n\nb", []string{"a", "", strings.Repeat("─", 40), "", "b"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			got := lineTexts(Render(tc.in, testTheme, 40))
			if !equalSlices(got, tc.want) {
				t.Fatalf("input=%q\n got=%q\nwant=%q", tc.in, got, tc.want)
			}
		})
	}
}

// TestBlockSpacingStreamConverges checks spacing survives streaming: the
// unfinished view (frozen + pending) equals a from-scratch render at every
// frame, including the blank lines between blocks.
func TestBlockSpacingStreamConverges(t *testing.T) {
	doc := "# H\n\none\n\ntwo\n\n- a\n- b\n\n| x |\n|---|\n| 1 |\n\n> q\n\nend"
	for _, width := range []int{20, 40} {
		s := NewStream(testTheme, width)
		for i := 0; i < len(doc); i++ {
			end := i + 1
			s.Write(doc[i:end])
			got := lineTexts(streamView(s))
			want := lineTexts(Render(doc[:end], testTheme, width))
			if !equalSlices(got, want) {
				t.Fatalf("width=%d offset=%d doc=%q\n got=%q\nwant=%q", width, end, doc[:end], got, want)
			}
		}
	}
}

// TestRenderDeterministic verifies Render agrees with itself across parser
// runs (determinism).
func TestRenderDeterministic(t *testing.T) {
	doc := "# H\n\ntext **bold** more\n\n- a\n- b\n\n> q\n\n```\nc\n```\n"
	first := lineTexts(Render(doc, testTheme, 40))
	for i := 0; i < 5; i++ {
		if got := lineTexts(Render(doc, testTheme, 40)); !equalSlices(got, first) {
			t.Fatalf("run %d differs\n got=%q\nwant=%q", i, got, first)
		}
	}
}

// continuationDocs exercises blocks that can still grow across blank lines:
// unclosed fences spanning blanks, lists merging, blockquote merging, lazy
// continuation, and blank-line-interior list items.
func continuationDocs() []string {
	docs := []string{
		"```\ncode\n\nstill same fence\n\n```",
		"- a\n\n- b continues same list?",
		"> quote\n\n> another quote block",
		"- a\n\n  lazy para still in item",
		"# h\n## h2 after single newline?",
		"> quote\n\nlazy para outside",
		"para\n\n```go\ncode\n```\n\nafter",
		"<div>\nraw html\n</div>\n\npara",
		"<div>\nraw\n\nmore html\n\npara",
		"para\n\n- list after",
		"para text\n> lazy quote continuation",
	}

	// Review-repro regressions: blank-spanning containers (lists and
	// indented code continue across blank lines; blockquotes resume
	// mid-line).
	docs = append(docs,
		"x\n\n- \n\n  - a\n  - b\n",
		"- b\n\n# H\n\n- \n\n  - nested\n  - nested\n",
		"- a\n\n- b",
		"- a\n\n- b\n\n# H",
		"- a\n\n- b\n\npara after list",
		"    code one\n\n    code two",
		"    code one\n\n    code two\n\n# H",
		"> q1\n\n  lazy q2",
		"para\n\npara2",
		"<div>\nraw\n\npara",
		"<script>\nvar x;\n\nstill script\n\npara",
		"x\n\n- a\n\n  cont\n\n- c\n\n  - deep\n  - deeper\n\n# Done",
		"## Sub heading\n\n<script>\nvar x;\n\nstill script\n\n# Heading\n\n| ",
	)
	return docs
}

func TestStreamConvergesContinuations(t *testing.T) {
	docs := continuationDocs()
	for _, doc := range docs {
		for _, width := range []int{20, 40} {
			s := NewStream(testTheme, width)
			for i := 0; i < len(doc); i++ {
				s.Write(doc[i : i+1])
			}
			got := lineTexts(streamView(s))
			want := lineTexts(Render(doc, testTheme, width))
			if !equalSlices(got, want) {
				t.Fatalf("doc=%q width=%d\n got=%q\nwant=%q", doc, width, got, want)
			}
		}
	}
}

// TestStreamConvergesRandomized generates random documents from markdown
// fragments and verifies convergence for every document at several chunk
// sizes. Fragments stress blank-spanning containers (empty list items,
// indented continuations, unclosed fences, type-1 HTML) — the shapes that
// broke earlier freeze rules.
func TestStreamConvergesRandomized(t *testing.T) {
	fragments := []string{
		"# Heading\n\n",
		"## Sub heading\n\n",
		"Plain paragraph with several words in it.\n\n",
		"Paragraph two with **bold** and `code` spans.\n\n",
		"- bullet one\n- bullet two\n\n",
		"1. first\n2. second\n\n",
		"- outer\n  - nested\n\n",
		"> quoted line\n> more quote\n\n",
		"```go\nsome.Code()\n```\n\n",
		"```\nunclosed fence\n",
		"---\n\n",
		"| a | b |\n|---|---|\n| 1 | 2 |\n\n",
		"- [x] done task\n- [ ] open task\n\n",
		"setext heading\n===\n\n",
		"text with <div>raw html</div>\n\n",
		"trailing line without final newline",
		"你好世界 CJK paragraph wrap test\n\n",
		// blank-spanning hazards
		"- \n\n",
		"- \n\n  - x\n\n",
		"- a\n\n  continuation\n\n",
		"    indented code\n\n",
		"x\n\n- \n\n  - a\n  - b\n\n",
		"- b\n\n# H\n\n- \n\n  - nested\n  - nested\n\n",
		"<script>\nvar x;\n\nstill script\n\n",
		"```\nunclosed\n\nmore\n\n",
		// ordered markers, whole and partial: a stream stopping mid-marker
		// ("1", "1.", "12") must not freeze the preceding list
		"1. first\n2. second\n\n",
		"1. x\n\n1. y\n\n",
		"2. y\n\n1. x\n\n",
		// inline styling across the wrap and the freeze boundary
		"words with *emphasis* and **strong** and `code` spans\n\n",
		"a [link](http://example.com) and ~~struck~~ text together\n\n",
		"entities &amp; &lt;tag&gt; and backslash \\* escapes\n\n",
		"\n\n",
	}
	rng := rand.New(rand.NewSource(7))
	for iter := 0; iter < 2000; iter++ {
		var doc strings.Builder
		n := 1 + rng.Intn(6)
		for i := 0; i < n; i++ {
			doc.WriteString(fragments[rng.Intn(len(fragments))])
		}
		text := doc.String()
		width := 5 + rng.Intn(76)
		chunk := 1 + rng.Intn(9)
		want := lineTexts(Render(text, testTheme, width))
		s := NewStream(testTheme, width)
		for i := 0; i < len(text); i += chunk {
			s.Write(text[i:min(i+chunk, len(text))])
			// mid-stream view must equal a from-scratch render of the same
			// bytes — not just at the end, since the transcript paints every
			// frame.
			end := min(i+chunk, len(text))
			got := lineTexts(streamView(s))
			wantNow := lineTexts(Render(text[:end], testTheme, width))
			if !equalSlices(got, wantNow) {
				t.Fatalf("mid-stream iter=%d chunk=%d width=%d doc=%q\n got=%q\nwant=%q", iter, chunk, width, text[:end], got, wantNow)
			}
		}
		// Finish must converge to the full render.
		s.Finish()
		got := lineTexts(s.Lines())
		if len(got) == 0 {
			got = []string{""}
		}
		if !equalSlices(got, want) {
			t.Fatalf("iter=%d chunk=%d width=%d doc=%q\n got=%q\nwant=%q", iter, chunk, width, text, got, want)
		}
		// SetText with the same content must agree too (edit path): compare
		// the full view (frozen + pending), since SetText leaves the tail
		// unfrozen exactly like a stream does.
		s2 := NewStream(testTheme, width)
		s2.SetText(text)
		got2 := lineTexts(streamView(s2))
		if !equalSlices(got2, want) {
			t.Fatalf("SetText iter=%d width=%d doc=%q\n got=%q\nwant=%q", iter, width, text, got2, want)
		}
	}
}

// TestStreamFrozenNeverRewrites walks a randomized document byte-by-byte and
// asserts that once a frozen line is published it never changes afterwards.
func TestStreamFrozenNeverRewrites(t *testing.T) {
	fragments := []string{
		"# Title\n\n",
		"Paragraph body with enough words to wrap around a narrow width.\n\n",
		"- item a\n- item b\n\n",
		"> quote block\n\n",
		"```js\nlet x = 1;\n```\n\n",
		"final words",
	}
	rng := rand.New(rand.NewSource(11))
	for iter := 0; iter < 50; iter++ {
		var doc strings.Builder
		for i := 0; i < 2+rng.Intn(4); i++ {
			doc.WriteString(fragments[rng.Intn(len(fragments))])
		}
		text := doc.String()
		s := NewStream(testTheme, 40)
		var previous []string
		for i := 0; i < len(text); i++ {
			s.Write(text[i : i+1])
			frozen := lineTexts(s.Lines())
			if len(frozen) < len(previous) {
				t.Fatalf("iter=%d frame=%d: published lines shrank %d < %d", iter, i, len(frozen), len(previous))
			}
			for j, f := range previous {
				if frozen[j] != f {
					t.Fatalf("iter=%d frame=%d: frozen line %d rewrote\n old=%q\n new=%q", iter, i, j, f, frozen[j])
				}
			}
			previous = frozen
		}
	}
}

func equalSlices(a, b []string) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

// TestStreamConvergesReviewRepro regressions for blank-spanning containers:
// lists and indented code continue across blank lines, so a blank inside them
// must not become a freeze boundary (the tail would re-parse a tight list and
// drop the inter-item blank). The ordered-list cases cover stream stops at
// partial markers ("1", "12") which must also refuse to freeze.
func TestStreamConvergesReviewRepro(t *testing.T) {
	docs := continuationDocs()
	for _, doc := range docs {
		for _, width := range []int{20, 40, 80} {
			want := lineTexts(Render(doc, testTheme, width))
			s := NewStream(testTheme, width)
			for i := 0; i < len(doc); i++ {
				s.Write(doc[i : i+1])
			}
			got := lineTexts(streamView(s))
			if !equalSlices(got, want) {
				t.Fatalf("doc=%q width=%d\n got=%q\nwant=%q", doc, width, got, want)
			}
			// Finish must converge to the same lines.
			s.Finish()
			got = lineTexts(s.Lines())
			if len(got) == 0 {
				got = []string{""}
			}
			if !equalSlices(got, want) {
				t.Fatalf("finish doc=%q width=%d\n got=%q\nwant=%q", doc, width, got, want)
			}
		}
	}
}

// TestPartialMarkersDoNotFreeze locks the #11 regression: a stream pausing
// mid-marker must not freeze the preceding list, at any stop point.
func TestPartialMarkersDoNotFreeze(t *testing.T) {
	docs := []string{
		"1. x\n\n1. y",
		"1. x\n1. y\n\n2. z",
		"2. y\n\n1. x",
		"10. ten\n\n11. eleven",
		"- a\n\n1. ordered after bullet",
	}
	for _, doc := range docs {
		// Stop the stream at every byte offset and verify the view still
		// equals a from-scratch render of the same bytes.
		for stop := 0; stop <= len(doc); stop++ {
			s := NewStream(testTheme, 40)
			s.Write(doc[:stop])
			got := lineTexts(streamView(s))
			want := lineTexts(Render(doc[:stop], testTheme, 40))
			if !equalSlices(got, want) {
				t.Fatalf("doc=%q stop=%d\n got=%q\nwant=%q", doc[:stop], stop, got, want)
			}
		}
	}
}
