package markdown

import (
	"strconv"
	"strings"
	"testing"
)

// TestInlineSpans checks that inline markdown produces the right text and
// style spans, and that spans never overlap or exceed the line text.
func TestInlineSpans(t *testing.T) {
	cases := []struct {
		name     string
		in       string
		wantText string
		wantSpan []Styled
	}{
		{
			name:     "emphasis",
			in:       "a *em* b",
			wantText: "a em b",
			wantSpan: []Styled{{Text: "em", Style: StyleEmph}},
		},
		{
			name:     "strong",
			in:       "a **strong** b",
			wantText: "a strong b",
			wantSpan: []Styled{{Text: "strong", Style: StyleStrong}},
		},
		{
			name:     "inline code",
			in:       "a `x := 1` b",
			wantText: "a x := 1 b",
			wantSpan: []Styled{{Text: "x := 1", Style: StyleCodeInline}},
		},
		{
			name:     "code span trims one edge space",
			in:       "a ` code ` b",
			wantText: "a code b",
			wantSpan: []Styled{{Text: "code", Style: StyleCodeInline}},
		},
		{
			name:     "strikethrough",
			in:       "a ~~gone~~ b",
			wantText: "a gone b",
			wantSpan: []Styled{{Text: "gone", Style: StyleStrikethrough}},
		},
		{
			name:     "link keeps text",
			in:       "a [label](http://x) b",
			wantText: "a label b",
			wantSpan: []Styled{{Text: "label", Style: StyleLink}},
		},
		{
			name:     "image alt text",
			in:       "a ![pic](img.png) b",
			wantText: "a pic b",
			wantSpan: []Styled{{Text: "pic", Style: StyleLink}},
		},
		{
			name:     "entities unescaped",
			in:       "a &amp; b &lt;tag&gt; &#65;",
			wantText: "a & b <tag> A",
		},
		{
			name:     "backslash escape",
			in:       `a \*literal\* b`,
			wantText: "a *literal* b",
		},
		{
			name:     "raw html dropped",
			in:       "a <b>bold</b> c",
			wantText: "a bold c",
		},
		{
			name:     "emph spanning spaces is one span",
			in:       "*two words*",
			wantText: "two words",
			wantSpan: []Styled{{Text: "two words", Style: StyleEmph}},
		},
		{
			name:     "nested strong in emph",
			in:       "*a **b** c*",
			wantText: "a b c",
			wantSpan: []Styled{{Text: "a ", Style: StyleEmph}, {Text: "b", Style: StyleStrong}, {Text: " c", Style: StyleEmph}},
		},
		{
			name:     "plain paragraph has no spans",
			in:       "just plain words here",
			wantText: "just plain words here",
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			lines := Render(tc.in, testTheme, 80)
			var got strings.Builder
			var spans []Styled
			for _, l := range lines {
				got.WriteString(l.Text)
				spans = append(spans, l.Spans...)
			}
			if got.String() != tc.wantText {
				t.Fatalf("text=%q want=%q", got.String(), tc.wantText)
			}
			if len(spans) != len(tc.wantSpan) {
				t.Fatalf("spans=%v want=%v", spans, tc.wantSpan)
			}
			for i := range spans {
				if spans[i] != tc.wantSpan[i] {
					t.Fatalf("span[%d]=%v want=%v", i, spans[i], tc.wantSpan[i])
				}
			}
			// Spans must be substrings of their line and never overlap.
			for _, l := range lines {
				pos := 0
				for _, sp := range l.Spans {
					idx := strings.Index(l.Text[pos:], sp.Text)
					if idx < 0 {
						t.Fatalf("span %q not found in line %q after %d", sp.Text, l.Text, pos)
					}
					pos += idx + len(sp.Text)
				}
			}
		})
	}
}

// TestInlineSpansSurviveWrap checks that wrapping a styled paragraph keeps
// each span's text intact and in order across the break, and that a span
// never crosses a line boundary.
func TestInlineSpansSurviveWrap(t *testing.T) {
	in := "start *the emphasized words here* middle **the strong run here** end"
	for _, width := range []int{10, 16, 24, 40, 80} {
		lines := Render(in, testTheme, width)
		var all strings.Builder
		for _, l := range lines {
			all.WriteString(l.Text)
			all.WriteString(" ")
			for _, sp := range l.Spans {
				if !strings.Contains(l.Text, sp.Text) {
					t.Fatalf("width=%d span %q not within line %q", width, sp.Text, l.Text)
				}
			}
		}
		// Every word from the source must survive wrapping.
		for _, w := range []string{"start", "the", "emphasized", "words", "here", "middle", "strong", "run", "end"} {
			if !strings.Contains(all.String(), w) {
				t.Fatalf("width=%d lost word %q in %q", width, w, all.String())
			}
		}
	}
}

// TestInlineSpansStreamingConverge checks the inline layer obeys the same
// streaming invariant: a streamed styled message equals the from-scratch
// render at every frame.
func TestInlineSpansStreamingConverge(t *testing.T) {
	docs := []string{
		"a *em* and **strong** and `code` mixed together\n\n",
		"paragraph with *styled* text that is long enough to wrap at narrow widths\n\n",
		"`code` &amp; *both* at once\n\n",
		"> quoted *emphasis* inside\n\n",
	}
	for _, doc := range docs {
		for _, width := range []int{12, 30, 80} {
			s := NewStream(testTheme, width)
			for i := 0; i < len(doc); i++ {
				s.Write(doc[i : i+1])
			}
			got := linesEqual(streamView(s))
			want := linesEqual(Render(doc, testTheme, width))
			if got != want {
				t.Fatalf("width=%d doc=%q\n got=%s\nwant=%s", width, doc, got, want)
			}
		}
	}
}

// linesEqual renders lines with their spans into a comparable string.
func linesEqual(lines []Line) string {
	var b strings.Builder
	for _, l := range lines {
		b.WriteString(l.Text)
		for _, sp := range l.Spans {
			b.WriteString("|")
			b.WriteString(strconv.Itoa(int(sp.Style)))
			b.WriteString(":")
			b.WriteString(sp.Text)
		}
		b.WriteString("\n")
	}
	return b.String()
}
