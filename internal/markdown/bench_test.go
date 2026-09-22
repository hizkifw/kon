package markdown

import (
	"strings"
	"testing"
)

// benchDoc builds a representative transcript document: headings, prose,
// lists, quotes, and fences like a coding-agent conversation.
func benchDoc(blocks int) string {
	var b strings.Builder
	for i := 0; i < blocks; i++ {
		switch i % 5 {
		case 0:
			b.WriteString("## Section heading\n\n")
		case 1:
			b.WriteString("The assistant explains the change with a paragraph long enough to wrap at narrow terminal widths.\n\n")
		case 2:
			b.WriteString("- first item\n- second item\n- third item\n\n")
		case 3:
			b.WriteString("> A quoted note about the change.\n\n")
		case 4:
			b.WriteString("```go\nfunc main() {\n\tfmt.Println(\"done\")\n}\n```\n\n")
		}
	}
	return b.String()
}

var sink []Line

// BenchmarkRenderFull is the from-scratch reference: parse + render the whole
// document, like the transcript's from-scratch path.
func BenchmarkRenderFull(b *testing.B) {
	for _, blocks := range []int{10, 100, 500} {
		doc := benchDoc(blocks)
		b.Run(itoa(blocks), func(b *testing.B) {
			b.ReportAllocs()
			for i := 0; i < b.N; i++ {
				sink = Render(doc, testTheme, 80)
			}
		})
	}
}

// BenchmarkRenderAppend measures the incremental path's cost after appending
// one block to a settled document: fold the new block and re-emit. This is
// the streaming architecture's steady-state cost.
//
// The growth adds one heading + paragraph block per iteration, so the frozen
// prefix grows ~68 bytes per iteration and the benchmark reports the average
// over a stream that reaches ~140 KB at 2000 iterations. Allocation count
// stays flat, which is the property under test; the ns/op figure is
// dominated by the growing reparse slice, not the render.
func BenchmarkRenderAppend(b *testing.B) {
	for _, blocks := range []int{10, 100, 500} {
		doc := benchDoc(blocks)
		// One extra block arrives per iteration.
		growth := "## New heading\n\nParagraph body with several words.\n\n"
		b.Run(itoa(blocks), func(b *testing.B) {
			b.ReportAllocs()
			s := NewStream(testTheme, 80)
			s.Write(doc)
			s.Lines()
			b.ResetTimer()
			for i := 0; i < b.N; i++ {
				s.Write(growth)
				sink = s.Lines()
				sink2 = s.Pending()
			}
		})
	}
}

// BenchmarkStreamDelta measures the per-frame cost while one long message
// streams. The delta is a sentence with no blank line, so nothing new
// freezes and the tail grows across iterations — this measures the O(tail)
// re-parse under the worst realistic shape (a model streaming one long
// paragraph). Blank-separated messages follow the flat profile of
// BenchmarkStreamParagraphDelta instead. See the Stream doc comment.
func BenchmarkStreamDelta(b *testing.B) {
	doc := benchDoc(100)
	s := NewStream(testTheme, 80)
	s.Write(doc)
	s.Lines()
	delta := "Another streamed sentence with several words in it. "
	b.ResetTimer()
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		s.Write(delta)
		sink = s.Lines()
		sink2 = s.Pending()
	}
}

// BenchmarkStreamParagraphDelta measures the same loop with a blank-
// terminated delta: each paragraph freezes, so the tail stays ~one
// paragraph and the per-frame cost is flat. This is the common shape of
// model output.
func BenchmarkStreamParagraphDelta(b *testing.B) {
	s := NewStream(testTheme, 80)
	s.Write(benchDoc(100))
	s.Lines()
	para := "A paragraph complete with blank line after it.\n\n"
	b.ResetTimer()
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		s.Write(para)
		sink = s.Lines()
		sink2 = s.Pending()
	}
}

// BenchmarkStreamOneParagraph measures the worst case the tail design allows:
// one paragraph with no blank separators, so nothing ever freezes and every
// frame re-parses the whole message. O(n^2) across the stream by design;
// the counterweight is that real model output blanks between blocks, and a
// Finish at finalize is one O(n) pass.
func BenchmarkStreamOneParagraph(b *testing.B) {
	s := NewStream(testTheme, 80)
	sentence := "Streaming one very long paragraph with no blank lines at all. "
	b.ResetTimer()
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		s.Write(sentence)
		sink = s.Lines()
		sink2 = s.Pending()
	}
}

// BenchmarkStreamFinish measures the finalize pass: freeze everything and
// render, the O(n) cost a stream pays exactly once at the end.
func BenchmarkStreamFinish(b *testing.B) {
	doc := benchDoc(100)
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		s := NewStream(testTheme, 80)
		s.Write(doc)
		s.Finish()
		sink = s.Lines()
	}
}

var sink2 []Line

// BenchmarkStreamDeltaNarrow checks the same at a narrow width where wrap
// pressure is higher.
func BenchmarkStreamDeltaNarrow(b *testing.B) {
	doc := benchDoc(100)
	s := NewStream(testTheme, 30)
	s.Write(doc)
	s.Lines()
	delta := "Another streamed sentence with several words in it. "
	b.ResetTimer()
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		s.Write(delta)
		sink = s.Lines()
		sink2 = s.Pending()
	}
}

// BenchmarkStreamLargeMessage streams one very large message (10k sentences)
// to measure amortized cost when the message blanks between paragraphs (the
// freeze-friendly shape). See BenchmarkStreamOneParagraph for the no-blank
// worst case.
func BenchmarkStreamLargeMessage(b *testing.B) {
	s := NewStream(testTheme, 80)
	sentence := "Streaming a long message should stay linear in size. "
	paragraph := sentence + sentence + sentence + "\n\n"
	b.ResetTimer()
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		s.Write(paragraph)
		if i%64 == 0 {
			sink = s.Lines()
			sink2 = s.Pending()
		}
	}
}

func itoa(n int) string {
	if n == 0 {
		return "0"
	}
	var digits []byte
	for n > 0 {
		digits = append([]byte{byte('0' + n%10)}, digits...)
		n /= 10
	}
	return string(digits)
}
