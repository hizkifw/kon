package provider

import (
	"fmt"
	"strings"
	"testing"

	"github.com/hizkifw/kon/internal/session"
)

// TestChatStreamKeepsInterleavedParts guards the stream's shared text buffer:
// starting a new part must not rewrite the text of an earlier one.
func TestChatStreamKeepsInterleavedParts(t *testing.T) {
	events := sse(`{"choices":[{"index":0,"delta":{"content":"one "}}]}`) +
		sse(`{"choices":[{"index":0,"delta":{"content":"two"}}]}`) +
		sse(`{"choices":[{"index":0,"delta":{"reasoning_content":"think"}}]}`) +
		sse(`{"choices":[{"index":0,"delta":{"content":"three"}}]}`) +
		sse(`{"choices":[{"index":0,"delta":{},"finish_reason":"stop"}]}`) +
		"data: [DONE]\n\n"
	response, err := decodeChatStream(strings.NewReader(events), nil)
	if err != nil {
		t.Fatal(err)
	}
	want := []session.Part{
		{Type: session.PartText, Text: "one two"},
		{Type: session.PartReasoning, Text: "think"},
		{Type: session.PartText, Text: "three"},
	}
	if len(response.Parts) != len(want) {
		t.Fatalf("parts = %+v, want %+v", response.Parts, want)
	}
	for i := range want {
		if response.Parts[i].Type != want[i].Type || response.Parts[i].Text != want[i].Text {
			t.Fatalf("part %d = %+v, want %+v", i, response.Parts[i], want[i])
		}
	}
}

// BenchmarkDecodeChatStream decodes one reply of n text deltas. The cost per
// delta must stay flat as n grows: assembling the reply by string
// concatenation once made a 200k-delta reply take 46 seconds.
func BenchmarkDecodeChatStream(b *testing.B) {
	for _, n := range []int{1_000, 10_000, 100_000} {
		var events strings.Builder
		for i := range n {
			events.WriteString(sse(fmt.Sprintf(`{"choices":[{"index":0,"delta":{"content":"word%d "}}]}`, i%10)))
		}
		events.WriteString(sse(`{"choices":[{"index":0,"delta":{},"finish_reason":"stop"}]}`) + "data: [DONE]\n\n")
		stream := events.String()
		b.Run(fmt.Sprintf("deltas=%d", n), func(b *testing.B) {
			b.ReportAllocs()
			for b.Loop() {
				if _, err := decodeChatStream(strings.NewReader(stream), nil); err != nil {
					b.Fatal(err)
				}
			}
			b.ReportMetric(float64(b.Elapsed().Nanoseconds())/float64(b.N*n), "ns/delta")
		})
	}
}
