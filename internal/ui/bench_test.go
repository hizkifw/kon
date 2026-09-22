package ui

import (
	"fmt"
	"strings"
	"testing"

	"github.com/hizkifw/kon/internal/tools"
)

// benchTranscript builds a transcript of n representative blocks, ending with a
// completed tool run so the incremental cache exercises both paths.
func benchTranscript(n int) *transcript {
	tr := &transcript{cwd: "/tmp"}
	for i := 0; i < n; i++ {
		switch i % 3 {
		case 0:
			tr.add(block{kind: blockUser, text: strings.Repeat("user message text here ", 20)})
		case 1:
			tr.add(block{kind: blockAssistant, text: strings.Repeat("assistant reply text here ", 20)})
		case 2:
			tr.add(block{kind: blockTool, name: "read", args: `{"path":"/tmp/a.go"}`, display: tools.Display{State: tools.StateRunning, Summary: "a.go"}})
			tr.add(block{kind: blockResult, name: "read", display: tools.Display{State: tools.StateDone, Summary: "a.go", Note: "12 lines"}})
		}
	}
	return tr
}

// fullRenderReference renders every block from scratch, matching the pre-cache
// behaviour so the incremental path can be compared against it.
func fullRenderReference(t *transcript, width int) string {
	var chunks []string
	var run []block
	flush := func() {
		if len(run) == 0 {
			return
		}
		if text := strings.Join(t.renderToolRun(run, width), "\n"); text != "" {
			chunks = append(chunks, text)
		}
		run = run[:0]
	}
	for _, b := range t.blocks {
		if b.kind == blockTool || b.kind == blockResult {
			run = append(run, b)
			continue
		}
		flush()
		if text := strings.Join(t.renderBlock(b, width), "\n"); text != "" {
			chunks = append(chunks, text)
		}
	}
	flush()
	return strings.Join(chunks, "\n\n")
}

// BenchmarkTranscriptRenderAppend measures the cost of rendering after one new
// block lands, which is what a tool-heavy run does on every event. Each
// iteration copies a warm transcript so the fold state starts from the same
// warm point; mutating the warm transcript in place (the obvious loop) would
// leave built beyond the truncated blocks and measure a no-op render.
func BenchmarkTranscriptRenderAppend(b *testing.B) {
	for _, n := range []int{100, 500, 2000} {
		b.Run(fmt.Sprintf("n=%d", n), func(b *testing.B) {
			warm := benchTranscript(n)
			warm.render(120)
			b.ResetTimer()
			for i := 0; i < b.N; i++ {
				tr := *warm
				tr.add(block{kind: blockAssistant, text: "delta"})
				tr.render(120)
			}
		})
	}
}

// BenchmarkTranscriptRenderStream measures a repaint during streaming, where no
// block is added but the pending stream grows. It drives the production path
// (linesFor) so the incremental live renderer is exercised.
func BenchmarkTranscriptRenderStream(b *testing.B) {
	for _, n := range []int{100, 500, 2000} {
		b.Run(fmt.Sprintf("n=%d", n), func(b *testing.B) {
			tr := benchTranscript(n)
			tr.linesFor(120)
			b.ResetTimer()
			for i := 0; i < b.N; i++ {
				tr.appendStream("more ")
				tr.linesFor(120)
			}
		})
	}
}

// BenchmarkTranscriptRenderLongStream measures repaint cost as a single live
// message grows very long, the O(n^2) case Phase 3 removes.
func BenchmarkTranscriptRenderLongStream(b *testing.B) {
	for _, words := range []int{1000, 10000} {
		b.Run(fmt.Sprintf("words=%d", words), func(b *testing.B) {
			var tr transcript
			tr.cwd = "/tmp"
			tr.appendStream(strings.Repeat("word ", words))
			tr.linesFor(120)
			b.ResetTimer()
			for i := 0; i < b.N; i++ {
				tr.appendStream("more ")
				tr.linesFor(120)
			}
		})
	}
}

// BenchmarkTranscriptRenderFull is the from-scratch reference for comparison.
func BenchmarkTranscriptRenderFull(b *testing.B) {
	for _, n := range []int{100, 500, 2000} {
		b.Run(fmt.Sprintf("n=%d", n), func(b *testing.B) {
			tr := benchTranscript(n)
			b.ResetTimer()
			for i := 0; i < b.N; i++ {
				_ = fullRenderReference(tr, 120)
			}
		})
	}
}

// BenchmarkViewportRefresh measures the remaining per-frame cost once the
// transcript cache is warm: the viewport still rescans the whole document.
func BenchmarkViewportRefresh(b *testing.B) {
	for _, n := range []int{100, 500, 2000} {
		b.Run(fmt.Sprintf("n=%d", n), func(b *testing.B) {
			m := newTestModel(b)
			m.width, m.height = 120, 40
			m.resize()
			m.transcript = *benchTranscript(n)
			m.refreshTranscript(false)
			b.ResetTimer()
			for i := 0; i < b.N; i++ {
				m.refreshTranscript(true)
				_ = m.viewport.View()
			}
		})
	}
}
