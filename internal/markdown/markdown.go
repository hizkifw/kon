package markdown

import (
	"github.com/yuin/goldmark"
	"github.com/yuin/goldmark/ast"
	"github.com/yuin/goldmark/extension"
	"github.com/yuin/goldmark/text"
)

// md is the configured goldmark instance used for every parse.
var md = goldmark.New(
	goldmark.WithExtensions(
		extension.Strikethrough,
		extension.Table,
		extension.TaskList,
		extension.Linkify,
	),
)

// parse runs goldmark over src and returns the document tree.
func parse(src []byte) ast.Node {
	return md.Parser().Parse(text.NewReader(src))
}

// Stream renders a growing markdown document incrementally. Closed blocks are
// frozen once parsed; only the still-open tail re-renders. Published lines are
// append-only and converge to the same output Render produces for the same
// text (guarded by TestStreamConvergesToRender).
//
// Per-frame cost is proportional to the tail, not the document: the source is
// an append-only byte buffer, reparse sees only the bytes past the freeze
// boundary, and Lines reuses one backing array instead of copying the frozen
// prefix. The same discipline as the transcript's chunk and line caches.
//
// Tail cost is O(tail): text stays unfrozen until a hard boundary (a blank
// line followed by a block opener, or a structural close), so one long
// paragraph with no blank lines re-parses per frame — the same trade kon's
// plainWrapper liveStream makes for prose, with a heavier constant. Callers
// painting a frame that must be complete call Finish (equivalently Render)
// once the stream ends; a Finish'd stream can no longer accept appends.
type Stream struct {
	theme Theme
	width int

	source  []byte // accumulated source, append-only
	blocks  []block
	lines   []Line
	frozenN int
	strip   oscStripper
	done    bool
}

// NewStream builds a stream renderer.
func NewStream(theme Theme, width int) *Stream {
	return &Stream{theme: theme, width: width}
}

// Write appends a delta. OSC 8 sequences are stripped statefully, so one
// split across deltas is dropped whole. Panics after Finish: a finished
// stream is frozen for good; start a new one instead.
func (s *Stream) Write(delta string) {
	if s.done {
		panic("markdown: Write after Finish")
	}
	clean := s.strip.strip(delta)
	s.source = append(s.source, clean...)
}

// Finish freezes every remaining block, as if the document were complete.
// After Finish the stream accepts no more writes; Lines returns the full
// render. Use it when a stream ends, before folding the block into stable
// transcript history.
func (s *Stream) Finish() {
	s.done = true
	s.reparse()
}

// SetText replaces the buffer (non-streaming edit). All freezes are cleared.
func (s *Stream) SetText(text string) {
	s.source = s.source[:0]
	s.blocks = nil
	s.lines = s.lines[:0]
	s.frozenN = 0
	s.strip = oscStripper{}
	s.Write(text)
}

// Boundary returns the byte offset after the last frozen block.
func (s *Stream) Boundary() int {
	if len(s.blocks) == 0 {
		return 0
	}
	return s.blocks[len(s.blocks)-1].end
}

// Lines returns the frozen block lines: everything already closed and
// byte-stable. The slice is append-only across calls (finished lines are
// never revised or removed), and aliases the stream's cache, so the caller
// must not modify it. This mirrors the transcript's linesFor contract, which
// feeds stable lines straight to the scroll container. Empty until the first
// block freezes; a stream with no closed blocks yet publishes nothing (the
// whole document is still in Pending).
func (s *Stream) Lines() []Line { return s.frozenLines() }

// frozenLines re-renders newly closed blocks into the cache and returns the
// frozen prefix.
func (s *Stream) frozenLines() []Line {
	s.reparse()
	return s.lines
}

// Pending returns only the live tail lines, not the frozen prefix. The
// result is fresh memory the caller owns; the full document view is
// append(Lines(), Pending()...). A stream with no closed blocks and no tail
// yet (empty source) renders as one empty line, mirroring Render, so a
// caller can always paint the result directly.
func (s *Stream) Pending() []Line {
	s.reparse()
	out := s.tailLines()
	if len(out) == 0 && len(s.lines) == 0 {
		out = []Line{Plain("")}
	}
	return out
}

// reparse re-renders newly closed tail blocks and freezes them. The tail
// starts at the current freeze boundary; everything before it is already
// rendered, so per-frame cost is proportional to the tail, not the document.
// When the stream is finished (Finish), the whole tail freezes at once.
func (s *Stream) reparse() {
	boundary := s.Boundary()
	if boundary >= len(s.source) {
		return
	}
	tail := s.source[boundary:]
	r := newBlockRenderer(s.theme, s.width)
	s.lines = s.lines[:s.frozenN]
	var closed []block
	if s.done {
		closed = r.renderAll(parse(tail), tail)
	} else {
		closed = r.renderClosed(parse(tail), tail)
	}
	for i := range closed {
		closed[i].end += boundary
	}
	s.blocks = append(s.blocks, closed...)
	s.lines = appendBlocks(s.lines, closed)
	s.frozenN = len(s.lines)
}

// tailLines renders the still-open portion after the frozen prefix. After
// Finish the tail is empty: everything froze. A separator precedes the tail
// when the frozen prefix already emitted output, so the concatenation
// Lines()+Pending() keeps the same inter-block spacing as a lone join.
func (s *Stream) tailLines() []Line {
	if s.done {
		return nil
	}
	tail := s.source[s.Boundary():]
	if len(tail) == 0 {
		return nil
	}
	r := newBlockRenderer(s.theme, s.width)
	lines := appendBlocks(nil, r.renderAll(parse(tail), tail))
	if len(lines) > 0 && len(s.lines) > 0 {
		lines = append(separatorLines(), lines...)
	}
	return lines
}

// Render parses text from scratch and returns display lines for every closed
// block. This is the from-scratch reference the streaming path must converge
// to: for the same source text, Stream.Lines() after the final Write must
// equal Render, block for block.
func Render(text string, theme Theme, width int) []Line {
	r := newBlockRenderer(theme, width)
	source := []byte(text)
	lines := appendBlocks(nil, r.renderAll(parse(source), source))
	if len(lines) == 0 {
		lines = []Line{Plain("")}
	}
	return lines
}
