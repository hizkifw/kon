package ui

import (
	"image/color"

	"github.com/hizkifw/kon/core/session"
	"github.com/hizkifw/kon/internal/codetools"
	"github.com/hizkifw/kon/internal/tui"
)

type blockKind uint8

const (
	blockUser blockKind = iota
	blockAssistant
	blockThinking
	blockTool
	blockResult
	blockError
	blockContext
	blockModel
	blockModels
	// blockElapsed is a finished turn's "Worked for …" total. It is appended
	// when a run ends and stays in history as the turn's duration.
	blockElapsed
	// blockCompaction is a compaction's summary, whole, with its marker line
	// under it.
	blockCompaction
	// blockOutput is a command's output, one line of text per line, shown as
	// written rather than as Markdown.
	blockOutput
)

// block is one entry in the transcript. Tool calls arrive as a blockTool
// (request) followed by a blockResult (completion); they are paired and
// grouped at render time. Display is the owning tool's presentation snapshot:
// for blockTool it may be replaced by live streaming updates, for blockResult
// it is resolved once from the persisted result.
type block struct {
	kind    blockKind
	name    string // tool name for tool blocks
	args    string // raw JSON arguments for tool blocks
	text    string
	display codetools.Display
	// model is the recorded selection behind a replayed model change, kept so
	// its title can be named again once the catalog loads.
	model *session.ModelSelection
	// marker is the line under a compaction block: what it compacted, or
	// that it stopped.
	marker string
}

type transcript struct {
	blocks   []block
	stream   []byte // buffered live message deltas, kept across frames to avoid re-copying the accumulated message per delta
	thinking string

	// chunks holds the rendered text of every block that can no longer change.
	// A trailing run of tool/result blocks is deliberately left out of chunks
	// because a later call or result may still join it; it is rendered on
	// demand from the tail of blocks. joined caches the concatenation of
	// chunks so a render after many streaming frames does not rejoin them.
	chunks []string
	// chunkFrom holds, for each chunk, the index of the first block folded
	// into it, so a line on screen can be traced back to its message.
	chunkFrom []int
	joined    string
	// joinedChunks is the len(t.chunks) at the time joined was built; it is a
	// memo key, not a count, so re-joining only happens when a chunk was added.
	joinedChunks int
	built        int // number of blocks folded into chunks

	// active renders the live stream (t.stream or t.thinking) incrementally so a
	// repaint during streaming does not re-wrap the whole accumulated message.
	// It is nil when no stream is open; activeThinking selects the renderer.
	// strip removes escapes/control bytes from incoming deltas; the stateful
	// machine survives across deltas so a sequence split between them is
	// dropped whole (see tui.Stripper).
	active         liveRenderer
	activeThinking bool
	strip          tui.Stripper
	// compacting marks t.stream as a compaction summary rather than a reply:
	// it streams in the tool slab's colors and ends as a blockCompaction.
	compacting bool

	// lines is the transcript rendered as a flat slice of display lines, so the
	// viewport never has to split the joined string again. cacheBase records the
	// stable text it was built from and cacheBanner the banner prefix, kept
	// separate so a frame that changes neither never re-concatenates the
	// document; sepDone/liveFin track the append-only live tail (blank
	// separator, finalized lines) so only the current line is repainted.
	lines       []string
	cacheBase   string
	cacheBanner string
	stableN     int
	sepDone     bool
	liveFin     int
	// liveTimer is the running turn's "Working… 12s" indicator, or "" when no
	// turn is in flight. It is rendered as a trailing line after the live tail,
	// so it repaints each second without disturbing the stream caches above it.
	// timerLines is how many trailing t.lines entries belong to it.
	liveTimer  string
	timerLines int

	// selection is the stretch picked with the mouse, nil when there is none.
	// It is anchored in lines, so any change that rebuilds them clears it.
	selection *selection

	dirty bool
	width int
	cwd   string
	// banner is the presentation-only mark that leads every transcript. It is
	// never a block, so it stays out of session records, but it renders as a
	// stable prefix above the conversation even on a resumed session. Its
	// rendering depends only on the mark and the width, so it is memoized.
	banner      banner
	bannerCache string
	bannerWidth int
	bannerValid bool
}

// Lines is the transcript at width, for a drawer showing it.
func (t *transcript) Lines(width int) []string { return t.linesFor(width) }

// Highlight marks the selection, for a drawer showing the transcript, or is
// nil when there is none.
func (t *transcript) Highlight(width int) func(i int, line string) string {
	if t.selection == nil {
		return nil
	}
	return func(i int, line string) string { return t.highlight(i, line, width) }
}

func (t *transcript) add(value block) {
	t.blocks = append(t.blocks, value)
	t.dirty = true
}

// updateToolLive replaces the running call's display with a fresh snapshot
// from the tool. The running call is always the transcript's trailing tool
// block: the agent runs tools one at a time and the result block terminates
// the run. When no trailing tool block matches (a done event already
// finalized the call), the snapshot is dropped.
func (t *transcript) updateToolLive(d codetools.Display) {
	if len(t.blocks) == 0 || t.blocks[len(t.blocks)-1].kind != blockTool {
		return
	}
	t.blocks[len(t.blocks)-1].display = d
	t.dirty = true
}

// liveRenderer renders the not-yet-finalized portion of a live stream
// incrementally. finalized returns append-only finished lines; currentLines
// returns the still-growing tail lines, rebuilt per frame; pending returns
// the whole live portion as one string for the from-scratch reference path.
type liveRenderer interface {
	append(text string)
	finalized() []string
	currentLines() []string
}

// ensureThinking promotes a buffered thinking trace into an incremental live
// stream so subsequent reasoning deltas fold in without a full re-render.
func (t *transcript) ensureThinking() liveRenderer {
	if t.activeThinking && t.active != nil {
		return t.active
	}
	t.active = newThinkingStream(t.width)
	t.activeThinking = true
	return t.active
}

// ensureMessage promotes a buffered assistant message into an incremental live
// stream so subsequent text deltas fold in without a full re-render. Assistant
// messages stream through the markdown renderer so formatting appears as it
// arrives; thinking traces stay plain (see newThinkingStream).
func (t *transcript) ensureMessage() liveRenderer {
	if t.active != nil && !t.activeThinking {
		return t.active
	}
	bg, fg := t.streamColors()
	t.active = newMarkdownLive(bg, fg, t.width)
	t.activeThinking = false
	return t.active
}

// streamColors are the slab colors of the text being streamed: a reply's, or
// a compaction summary's, which reads like a tool's output.
func (t *transcript) streamColors() (bg, fg color.Color) {
	if t.compacting {
		return colorToolBg, colorToolFg
	}
	return colorAgentBg, colorAgentFg
}

// beginCompaction opens a compaction summary's stream. A summary restarted
// in its fallback form discards what the first attempt streamed.
func (t *transcript) beginCompaction() {
	if !t.compacting {
		t.finishStream()
	}
	t.stream = t.stream[:0]
	t.active = nil
	t.compacting = true
}

// appendCompaction buffers a delta of the summary being written.
func (t *transcript) appendCompaction(text string) {
	if !t.compacting {
		t.beginCompaction()
	}
	clean := t.strip.Strip(text)
	t.stream = append(t.stream, clean...)
	t.ensureMessage().append(clean)
}

// finishCompaction closes the summary's stream as a block holding summary,
// the text that was kept, and marker.
func (t *transcript) finishCompaction(summary, marker string) {
	t.add(block{kind: blockCompaction, text: summary, marker: marker})
	t.stream = t.stream[:0]
	t.active = nil
	t.compacting = false
}

// appendThinking buffers a reasoning delta. Thinking may interleave with text
// within one assistant turn, so an active text stream is finalized first to
// keep blocks in chronological order.
func (t *transcript) appendThinking(text string) {
	if len(t.stream) > 0 {
		t.add(block{kind: blockAssistant, text: string(t.stream)})
		t.stream = t.stream[:0]
	}
	// Stripped bytes feed both the live stream and the buffered text, so the
	// block this stream later becomes wraps identically on the stable path
	// (plainWrapper cannot parse escape sequences; see tui.Stripper).
	clean := t.strip.Strip(text)
	t.ensureThinking().append(clean)
	t.thinking += clean
}

// appendStream buffers an assistant text delta, finalizing any pending
// thinking block first. Deltas are stripped of ANSI escapes and control bytes
// (see tui.Stripper) so the streaming wrapper only ever sees plain text; the
// buffered t.stream keeps the same stripped bytes, so the block the stream is
// later rendered from wraps identically and nothing shifts on finalize.
// The buffer is reused across deltas: `+=` would re-copy the whole accumulated
// message on every frame, which is O(n²) over a long stream and dominates the
// render cost once the message exceeds a few hundred KB.
func (t *transcript) appendStream(text string) {
	if t.thinking != "" {
		t.add(block{kind: blockThinking, text: t.thinking})
		t.thinking = ""
	}
	clean := t.strip.Strip(text)
	t.stream = append(t.stream, clean...)
	t.ensureMessage().append(clean)
}

func (t *transcript) finishStream() {
	if t.compacting {
		// A summary still open when its run ends was never kept.
		t.finishCompaction(string(t.stream), compactionStoppedLabel)
		return
	}
	if t.thinking != "" {
		t.add(block{kind: blockThinking, text: t.thinking})
		t.thinking = ""
	}
	if len(t.stream) == 0 {
		t.active = nil
		return
	}
	t.add(block{kind: blockAssistant, text: string(t.stream)})
	t.stream = t.stream[:0]
	t.active = nil
}

func (t *transcript) reset() {
	t.blocks = nil
	t.stream = t.stream[:0]
	t.thinking = t.thinking[:0]
	t.chunks = nil
	t.chunkFrom = nil
	t.joined = ""
	t.joinedChunks = 0
	t.built = 0
	t.selection = nil
	t.active = nil
	t.activeThinking = false
	t.strip = tui.Stripper{}
	t.liveTimer = ""
	t.lines = nil
	t.cacheBase = ""
	t.cacheBanner = ""
	t.stableN = 0
	t.sepDone = false
	t.liveFin = 0
	t.timerLines = 0
	t.dirty = false
}
