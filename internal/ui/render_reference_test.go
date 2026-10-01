package ui

import (
	"strings"

	"kon.kitsu.red/internal/markdown"
)

// This file holds the from-scratch transcript renderer. Production repaints go
// through linesFor, which keeps per-line caches; render rebuilds everything on
// each call, so it is the reference the output-equivalence tests compare the
// cached path against. It lives in a test file so a production caller cannot
// reach for it and pay O(transcript) per frame.

// render returns the whole transcript as one joined string.
func (t *transcript) render(width int) string {
	t.prepare(width)
	return joinLive(t.stableBase(width), t.pending(width))
}

// stableBase returns the stable (already-finalized) transcript text: the
// welcome banner, then the joined chunks and any trailing tool run not yet
// folded.
func (t *transcript) stableBase(width int) string {
	return joinLive(t.bannerText(width), t.bodyBase(width))
}

// pending renders the live, not-yet-finalized portion of the transcript. Only
// one stream is active at a time. When an incremental live stream is available
// it is used; otherwise (e.g. a transcript built directly from blocks) the
// buffered text is rendered from scratch. The running turn's timer, when set,
// trails the live content as its own block.
func (t *transcript) pending(width int) string {
	return joinLive(t.pendingStream(width), t.timerText(width))
}

// pendingStream renders the live stream alone, without the trailing timer.
func (t *transcript) pendingStream(width int) string {
	switch {
	case t.thinking != "":
		if t.active != nil && t.activeThinking {
			return livePending(t.active)
		}
		return strings.Join(thinkingLines(normalizeText(t.thinking), width), "\n")
	case len(t.stream) > 0:
		if t.active != nil && !t.activeThinking {
			return livePending(t.active)
		}
		bg, fg := t.streamColors()
		return strings.Join(renderMarkdownBlock(markdown.Render(string(t.stream), markdown.Theme{}, markdownContentWidth(width)), bg, fg, width), "\n")
	default:
		return ""
	}
}

// livePending returns a live renderer's whole output, finished and growing
// lines alike, as one string.
func livePending(r liveRenderer) string {
	return strings.Join(append(append([]string{}, r.finalized()...), r.currentLines()...), "\n")
}

// joinLive concatenates two sections with the transcript's blank separator,
// dropping an empty one so no stray gap is left behind.
func joinLive(a, b string) string {
	switch {
	case a == "":
		return b
	case b == "":
		return a
	default:
		return a + "\n\n" + b
	}
}
