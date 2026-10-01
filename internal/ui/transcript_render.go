package ui

import (
	"fmt"
	"image/color"
	"strings"

	"charm.land/lipgloss/v2"
	"kon.kitsu.red/internal/codetools"
	"kon.kitsu.red/internal/markdown"
	"kon.kitsu.red/internal/tui"
)

// timerText renders the running indicator as its own live section, or "" when no
// turn is in flight. It shares blockElapsed's dim styling so the running and
// finished forms read as the same element.
func (t *transcript) timerText(width int) string {
	if t.liveTimer == "" {
		return ""
	}
	return strings.Join(markerLines(t.liveTimer, width), "\n")
}

// rebuildActive re-creates the incremental live stream at a new width from the
// buffered text, so a width change (terminal resize) renders correctly.
func (t *transcript) rebuildActive(width int) {
	switch {
	case t.thinking != "":
		t.active = newThinkingStream(width)
		t.active.append(t.thinking)
		t.activeThinking = true
	case len(t.stream) > 0:
		bg, fg := t.streamColors()
		t.active = newMarkdownLive(bg, fg, width)
		t.active.append(string(t.stream))
		t.activeThinking = false
	default:
		t.active = nil
		t.activeThinking = false
	}
}

// restyle discards every cached rendering so blocks edited in place render
// again. Zero is never a real width, so the next prepare rebuilds everything
// as it does after a resize.
func (t *transcript) restyle() { t.width = 0 }

// prepare refreshes the chunk cache for the given width and returns the stable
// joined text. It folds any newly stable blocks and joins chunks only when the
// chunk set changed, so repeated frames while streaming do not rejoin history.
func (t *transcript) prepare(width int) string {
	if width < 1 {
		width = 1
	}
	if width != t.width {
		t.width = width
		t.chunks = nil
		t.chunkFrom = nil
		t.selection = nil
		t.joined = ""
		t.joinedChunks = 0
		t.built = 0
		t.dirty = true
		t.lines = nil
		t.cacheBase = ""
		t.cacheBanner = ""
		t.stableN = 0
		t.sepDone = false
		t.liveFin = 0
		t.timerLines = 0
		t.rebuildActive(width)
	}
	if t.dirty {
		t.ensureChunks(width)
		t.dirty = false
	}
	if t.joinedChunks != len(t.chunks) {
		t.joined = strings.Join(t.chunks, "\n\n")
		t.joinedChunks = len(t.chunks)
	}
	return t.joined
}

// linesFor returns the transcript as a slice of display lines, ready for
// viewport.SetContentLines. It maintains a line cache across frames: while the
// stable text is unchanged it keeps the stable prefix and appends the live
// stream's finalised lines, so only the current live line is rebuilt each frame.
// A structural change rebuilds once. The returned slice aliases the cache and
// must not be modified.
func (t *transcript) linesFor(width int) []string {
	t.prepare(width)
	// The banner is a fixed prefix that only changes with width, so it is
	// compared separately from the body. Folding it into the change key would
	// concatenate the whole document on every frame and turn the cache hit into
	// an O(history) copy and compare — the per-frame cost that the tests below
	// once hid by measuring banner-less transcripts.
	body := t.bodyBase(width)
	banner := t.bannerText(width)
	if t.lines == nil || body != t.cacheBase || banner != t.cacheBanner {
		t.lines = t.lines[:0]
		if banner != "" {
			t.lines = append(t.lines, strings.Split(banner, "\n")...)
		}
		if body != "" {
			if banner != "" {
				t.lines = append(t.lines, "")
			}
			t.lines = append(t.lines, strings.Split(body, "\n")...)
		}
		t.cacheBase = body
		t.cacheBanner = banner
		t.stableN = len(t.lines)
		t.sepDone = false
		t.liveFin = 0
		t.timerLines = 0
	}
	// Drop the previous frame's timer before the stream tail touches the slice.
	// The stream branch below truncates from liveStart, which would otherwise
	// discard the timer's lines and make the drop count cut into the live stream.
	t.lines = t.lines[:len(t.lines)-t.timerLines]
	t.timerLines = 0
	// The label and finalized live lines are append-only. Each frame drops the
	// previous current line (and, if nothing is live now, the separator) and
	// rebuilds only what changed.
	if t.active != nil && (t.thinking != "" || len(t.stream) > 0) {
		if !t.sepDone && t.stableN > 0 {
			t.lines = append(t.lines, "")
			t.sepDone = true
		}
		t.lines = t.lines[:t.liveStart()]
		fin := t.active.finalized()
		for t.liveFin < len(fin) {
			t.lines = append(t.lines, fin[t.liveFin])
			t.liveFin++
		}
		t.lines = append(t.lines, t.active.currentLines()...)
	} else if t.sepDone {
		t.lines = t.lines[:t.stableN]
		t.sepDone = false
		t.liveFin = 0
	}
	// The running timer trails everything, stream and all. Only its own lines
	// are rebuilt each frame, so a ticking second never repaints the stream.
	if timer := t.timerText(width); timer != "" {
		before := len(t.lines)
		if len(t.lines) > 0 {
			t.lines = append(t.lines, "")
		}
		t.lines = append(t.lines, strings.Split(timer, "\n")...)
		// Count the separator too, so the next frame drops all of it.
		t.timerLines = len(t.lines) - before
	}
	return t.lines
}

// liveStart is the index in t.lines where the current live line begins: after
// the stable lines, the separator, and the finalized live lines.
func (t *transcript) liveStart() int {
	n := t.stableN + t.liveFin
	if t.sepDone {
		n++
	}
	return n
}

// bodyBase returns the stable transcript body without the banner: the joined
// chunks plus any trailing tool run not yet folded into them. It returns the
// memoized join whenever nothing is left unfolded, so a caller comparing it
// across frames pays nothing.
func (t *transcript) bodyBase(width int) string {
	base := t.joined
	if t.built < len(t.blocks) {
		tail := strings.Join(t.renderToolRun(t.blocks[t.built:], width), "\n")
		if tail != "" {
			if base == "" {
				base = tail
			} else {
				base += "\n\n" + tail
			}
		}
	}
	return base
}

// ensureChunks folds every stable block into chunks, appending the rendered
// text of each one. Only blocks before the trailing run of tool/result blocks
// are stable: a run in progress may still be joined by a later result, so it
// is left in blocks for render to draw on demand. Because appending a block
// either extends the trailing run in place or terminates it, the stable
// boundary never moves backward and chunks are never folded twice. Runs of
// consecutive tool and result blocks collapse into a single grouped chunk.
func (t *transcript) ensureChunks(width int) {
	stable := len(t.blocks)
	for stable > 0 && (t.blocks[stable-1].kind == blockTool || t.blocks[stable-1].kind == blockResult) {
		stable--
	}
	for t.built < stable {
		if t.blocks[t.built].kind == blockTool || t.blocks[t.built].kind == blockResult {
			end := t.built
			for end < stable && (t.blocks[end].kind == blockTool || t.blocks[end].kind == blockResult) {
				end++
			}
			t.pushChunk(strings.Join(t.renderToolRun(t.blocks[t.built:end], width), "\n"), t.built)
			t.built = end
			continue
		}
		t.pushChunk(strings.Join(t.renderBlock(t.blocks[t.built], width), "\n"), t.built)
		t.built++
	}
}

// pushChunk appends a chunk rendered from the blocks starting at from,
// dropping empty ones so grouping does not leave stray blank separators.
func (t *transcript) pushChunk(text string, from int) {
	if text != "" {
		t.chunks = append(t.chunks, text)
		t.chunkFrom = append(t.chunkFrom, from)
	}
}

func (t *transcript) renderBlock(b block, width int) []string {
	switch b.kind {
	case blockUser:
		return messageSlab(normalizeText(b.text), colorUserBg, colorUserFg, width)
	case blockAssistant:
		return renderMarkdownBlock(markdown.Render(b.text, markdown.Theme{}, markdownContentWidth(width)), colorAgentBg, colorAgentFg, width)
	case blockThinking:
		return thinkingLines(normalizeText(b.text), width)
	case blockError:
		return messageSlab(normalizeText(b.text), colorErrorBg, colorErrorFg, width)
	case blockContext:
		return []string{separatorLine(b.text, width)}
	case blockElapsed:
		return markerLines(b.text, width)
	case blockCompaction:
		// The summary renders as it streamed, and the marker takes the place
		// the running timer had under it.
		var lines []string
		if b.text != "" {
			lines = append(renderMarkdownBlock(markdown.Render(b.text, markdown.Theme{}, markdownContentWidth(width)), colorToolBg, colorToolFg, width), "")
		}
		return append(lines, markerLines(b.marker, width)...)
	case blockModel, blockModels:
		return dimLines(normalizeText(b.text), width)
	case blockOutput:
		return outputLines(b.text, width)
	default:
		return nil
	}
}

// toolNameWidth is the width of the name column on a tool request line: names
// are padded so the summaries of a grouped run align.
const toolNameWidth = 6

// renderToolRun renders a run of consecutive tool and result blocks as one
// slab. Each pair renders from the displays its owning tool resolved: the
// request line combines the running call's arguments summary with the
// finished result's outcome note, and the body belongs to the result. A
// blockTool without a result yet is still running.
// The pair pointers alias run and are only valid for the duration of the
// call; callers must not append to the underlying slice until it returns.
func (t *transcript) renderToolRun(run []block, width int) []string {
	type pair struct {
		start, done *block
	}
	var pairs []pair
	for i := 0; i < len(run); i++ {
		switch run[i].kind {
		case blockTool:
			current := pair{start: &run[i]}
			if i+1 < len(run) && run[i+1].kind == blockResult {
				i++
				current.done = &run[i]
			}
			pairs = append(pairs, current)
		case blockResult:
			pairs = append(pairs, pair{done: &run[i]})
		}
	}
	var lines []string
	for _, p := range pairs {
		lines = append(lines, t.toolRequestLine(p.start, p.done, width))
		lines = append(lines, t.toolBodyLines(p.start, p.done, width)...)
	}
	return lines
}

// callDisplay picks the display that carries a call's outcome: the result's
// resolved display when the call finished, otherwise the running call's live
// snapshot, and finally a bare running shell.
func (t *transcript) callDisplay(start, done *block) codetools.Display {
	if done != nil {
		return done.display
	}
	if start != nil && start.display.State == codetools.StateRunning || start != nil && start.display.Summary != "" {
		return start.display
	}
	return codetools.Display{State: codetools.StateRunning}
}

// toolRequestLine renders one call's request line: status icon, the padded
// tool name, the summary rendered from the call's arguments, and the outcome
// note from the finished result when one exists.
func (t *transcript) toolRequestLine(start, done *block, width int) string {
	name := ""
	if start != nil {
		name = start.name
	} else if done != nil {
		name = done.name
	}
	summary := ""
	if start != nil {
		summary = start.display.Summary
	} else if done != nil {
		summary = done.display.Summary
	}
	outcome := t.callDisplay(start, done)

	icon, iconColor := "●", colorRun
	noteColor := colorToolNote
	if done != nil || start == nil {
		switch outcome.State {
		case codetools.StateFailed:
			icon, iconColor, noteColor = "✗", colorFail, colorFail
		case codetools.StateDone:
			icon, iconColor = "✓", colorOK
		}
	}
	segments := []part{
		{text: icon, fg: iconColor},
		{text: padName(name, toolNameWidth), fg: colorToolName, bold: true},
	}
	if summary != "" {
		segments = append(segments, part{text: summary, fg: colorToolFg})
	}
	if outcome.Note != "" {
		segments = append(segments, part{text: "· " + outcome.Note, fg: noteColor})
	}
	return slabLine(colorToolBg, width, segments...)
}

// toolBodyLines renders a call's body: the owning tool's trimmed output
// lines, the omission marker, and the terminal status line. A running call
// streams its lines live; a finished call shows the outcome tail its tool
// chose to keep.
func (t *transcript) toolBodyLines(start, done *block, width int) []string {
	display := t.callDisplay(start, done)
	fg := colorToolFg
	if display.Quiet {
		fg = colorToolNote
	}
	if display.State == codetools.StateFailed {
		fg = colorFail
	}
	var out []string
	for _, line := range display.Lines {
		out = append(out, slabLine(colorToolBg, width, part{text: "  " + tui.ExpandTabs(line), fg: fg}))
	}
	if display.More > 0 {
		more := fmt.Sprintf("  … %d more lines", display.More)
		if display.More == 1 {
			more = "  … 1 more line"
		}
		out = append(out, slabLine(colorToolBg, width, part{text: more, fg: colorToolNote}))
	}
	if display.Status != "" {
		// A running call's status (a shell command's ticking elapsed/timeout)
		// is progress, not an outcome, so it stays quiet like the body. Only a
		// finished call colors its status line.
		statusColor := colorOK
		switch display.State {
		case codetools.StateFailed:
			statusColor = colorFail
		case codetools.StateRunning:
			statusColor = colorToolNote
		}
		out = append(out, slabLine(colorToolBg, width, part{text: "  " + display.Status, fg: statusColor}))
	}
	return out
}

// messageSlab renders a user, agent, or error message body: word-wrapped and
// painted by linePainter so the background reaches the full viewport width.
// The body wrap and painting match liveStream exactly, so a message rendered
// live and later folded into a stable block does not shift.
func messageSlab(body string, bg, fg color.Color, width int) []string {
	p := linePainter{width: width, bg: bg, fg: fg, padLeft: 1}
	return paintBody(p, tui.WrapPlain(tui.ExpandTabs(body), max(1, width-2)))
}

// thinkingLines renders a reasoning trace in a muted gray, visually quieter
// than agent messages. The body is indented like a message slab so the
// reasoning aligns with the text it belongs to; it just carries no background.
func thinkingLines(body string, width int) []string {
	p := linePainter{width: width, fg: colorFaint, italic: true, padLeft: 1}
	return paintBody(p, tui.WrapPlain(body, max(1, width-2)))
}

// paintBody renders a painter's wrapped body lines.
func paintBody(p linePainter, lines []string) []string {
	out := make([]string, 0, len(lines))
	for _, line := range lines {
		out = append(out, p.bodyLine(line))
	}
	return out
}

// separatorLine centers text between horizontal rules, for markers like
// context compaction.
func separatorLine(text string, width int) string {
	text = " " + text + " "
	fill := width - lipgloss.Width(text)
	if fill < 2 {
		return lipgloss.NewStyle().Foreground(colorFaint).Render(text)
	}
	left := fill / 2
	line := strings.Repeat("─", left) + text + strings.Repeat("─", fill-left)
	return lipgloss.NewStyle().Foreground(colorFaint).Render(line)
}

func dimLines(text string, width int) []string {
	style := lipgloss.NewStyle().Foreground(colorFaint).Width(width)
	return strings.Split(style.Render(text), "\n")
}

// outputLines wraps each line of a command's output, inset one cell like
// message slabs, in the terminal's own text color.
func outputLines(text string, width int) []string {
	var out []string
	for _, line := range strings.Split(text, "\n") {
		for _, part := range tui.WrapPlain(line, max(1, width-1)) {
			out = append(out, " "+part)
		}
	}
	return out
}

// markerLines renders a turn marker (the running indicator or a frozen total) in
// a muted gray, inset one cell like message slabs so it aligns with the text
// above it. A wrapped continuation keeps the inset too.
func markerLines(text string, width int) []string {
	body := tui.WrapPlain(text, max(1, width-1))
	out := make([]string, 0, len(body))
	for _, line := range body {
		out = append(out, " "+lipgloss.NewStyle().Foreground(colorFaint).Render(line))
	}
	return out
}

func padName(name string, width int) string {
	return fmt.Sprintf("%-*s", width, name)
}

// normalizeText prepares a message or reasoning body for display: it unifies
// carriage returns to newlines, drops leading and trailing blank lines, removes
// trailing spaces from each line, and collapses internal blank runs to a single
// blank line so a body never leaves stray whitespace that disturbs the slab.
func normalizeText(text string) string {
	lines := splitDisplayLines(text)
	for i, line := range lines {
		lines[i] = strings.TrimRight(line, " ")
	}
	start, end := 0, len(lines)
	for start < end && lines[start] == "" {
		start++
	}
	for end > start && lines[end-1] == "" {
		end--
	}
	kept := make([]string, 0, end-start)
	prevBlank := true
	for _, line := range lines[start:end] {
		blank := line == ""
		if blank && prevBlank {
			continue
		}
		kept = append(kept, line)
		prevBlank = blank
	}
	return strings.Join(kept, "\n")
}

// splitDisplayLines splits text on line breaks, treating CRLF as a single
// newline and a lone CR (spinner-style \r output) as a line break.
func splitDisplayLines(text string) []string {
	text = strings.ReplaceAll(text, "\r\n", "\n")
	text = strings.ReplaceAll(text, "\r", "\n")
	return strings.Split(text, "\n")
}
