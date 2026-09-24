package ui

import (
	"fmt"
	"image/color"
	"strings"

	"charm.land/lipgloss/v2"
	"github.com/charmbracelet/x/ansi"
	"github.com/hizkifw/kon/internal/markdown"
	"github.com/hizkifw/kon/internal/session"
	"github.com/hizkifw/kon/internal/tools"
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
	display tools.Display
	// model is the recorded selection behind a replayed model change, kept so
	// its title can be named again once the catalog loads.
	model *session.ModelSelection
}

// Transcript palette. The base is neutral grey: message slabs differ by
// lightness rather than hue. Green is reserved for success (colorOK); red for
// failure (colorFail). lipgloss degrades these automatically on terminals with
// smaller color profiles.
var (
	// colorAccent is the muted red kon brand accent (header wordmark).
	colorAccent = lipgloss.Color("#C98A8A")

	colorUserBg = lipgloss.Color("#313131")
	colorUserFg = lipgloss.Color("#DEDEDE")

	// Agent messages render on the default terminal background: NoColor draws
	// no background at all, so only the user slab (and tool/error slabs) are
	// painted.
	colorAgentBg = lipgloss.NoColor{}
	colorAgentFg = lipgloss.Color("#EAEAEA")

	colorToolBg   = lipgloss.Color("#2B2B2B")
	colorToolFg   = lipgloss.Color("#909090")
	colorToolName = lipgloss.Color("#C9C9C9")
	colorToolNote = lipgloss.Color("#707070")
	colorResult   = lipgloss.Color("#808080")

	colorErrorBg = lipgloss.Color("#5A2120")
	colorErrorFg = lipgloss.Color("#F2DCD8")

	colorFaint = lipgloss.Color("#757575")
	colorOK    = lipgloss.Color("#79C98B")
	colorRun   = colorFaint // in-flight tools stay quiet
	colorFail  = lipgloss.Color("#E06C6C")

	// Header and status bar share one background so the top and bottom edges
	// read as a single frame.
	colorBarBg = lipgloss.Color("#1C1C1C")
	colorBarFg = lipgloss.Color("#C8C8C8")
)

// part is one styled segment of a slab line.
type part struct {
	text      string
	fg        color.Color
	bg        color.Color // segment background; nil uses the slab background
	link      string      // OSC 8 hyperlink target, when set
	bold      bool
	italic    bool
	underline bool
	strike    bool
}

// Markdown palette additions: colors used only by markdown roles.
var (
	colorHeadingFg = lipgloss.Color("#FFFFFF")
	// Code shares the kon brand accent so code and the wordmark read as one
	// family; the muted red is legible on both the default and tool slabs.
	colorCodeFg  = colorAccent
	colorQuoteFg = lipgloss.Color("#B0B0B0")
	colorLink    = lipgloss.Color("#7FB3D5")

	// Table tints: subtle neutral backgrounds so a table reads as a distinct
	// block without borders. The row band makes wide or wrapped tables easy to
	// follow across lines.
	colorTableHeaderBg = lipgloss.Color("#3A3A3A")
	colorTableRowBg    = lipgloss.Color("#262626")
)

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
	joined string
	// joinedChunks is the len(t.chunks) at the time joined was built; it is a
	// memo key, not a count, so re-joining only happens when a chunk was added.
	joinedChunks int
	built        int // number of blocks folded into chunks

	// active renders the live stream (t.stream or t.thinking) incrementally so a
	// repaint during streaming does not re-wrap the whole accumulated message.
	// It is nil when no stream is open; activeThinking selects the renderer.
	// strip removes escapes/control bytes from incoming deltas; the stateful
	// machine survives across deltas so a sequence split between them is
	// dropped whole (see ansiStripper).
	active         liveRenderer
	activeThinking bool
	strip          ansiStripper

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

	dirty bool
	width int
	cwd   string
	// banner is the presentation-only mark that leads every transcript. It is
	// never a block, so it stays out of session records, but it renders as a
	// stable prefix above the conversation even on a resumed session. Its
	// rendering depends only on the mark and the width, so it is memoized.
	banner      string
	bannerCache string
	bannerWidth int
	bannerValid bool
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
func (t *transcript) updateToolLive(d tools.Display) {
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
	pending() string
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
	t.active = newMarkdownLive(colorAgentBg, colorAgentFg, t.width)
	t.activeThinking = false
	return t.active
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
	// (plainWrapper cannot parse escape sequences; see ansiStripper).
	clean := t.strip.strip(text)
	t.ensureThinking().append(clean)
	t.thinking += clean
}

// appendStream buffers an assistant text delta, finalizing any pending
// thinking block first. Deltas are stripped of ANSI escapes and control bytes
// (see ansiStripper) so the streaming wrapper only ever sees plain text; the
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
	clean := t.strip.strip(text)
	t.stream = append(t.stream, clean...)
	t.ensureMessage().append(clean)
}

func (t *transcript) finishStream() {
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
	t.joined = ""
	t.joinedChunks = 0
	t.built = 0
	t.active = nil
	t.activeThinking = false
	t.strip = ansiStripper{}
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
			return t.active.pending()
		}
		return strings.Join(thinkingLines(normalizeText(t.thinking), width), "\n")
	case len(t.stream) > 0:
		if t.active != nil && !t.activeThinking {
			return t.active.pending()
		}
		return strings.Join(renderMarkdownBlock(markdown.Render(string(t.stream), markdown.Theme{}, markdownContentWidth(width)), colorAgentBg, colorAgentFg, width), "\n")
	default:
		return ""
	}
}

// timerText renders the running indicator as its own live section, or "" when no
// turn is in flight. It shares blockElapsed's dim styling so the running and
// finished forms read as the same element.
func (t *transcript) timerText(width int) string {
	if t.liveTimer == "" {
		return ""
	}
	return strings.Join(markerLines(t.liveTimer, width), "\n")
}

// joinLive concatenates two live sections with the transcript's blank separator,
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

// rebuildActive re-creates the incremental live stream at a new width from the
// buffered text, so a width change (terminal resize) renders correctly.
func (t *transcript) rebuildActive(width int) {
	switch {
	case t.thinking != "":
		t.active = newThinkingStream(width)
		t.active.append(t.thinking)
		t.activeThinking = true
	case len(t.stream) > 0:
		t.active = newMarkdownLive(colorAgentBg, colorAgentFg, width)
		t.active.append(string(t.stream))
		t.activeThinking = false
	default:
		t.active = nil
		t.activeThinking = false
	}
}

// prepare refreshes the chunk cache for the given width and returns the stable
// joined text. It folds any newly stable blocks and joins chunks only when the
// chunk set changed, so repeated frames while streaming do not rejoin history.
// restyle discards every cached rendering so blocks edited in place render
// again. Zero is never a real width, so the next prepare rebuilds everything
// as it does after a resize.
func (t *transcript) restyle() { t.width = 0 }

func (t *transcript) prepare(width int) string {
	if width < 1 {
		width = 1
	}
	if width != t.width {
		t.width = width
		t.chunks = nil
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

// render returns the whole transcript as one joined string. Production repaints
// go through linesFor, which keeps per-line caches; render stays as the
// from-scratch reference implementation that the output-equivalence tests
// compare against, so any change here must keep the incremental path in sync.
func (t *transcript) render(width int) string {
	t.prepare(width)
	base, live := t.assemble(width)
	switch {
	case live == "":
		return base
	case base == "":
		return live
	default:
		return base + "\n\n" + live
	}
}

// assemble returns the stable transcript text and the live (unfinished tail)
// text separately. Keeping them apart lets the line cache append only the live
// portion instead of splitting the whole document on every frame.
func (t *transcript) assemble(width int) (base, live string) {
	return t.stableBase(width), t.pending(width)
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

// stableBase returns the stable (already-finalized) transcript text: the
// welcome banner, then the joined chunks and any trailing tool run not yet
// folded. It is the from-scratch reference; the cached line path in linesFor
// keeps the banner separate so a frame that changes neither never re-copies the
// document.
func (t *transcript) stableBase(width int) string {
	base := t.bodyBase(width)
	banner := t.bannerText(width)
	switch {
	case banner == "":
		return base
	case base == "":
		return banner
	default:
		return banner + "\n\n" + base
	}
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
			t.pushChunk(strings.Join(t.renderToolRun(t.blocks[t.built:end], width), "\n"))
			t.built = end
			continue
		}
		t.pushChunk(strings.Join(t.renderBlock(t.blocks[t.built], width), "\n"))
		t.built++
	}
}

// pushChunk appends a rendered chunk, dropping empty ones so grouping does not
// leave stray blank separators.
func (t *transcript) pushChunk(text string) {
	if text != "" {
		t.chunks = append(t.chunks, text)
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
	case blockModel, blockModels:
		return dimLines(normalizeText(b.text), width)
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
func (t *transcript) callDisplay(start, done *block) tools.Display {
	if done != nil {
		return done.display
	}
	if start != nil && start.display.State == tools.StateRunning || start != nil && start.display.Summary != "" {
		return start.display
	}
	return tools.Display{State: tools.StateRunning}
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
		case tools.StateFailed:
			icon, iconColor, noteColor = "✗", colorFail, colorFail
		case tools.StateDone:
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
	if display.State == tools.StateFailed {
		fg = colorFail
	}
	var out []string
	for _, line := range display.Lines {
		out = append(out, slabLine(colorToolBg, width, part{text: "  " + line, fg: fg}))
	}
	if display.More > 0 {
		out = append(out, slabLine(colorToolBg, width, part{text: fmt.Sprintf("  … %d more lines", display.More), fg: colorToolNote}))
	}
	if display.Status != "" {
		// A running call's status (a shell command's ticking elapsed/timeout)
		// is progress, not an outcome, so it stays quiet like the body. Only a
		// finished call colors its status line.
		statusColor := colorOK
		switch display.State {
		case tools.StateFailed:
			statusColor = colorFail
		case tools.StateRunning:
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
	return paintBody(p, wrapPlain(body, max(1, width-2)))
}

// thinkingLines renders a reasoning trace in a muted gray, visually quieter
// than agent messages. The body is indented like a message slab so the
// reasoning aligns with the text it belongs to; it just carries no background.
func thinkingLines(body string, width int) []string {
	p := linePainter{width: width, fg: colorFaint, italic: true, padLeft: 1}
	return paintBody(p, wrapPlain(body, max(1, width-2)))
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

// markerLines renders a turn marker (the running indicator or a frozen total) in
// a muted gray, inset one cell like message slabs so it aligns with the text
// above it. A wrapped continuation keeps the inset too.
func markerLines(text string, width int) []string {
	body := wrapPlain(text, max(1, width-1))
	out := make([]string, 0, len(body))
	for _, line := range body {
		out = append(out, " "+lipgloss.NewStyle().Foreground(colorFaint).Render(line))
	}
	return out
}

// slabLine renders one full-width line of a background slab with individually
// colored segments. Segments are styled separately (each carrying the slab
// background) so their resets cannot punch holes in the slab, and the joins
// and padding are painted with the same background.
func slabLine(bg color.Color, width int, segments ...part) string {
	var out strings.Builder
	used := 1 // left padding cell
	out.WriteString(bgSpaces(bg, 1))
	for _, segment := range segments {
		if segment.text == "" {
			continue
		}
		if used > 1 {
			out.WriteString(bgSpaces(bg, 1))
			used++
		}
		avail := width - used - 1 // keep one cell of right padding
		if avail < 1 {
			break
		}
		text := segment.text
		if lipgloss.Width(text) > avail {
			text = ansi.Truncate(text, avail, "…")
		}
		style := lipgloss.NewStyle().Foreground(segment.fg).Background(bg)
		if segment.bold {
			style = style.Bold(true)
		}
		out.WriteString(style.Render(text))
		used += lipgloss.Width(text)
	}
	out.WriteString(bgSpaces(bg, max(0, width-used)))
	return out.String()
}

func bgSpaces(bg color.Color, n int) string {
	if n <= 0 {
		return ""
	}
	return lipgloss.NewStyle().Background(bg).Render(strings.Repeat(" ", n))
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

// tailLines returns at most n lines, dropping trailing blank lines.
func tailLines(text string, n int) []string {
	lines := strings.Split(text, "\n")
	for len(lines) > 0 && strings.TrimSpace(lines[len(lines)-1]) == "" {
		lines = lines[:len(lines)-1]
	}
	if len(lines) > n {
		lines = lines[len(lines)-n:]
	}
	return lines
}
