package ui

import (
	"fmt"
	"image/color"
	"strings"

	"charm.land/lipgloss/v2"
	"github.com/charmbracelet/x/ansi"
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
)

// toolTailLines caps how many trailing lines of tool output are displayed for
// results that are echoed verbatim (shell output, failures, unknown tools).
const toolTailLines = 6

// block is one entry in the transcript. Tool calls arrive as a blockTool
// (request) followed by a blockResult (completion); they are paired and
// grouped at render time.
type block struct {
	kind   blockKind
	name   string // tool name for tool blocks
	args   string // raw JSON arguments for tool blocks
	text   string
	note   string // tool success summary, shown on the request line ("12 lines")
	exit   string // shell exit code
	took   string // shell wall-clock duration
	failed bool   // tool result reported an error
}

// Transcript palette. lipgloss degrades these automatically on terminals with
// smaller color profiles.
var (
	colorUserBg    = lipgloss.Color("#1E3A5C")
	colorUserFg    = lipgloss.Color("#DCE6F5")
	colorUserLabel = lipgloss.Color("#9CC3F5")

	colorAgentBg    = lipgloss.Color("#1D332A")
	colorAgentFg    = lipgloss.Color("#DAE8DF")
	colorAgentLabel = lipgloss.Color("#82D2A6")

	colorToolBg   = lipgloss.Color("#242936")
	colorToolFg   = lipgloss.Color("#8C96A8")
	colorToolName = lipgloss.Color("#C9D3E0")
	colorToolNote = lipgloss.Color("#6E7989")
	colorResult   = lipgloss.Color("#7C8798")

	colorErrorBg    = lipgloss.Color("#5A2120")
	colorErrorFg    = lipgloss.Color("#F2DCD8")
	colorErrorLabel = lipgloss.Color("#F0A9A2")

	colorFaint = lipgloss.Color("#6E7787")
	colorOK    = lipgloss.Color("#79C98B")
	colorRun   = lipgloss.Color("#D9B45C")
	colorFail  = lipgloss.Color("#E06C6C")
)

// part is one styled segment of a slab line.
type part struct {
	text string
	fg   color.Color
	bold bool
}

type transcript struct {
	blocks   []block
	stream   string
	thinking string
	cached   string
	dirty    bool
	width    int
	cwd      string
}

func (t *transcript) add(value block) {
	t.blocks = append(t.blocks, value)
	t.dirty = true
}

// appendThinking buffers a reasoning delta. Thinking may interleave with text
// within one assistant turn, so an active text stream is finalized first to
// keep blocks in chronological order.
func (t *transcript) appendThinking(text string) {
	if t.stream != "" {
		t.add(block{kind: blockAssistant, text: t.stream})
		t.stream = ""
	}
	t.thinking += text
}

// appendStream buffers an assistant text delta, finalizing any pending
// thinking block first.
func (t *transcript) appendStream(text string) {
	if t.thinking != "" {
		t.add(block{kind: blockThinking, text: t.thinking})
		t.thinking = ""
	}
	t.stream += text
}

func (t *transcript) finishStream() {
	if t.thinking != "" {
		t.add(block{kind: blockThinking, text: t.thinking})
		t.thinking = ""
	}
	if t.stream == "" {
		return
	}
	t.add(block{kind: blockAssistant, text: t.stream})
	t.stream = ""
}

func (t *transcript) reset() {
	t.blocks = nil
	t.stream = ""
	t.thinking = ""
	t.cached = ""
	t.dirty = false
}

// pending renders the live, not-yet-finalized portion of the transcript. Only
// one stream is active at a time.
func (t *transcript) pending(width int) string {
	switch {
	case t.thinking != "":
		return strings.Join(thinkingLines(normalizeText(t.thinking), width), "\n")
	case t.stream != "":
		return strings.Join(messageSlab("kon", normalizeText(t.stream), colorAgentBg, colorAgentFg, colorAgentLabel, width), "\n")
	default:
		return ""
	}
}

func (t *transcript) render(width int) string {
	if width < 1 {
		width = 1
	}
	if t.dirty || width != t.width {
		t.width = width
		t.cached = t.build(width)
		t.dirty = false
	}
	live := t.pending(width)
	switch {
	case live == "":
		return t.cached
	case t.cached == "":
		return live
	default:
		return t.cached + "\n\n" + live
	}
}

// build renders every finalized block. Runs of consecutive tool and result
// blocks collapse into a single grouped slab so that bursts of tool calls
// stack tightly instead of littering the history.
func (t *transcript) build(width int) string {
	var chunks [][]string
	var run []block
	flush := func() {
		if len(run) == 0 {
			return
		}
		chunks = append(chunks, t.renderToolRun(run, width))
		run = run[:0]
	}
	for _, b := range t.blocks {
		if b.kind == blockTool || b.kind == blockResult {
			run = append(run, b)
			continue
		}
		flush()
		chunks = append(chunks, t.renderBlock(b, width))
	}
	flush()
	rendered := make([]string, 0, len(chunks))
	for _, chunk := range chunks {
		if len(chunk) == 0 {
			continue
		}
		rendered = append(rendered, strings.Join(chunk, "\n"))
	}
	return strings.Join(rendered, "\n\n")
}

func (t *transcript) renderBlock(b block, width int) []string {
	switch b.kind {
	case blockUser:
		return messageSlab("you", normalizeText(b.text), colorUserBg, colorUserFg, colorUserLabel, width)
	case blockAssistant:
		return messageSlab("kon", normalizeText(b.text), colorAgentBg, colorAgentFg, colorAgentLabel, width)
	case blockThinking:
		return thinkingLines(normalizeText(b.text), width)
	case blockError:
		return messageSlab("error", normalizeText(b.text), colorErrorBg, colorErrorFg, colorErrorLabel, width)
	case blockContext:
		return []string{separatorLine(b.text, width)}
	case blockModel, blockModels:
		return dimLines(normalizeText(b.text), width)
	default:
		return nil
	}
}

// renderToolRun renders a run of consecutive tool and result blocks as one
// slab. A blockTool pairs with the blockResult that follows it; a blockTool
// without a result yet is still running.
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
	nameWidth := 0
	for _, p := range pairs {
		name := ""
		if p.start != nil {
			name = p.start.name
		} else if p.done != nil {
			name = p.done.name
		}
		nameWidth = max(nameWidth, len(name))
	}
	var lines []string
	for _, p := range pairs {
		lines = append(lines, t.toolRequestLine(p.start, p.done, nameWidth, width))
		lines = append(lines, t.toolResultLines(p.done, width)...)
	}
	return lines
}

func (t *transcript) toolRequestLine(start, done *block, nameWidth, width int) string {
	name, args := "", ""
	if start != nil {
		name, args = start.name, start.args
	} else if done != nil {
		name, args = done.name, done.args
	}
	icon, iconColor := "●", colorRun
	finished := false
	if done != nil {
		if done.failed {
			icon, iconColor, finished = "✗", colorFail, true
		} else {
			icon, iconColor, finished = "✓", colorOK, true
		}
	}
	summary := toolSummary(name, args, t.cwd)
	segments := []part{
		{text: icon, fg: iconColor},
		{text: padName(name, nameWidth), fg: colorToolName, bold: true},
	}
	if finished && !done.failed && done.note != "" {
		segments = append(segments,
			part{text: summary, fg: colorToolFg},
			part{text: "· " + done.note, fg: colorToolNote},
		)
	} else {
		segments = append(segments, part{text: summary, fg: colorToolFg})
	}
	return slabLine(colorToolBg, width, segments...)
}

// toolResultLines renders the echo of a finished tool call. Successful
// read/write/edit results are already summarized on the request line, so only
// output-carrying results (shell, failures, unknown tools) appear here.
func (t *transcript) toolResultLines(done *block, width int) []string {
	if done == nil {
		return nil
	}
	// Successful read/write/edit results are summarized on the request line.
	if !done.failed && done.name != "shell" && done.text == "" {
		return nil
	}
	fg := colorResult
	if done.failed {
		fg = colorFail
	}
	text := normalizeOutput(done.text)
	lines := tailLines(text, toolTailLines)
	if !done.failed && done.name == "shell" {
		// Success output gets a fainter voice than the request line.
		fg = colorToolNote
	}
	out := make([]string, 0, len(lines)+1)
	for _, line := range lines {
		out = append(out, slabLine(colorToolBg, width, part{text: "  " + line, fg: fg}))
	}
	if done.name == "shell" && done.exit != "" {
		color := colorFail
		if done.exit == "0" {
			color = colorOK
		}
		line := "  exit " + done.exit
		if done.took != "" {
			line += " · took " + done.took
		}
		out = append(out, slabLine(colorToolBg, width, part{text: line, fg: color}))
	}
	return out
}

// messageSlab renders a user, agent, or error message: a bold label line over
// the body, word-wrapped and padded by lipgloss so the background reaches the
// full viewport width.
func messageSlab(label, body string, bg, fg, labelColor color.Color, width int) []string {
	slab := lipgloss.NewStyle().Background(bg).Foreground(fg).Width(width).Padding(0, 1)
	labelLine := lipgloss.NewStyle().Background(bg).Foreground(labelColor).Bold(true).Render(label)
	return strings.Split(slab.Render(labelLine+"\n"+body), "\n")
}

// thinkingLines renders a reasoning trace in a muted gray, visually quieter
// than agent messages.
func thinkingLines(body string, width int) []string {
	style := lipgloss.NewStyle().Foreground(colorFaint).Italic(true).Width(width)
	return strings.Split(style.Render("thinking\n"+body), "\n")
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

// normalizeOutput prepares raw tool output (shell results, error echoes) for
// display. It unifies carriage returns to newlines, drops leading and trailing
// blank lines, and strips trailing spaces per line, but preserves internal
// blank lines so the output keeps its shape.
func normalizeOutput(text string) string {
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
	return strings.Join(lines[start:end], "\n")
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
