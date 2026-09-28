package ui

import (
	"strings"
	"time"

	tea "charm.land/bubbletea/v2"
	"charm.land/lipgloss/v2"
	"github.com/charmbracelet/x/ansi"
	"github.com/hizkifw/kon/internal/markdown"
)

// Dragging over the transcript selects it, and releasing the button copies
// the selection. kon takes the mouse to scroll, so the terminal's own
// selection needs a modifier held, and it would copy replies as wrapped and
// styled for the screen. A reply is copied as the Markdown behind the
// selection instead, cut out by markdown.Excerpt; everything else is copied
// as shown.

// point is a cell of the transcript: a line of it and a screen column.
type point struct{ line, col int }

func (p point) before(q point) bool { return p.line < q.line || p.line == q.line && p.col < q.col }

// selection is a stretch of the transcript picked with the mouse, from the
// cell pressed (anchor) to the cell the pointer is on (head), both included.
type selection struct {
	anchor, head point
	// dragging is set while the button is held. edge is -1 or 1 while the
	// pointer is above or below the transcript, which scrolls it that way.
	dragging bool
	edge     int
}

// span returns the selection's first and last cells in reading order.
func (s *selection) span() (start, end point) {
	if s.head.before(s.anchor) {
		return s.head, s.anchor
	}
	return s.anchor, s.head
}

// transcriptTop is the screen row the transcript starts on, under the
// one-line header.
const transcriptTop = 1

// selectScrollInterval is how often a drag held past the top or bottom of the
// transcript scrolls it by a line.
const selectScrollInterval = 50 * time.Millisecond

type selectScrollMsg struct{ epoch int }

func selectScrollTick(epoch int) tea.Cmd {
	return tea.Tick(selectScrollInterval, func(time.Time) tea.Msg { return selectScrollMsg{epoch} })
}

// selectionTextMsg carries a selection's text, worked out off the update
// loop, to be copied.
type selectionTextMsg struct{ text string }

var selectedStyle = lipgloss.NewStyle().Reverse(true)

// pointAt returns the transcript cell under screen cell (x, y), held to the
// lines the transcript has, and which edge the row lies past: -1 above the
// transcript, 1 below it, 0 on it.
func (m Model) pointAt(x, y int) (point, int) {
	row, edge := y-transcriptTop, 0
	if row < 0 {
		row, edge = 0, -1
	} else if row >= m.viewport.Height() {
		row, edge = m.viewport.Height()-1, 1
	}
	line := m.viewport.YOffset() + row
	if last := len(m.viewport.lines) - 1; line > last {
		// Below the last line, as on the blank row under it: the selection
		// runs to that line's end.
		return point{last, m.width}, edge
	}
	return point{line, max(0, x)}, edge
}

// pressMouse starts a selection at a left click on the transcript. Any click
// drops the selection that was there.
func (m Model) pressMouse(msg tea.MouseClickMsg) (tea.Model, tea.Cmd) {
	m.transcript.selection = nil
	row := msg.Y - transcriptTop
	if msg.Button != tea.MouseLeft || m.preview != nil || row < 0 || row >= m.viewport.Height() || len(m.viewport.lines) == 0 {
		return m, nil
	}
	p, _ := m.pointAt(msg.X, msg.Y)
	m.transcript.selection = &selection{anchor: p, head: p, dragging: true}
	return m, nil
}

// dragMouse moves the selection's head with the pointer. Past the top or
// bottom of the transcript it starts scrolling that way.
func (m Model) dragMouse(msg tea.MouseMotionMsg) (tea.Model, tea.Cmd) {
	sel := m.transcript.selection
	if sel == nil || !sel.dragging {
		return m, nil
	}
	p, edge := m.pointAt(msg.X, msg.Y)
	sel.head = p
	start := edge != 0 && sel.edge == 0
	sel.edge = edge
	if start {
		m.selectEpoch++
		return m, selectScrollTick(m.selectEpoch)
	}
	return m, nil
}

// scrollSelection scrolls a line toward the edge a drag is held past and
// moves the selection's head onto the line brought in.
func (m Model) scrollSelection(msg selectScrollMsg) (tea.Model, tea.Cmd) {
	sel := m.transcript.selection
	if sel == nil || !sel.dragging || sel.edge == 0 || msg.epoch != m.selectEpoch {
		return m, nil
	}
	m.viewport.SetYOffset(m.viewport.YOffset() + sel.edge)
	row := transcriptTop
	if sel.edge > 0 {
		row += m.viewport.Height() - 1
	}
	sel.head, _ = m.pointAt(sel.head.col, row)
	return m, selectScrollTick(msg.epoch)
}

// releaseMouse ends a drag and copies what it selected. A click without a
// drag selects nothing, so clicking to focus the window never replaces what
// is on the clipboard.
func (m Model) releaseMouse(msg tea.MouseReleaseMsg) (tea.Model, tea.Cmd) {
	sel := m.transcript.selection
	if sel == nil || !sel.dragging {
		return m, nil
	}
	if msg.Y >= transcriptTop && msg.Y < transcriptTop+m.viewport.Height() {
		sel.head, _ = m.pointAt(msg.X, msg.Y)
	}
	sel.dragging, sel.edge = false, 0
	if sel.head == sel.anchor {
		m.transcript.selection = nil
		return m, nil
	}
	parts := m.transcript.selectedParts(m.width)
	if len(parts) == 0 {
		m.status = "nothing to copy in the selection"
		return m, nil
	}
	return m, copySelection(parts)
}

// copied puts a worked-out selection on the clipboard.
func (m Model) copied(msg selectionTextMsg) (tea.Model, tea.Cmd) {
	if strings.TrimSpace(msg.text) == "" {
		m.status = "nothing to copy in the selection"
		return m, nil
	}
	m.status = "copied selection"
	return m, copyToClipboard(msg.text)
}

// highlight paints the selected cells of line i in reverse video, as plain
// text: a selection shows where it runs, not the styles under it.
func (t *transcript) highlight(i int, line string, width int) string {
	if t.selection == nil {
		return line
	}
	start, end := t.selection.span()
	if i < start.line || i > end.line {
		return line
	}
	from, to := 0, width
	if i == start.line {
		from = start.col
	}
	if i == end.line {
		to = end.col + 1
	}
	if from >= to {
		return line
	}
	return ansi.Cut(line, 0, from) + selectedStyle.Render(ansi.Strip(ansi.Cut(line, from, to))) + ansi.Cut(line, to, width)
}

// region is a run of transcript lines shown by one chunk, the banner, or the
// live stream. block is the first block the chunk was rendered from, or -1.
type region struct {
	first, n int
	block    int
	live     bool
}

// regions lays out the lines of the transcript's last render: the banner,
// then each chunk with a blank line after it, then the live stream. It mirrors
// linesFor, which it reads the parts of.
func (t *transcript) regions() []region {
	var out []region
	line := 0
	if t.cacheBanner != "" {
		n := strings.Count(t.cacheBanner, "\n") + 1
		out = append(out, region{first: 0, n: n, block: -1})
		line = n + 1
	}
	for i, chunk := range t.chunks {
		n := strings.Count(chunk, "\n") + 1
		out = append(out, region{first: line, n: n, block: t.chunkFrom[i]})
		line += n + 1
	}
	// A tool run still in progress is drawn after the chunks, from its blocks.
	if tail := strings.TrimPrefix(strings.TrimPrefix(t.cacheBase, t.joined), "\n\n"); t.built < len(t.blocks) && tail != "" {
		out = append(out, region{first: line, n: strings.Count(tail, "\n") + 1, block: t.built})
	}
	if start := t.liveStart() - t.liveFin; t.active != nil && start < len(t.lines)-t.timerLines {
		out = append(out, region{first: start, n: len(t.lines) - t.timerLines - start, block: -1, live: true})
	}
	return out
}

// selectedPart is one message's share of a selection, taken when the button
// is released so the text can be worked out off the update loop.
type selectedPart struct {
	// reply is the Markdown behind an agent reply, rendered at width, and
	// from and to are the selection's first and last cells in its lines.
	reply    string
	width    int
	from, to point
	// shown is the selected text as shown, for anything but a reply.
	shown string
	// prompt marks the user's own words, quoted when a selection holds more.
	prompt bool
}

// selectedParts splits the selection by the message each line shows.
func (t *transcript) selectedParts(width int) []selectedPart {
	start, end := t.selection.span()
	var parts []selectedPart
	for _, r := range t.regions() {
		last := r.first + r.n - 1
		if last < start.line || r.first > end.line || r.block < 0 && !r.live {
			continue
		}
		from, to := start, end
		if from.line < r.first {
			from = point{r.first, 0}
		}
		if to.line > last {
			to = point{last, width}
		}
		if r.block >= 0 && t.blocks[r.block].kind == blockAssistant {
			parts = append(parts, selectedPart{
				reply: t.blocks[r.block].text, width: width,
				from: point{from.line - r.first, from.col}, to: point{to.line - r.first, to.col},
			})
			continue
		}
		if shown := t.shownText(from, to); shown != "" {
			parts = append(parts, selectedPart{shown: shown, prompt: r.block >= 0 && t.blocks[r.block].kind == blockUser})
		}
	}
	return parts
}

// shownText returns the text shown from cell from to cell to, both included,
// without the indentation the lines share, which is the slab's rather than
// the text's. It is measured on the whole lines, so a line cut inside that
// indentation or right after it loses only what it still holds of it.
func (t *transcript) shownText(from, to point) string {
	indent := -1
	for i := from.line; i <= to.line; i++ {
		if line := strings.TrimRight(ansi.Strip(t.lines[i]), " "); line != "" {
			if n := len(line) - len(strings.TrimLeft(line, " ")); indent < 0 || n < indent {
				indent = n
			}
		}
	}
	var lines []string
	for i := from.line; i <= to.line; i++ {
		a, b := 0, len(t.lines[i])
		if i == from.line {
			a = from.col
		}
		if i == to.line {
			b = to.col + 1
		}
		line := strings.TrimRight(ansi.Cut(ansi.Strip(t.lines[i]), a, b), " ")
		spaces := len(line) - len(strings.TrimLeft(line, " "))
		lines = append(lines, line[min(max(indent-a, 0), spaces):])
	}
	return strings.Trim(strings.Join(lines, "\n"), "\n")
}

// copySelection works out the text of a selection's parts off the update
// loop: a reply is rendered again with its source map, which the screen
// never needs, to find the Markdown behind the selection.
func copySelection(parts []selectedPart) tea.Cmd {
	return func() tea.Msg {
		texts := make([]string, 0, len(parts))
		for _, p := range parts {
			text := p.text()
			if text == "" {
				continue
			}
			if p.prompt && len(parts) > 1 {
				text = quoted(text)
			}
			texts = append(texts, text)
		}
		return selectionTextMsg{strings.Join(texts, "\n\n")}
	}
}

func (p selectedPart) text() string {
	if p.reply == "" {
		return p.shown
	}
	lines := markdown.RenderWithSource(p.reply, markdown.Theme{}, markdownContentWidth(p.width))
	if p.from.line >= len(lines) {
		return ""
	}
	to := p.to
	if to.line >= len(lines) {
		to = point{len(lines) - 1, p.width}
	}
	// A reply is painted one cell in from the left edge, so screen column c
	// shows cell c-1 of its line, and a selection through column c ends
	// after that cell.
	startCol := byteAt(lines[p.from.line].Text, p.from.col-1)
	endCol := byteEnd(lines[to.line].Text, to.col)
	start, end, ok := markdown.SelectionSource(lines, p.from.line, startCol, to.line, endCol)
	if !ok {
		return ""
	}
	return markdown.Excerpt(p.reply, start, end)
}

// byteAt returns where the character covering cell col of text starts, or
// the end of text when col lies past it.
func byteAt(text string, col int) int {
	cells := 0
	for i := 0; i < len(text); {
		cluster, width := ansi.FirstGraphemeCluster(text[i:], ansi.GraphemeWidth)
		if cells+width > col {
			return i
		}
		cells += width
		i += len(cluster)
	}
	return len(text)
}

// byteEnd returns where a run of cells up to col, not included, ends in text:
// after the character covering cell col-1.
func byteEnd(text string, col int) int {
	if col <= 0 {
		return 0
	}
	cells := 0
	for i := 0; i < len(text); {
		cluster, width := ansi.FirstGraphemeCluster(text[i:], ansi.GraphemeWidth)
		if cells+width >= col {
			return i + len(cluster)
		}
		cells += width
		i += len(cluster)
	}
	return len(text)
}
