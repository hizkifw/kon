package ui

import (
	"strings"
	"time"
	"unicode"

	tea "charm.land/bubbletea/v2"
	"charm.land/lipgloss/v2"
	"github.com/charmbracelet/x/ansi"
	"github.com/hizkifw/kon/internal/markdown"
	"github.com/hizkifw/kon/internal/tui"
)

// Dragging over the transcript selects it, and releasing the button copies
// the selection and clears it. A double click selects a word and a triple
// click a paragraph, and dragging on from either grows the selection by that
// unit. kon takes the mouse to scroll, so the terminal's own selection needs
// a modifier held, and it would copy replies as wrapped and styled for the
// screen. A reply is copied as the Markdown behind the selection instead, cut
// out by markdown.Excerpt; everything else is copied as shown.

// point is a cell of the transcript: a line of it and a screen column.
type point struct{ line, col int }

func (p point) before(q point) bool { return p.line < q.line || p.line == q.line && p.col < q.col }

// cells is a run of the transcript from one cell to another, both included.
type cells struct{ from, to point }

// unit is what a selection grows by as it is dragged: cells from a press,
// words from a double click, paragraphs from a triple click.
type unit int

const (
	byCell unit = iota
	byWord
	byParagraph
)

// selection is a stretch of the transcript being picked with the mouse. It
// lasts only while the button is held: releasing copies it and clears it.
type selection struct {
	unit unit
	// anchor is the unit pressed on, and head the unit under the pointer.
	anchor, head cells
	// edge is how many rows the pointer is past the top (negative) or the
	// bottom (positive) of the transcript, which scrolls it that way.
	edge int
}

// span returns the selection's first and last cells in reading order.
func (s *selection) span() (start, end point) {
	start, end = s.anchor.from, s.anchor.to
	if s.head.from.before(start) {
		start = s.head.from
	}
	if end.before(s.head.to) {
		end = s.head.to
	}
	return start, end
}

// click is the last press on the transcript, to tell a double or triple click
// from separate presses and to start a drag from.
type click struct {
	at    point
	when  time.Time
	count int // presses in a row, from 1 to 3
	down  bool
}

// multiClickInterval is how soon a press on the same cell must follow the
// last to count as a double or triple click.
const multiClickInterval = 400 * time.Millisecond

// transcriptTop is the screen row the transcript starts on, under the
// one-line header.
const transcriptTop = 1

// selectScrollInterval is how often a drag held past the top or bottom of the
// transcript scrolls it. Each tick scrolls a line per row the pointer is past
// the edge, up to maxSelectScroll, so reaching further scrolls faster.
const (
	selectScrollInterval = 50 * time.Millisecond
	maxSelectScroll      = 5
)

type selectScrollMsg struct{ epoch int }

func selectScrollTick(epoch int) tea.Cmd {
	return tea.Tick(selectScrollInterval, func(time.Time) tea.Msg { return selectScrollMsg{epoch} })
}

// selectionTextMsg carries a selection's text, worked out off the update
// loop, to be copied.
type selectionTextMsg struct{ text string }

var selectedStyle = lipgloss.NewStyle().Reverse(true)

// surface is the scrolling view the mouse works on and where it sits on
// screen: the top drawer's when one is open, otherwise the main transcript's.
func (m *Model) surface() (*tui.Scroll, tui.Rect) {
	if d := m.topDrawer(); d != nil {
		return d.View(), m.drawers.Body(m.drawers.Len() - 1)
	}
	return &m.viewport, tui.Rect{Y: transcriptTop, W: m.width, H: m.viewport.Height()}
}

// pointAt returns the transcript cell under screen cell (x, y), held to the
// lines the transcript has, and how many rows y lies past the top (negative)
// or the bottom (positive) of the transcript.
func (m Model) pointAt(x, y int) (point, int) {
	view, area := m.surface()
	row, edge := y-area.Y, 0
	if row < 0 {
		row, edge = 0, row
	} else if last := view.Height() - 1; row > last {
		row, edge = last, row-last
	}
	line := view.YOffset() + row
	if last := view.LineCount() - 1; line > last {
		// Below the last line, as on the blank row under it: the selection
		// runs to that line's end.
		return point{last, area.W}, edge
	}
	return point{line, max(0, x-area.X)}, edge
}

// pressMouse records a left press on the transcript. A single press selects
// nothing until the pointer moves; a double click selects the word pressed
// on and a triple click the paragraph, straight away.
func (m Model) pressMouse(msg tea.MouseClickMsg) (tea.Model, tea.Cmd) {
	if m.drawers.Len() > 0 {
		// The dimmed area around the top drawer only takes a click to close it.
		if r := m.drawers.Rect(m.drawers.Len() - 1); !r.Contains(msg.X, msg.Y) {
			m.closeDrawer()
			return m, nil
		}
		if msg.Button == tea.MouseLeft {
			if updated, cmd, ok := m.clickDrawer(msg.X, msg.Y); ok {
				return updated, cmd
			}
		}
		// A list has no text to select.
		if m.topDrawer().List != nil {
			return m, nil
		}
	}
	m.activeTranscript().selection = nil
	view, area := m.surface()
	if msg.Button != tea.MouseLeft || m.drawers.Len() == 0 && m.preview != nil || !area.Contains(msg.X, msg.Y) || view.LineCount() == 0 {
		m.click = click{}
		return m, nil
	}
	p, _ := m.pointAt(msg.X, msg.Y)
	now := time.Now()
	count := 1
	if p == m.click.at && now.Sub(m.click.when) < multiClickInterval {
		count = m.click.count%3 + 1
	}
	m.click = click{at: p, when: now, count: count, down: true}
	if u := unit(count - 1); u != byCell {
		if r, ok := m.activeTranscript().unitAt(p, u, area.W); ok {
			m.activeTranscript().selection = &selection{unit: u, anchor: r, head: r}
		}
	}
	return m, nil
}

// dragMouse grows the selection to the unit under the pointer, starting one
// once a single press moves off its cell. Past the top or bottom of the
// transcript it scrolls that way.
func (m Model) dragMouse(msg tea.MouseMotionMsg) (tea.Model, tea.Cmd) {
	if !m.click.down {
		return m, nil
	}
	p, edge := m.pointAt(msg.X, msg.Y)
	sel := m.activeTranscript().selection
	if sel == nil {
		// A pointer past the edge has left the pressed cell, though the
		// cell it is held to is that one.
		if p == m.click.at && edge == 0 {
			return m, nil
		}
		pressed := cells{m.click.at, m.click.at}
		sel = &selection{unit: byCell, anchor: pressed, head: pressed}
		m.activeTranscript().selection = sel
	}
	m.moveHead(p)
	scroll := edge != 0 && sel.edge == 0
	sel.edge = edge
	if scroll {
		m.selectEpoch++
		return m, selectScrollTick(m.selectEpoch)
	}
	return m, nil
}

// moveHead moves the selection's head to the unit at p, or to p alone when no
// unit is there, as on a blank line.
func (m *Model) moveHead(p point) {
	sel := m.activeTranscript().selection
	_, area := m.surface()
	r, ok := m.activeTranscript().unitAt(p, sel.unit, area.W)
	if !ok {
		r = cells{p, p}
	}
	sel.head = r
}

// wheelMouse scrolls the transcript, and a drag in progress takes in the
// lines the wheel brings under the pointer.
func (m Model) wheelMouse(msg tea.MouseWheelMsg) (tea.Model, tea.Cmd) {
	view, _ := m.surface()
	view.Update(msg)
	if t := m.activeTranscript(); t != nil && t.selection != nil {
		p, _ := m.pointAt(msg.X, msg.Y)
		m.moveHead(p)
	}
	return m, nil
}

// scrollSelection scrolls toward the edge a drag is held past and moves the
// selection's head onto the line brought in.
func (m Model) scrollSelection(msg selectScrollMsg) (tea.Model, tea.Cmd) {
	if m.activeTranscript() == nil {
		return m, nil
	}
	sel := m.activeTranscript().selection
	if sel == nil || sel.edge == 0 || msg.epoch != m.selectEpoch {
		return m, nil
	}
	view, area := m.surface()
	view.SetYOffset(view.YOffset() + max(-maxSelectScroll, min(sel.edge, maxSelectScroll)))
	row := area.Y
	if sel.edge > 0 {
		row += view.Height() - 1
	}
	p, _ := m.pointAt(area.X+sel.head.to.col, row)
	m.moveHead(p)
	return m, selectScrollTick(msg.epoch)
}

// releaseMouse copies what the press selected and clears the selection. A
// click without a drag selects nothing, so clicking to focus the window never
// replaces what is on the clipboard.
func (m Model) releaseMouse(tea.MouseReleaseMsg) (tea.Model, tea.Cmd) {
	if m.activeTranscript() == nil {
		return m, nil
	}
	sel := m.activeTranscript().selection
	m.activeTranscript().selection = nil
	m.click.down = false
	if sel == nil {
		return m, nil
	}
	start, end := sel.span()
	_, area := m.surface()
	parts := m.activeTranscript().selectedParts(start, end, area.W)
	if len(parts) == 0 {
		return m, m.flash(toneInfo, "nothing to copy in the selection")
	}
	return m, copySelection(parts)
}

// copied puts a worked-out selection on the clipboard.
func (m Model) copied(msg selectionTextMsg) (tea.Model, tea.Cmd) {
	if strings.TrimSpace(msg.text) == "" {
		return m, m.flash(toneInfo, "nothing to copy in the selection")
	}
	return m, tea.Batch(m.flash(toneSuccess, "copied selection"), copyToClipboard(msg.text))
}

// unitAt returns the unit u that covers p: the cell itself, the word there,
// or the paragraph, the run of lines around p with no blank line between.
// ok is false when p is on no word or paragraph.
func (t *transcript) unitAt(p point, u unit, width int) (cells, bool) {
	switch u {
	case byWord:
		from, to, ok := wordAt(ansi.Strip(t.lines[p.line]), p.col)
		return cells{point{p.line, from}, point{p.line, to}}, ok
	case byParagraph:
		blank := func(i int) bool { return strings.TrimSpace(ansi.Strip(t.lines[i])) == "" }
		if blank(p.line) {
			return cells{}, false
		}
		first, last := p.line, p.line
		for first > 0 && !blank(first-1) {
			first--
		}
		for last < len(t.lines)-1 && !blank(last+1) {
			last++
		}
		return cells{point{first, 0}, point{last, width}}, true
	}
	return cells{p, p}, true
}

// wordAt returns the first and last cells of the word covering cell col of
// line, as shown. A word is a run of letters and digits along with the
// punctuation that joins the parts of a name, a path, or a URL, so one double
// click takes "fmt.Println" or "https://go.dev/doc" whole. Punctuation that
// ends a sentence is left off the end.
func wordAt(line string, col int) (from, to int, ok bool) {
	type cluster struct {
		text        string
		start, cols int
	}
	var clusters []cluster
	at := -1
	for i, c := 0, 0; i < len(line); {
		text, w := ansi.FirstGraphemeCluster(line[i:], ansi.GraphemeWidth)
		if c <= col && col < c+w {
			at = len(clusters)
		}
		clusters = append(clusters, cluster{text, c, w})
		i, c = i+len(text), c+w
	}
	isWord := func(i int) bool {
		r := []rune(clusters[i].text)[0]
		return unicode.IsLetter(r) || unicode.IsDigit(r) || strings.ContainsRune("_-./:@~+#%&=?$", r)
	}
	if at < 0 || !isWord(at) {
		return 0, 0, false
	}
	first, last := at, at
	for first > 0 && isWord(first-1) {
		first--
	}
	for last < len(clusters)-1 && isWord(last+1) {
		last++
	}
	hasText := false
	for i := first; i <= last; i++ {
		if r := []rune(clusters[i].text)[0]; unicode.IsLetter(r) || unicode.IsDigit(r) {
			hasText = true
		}
	}
	for hasText && last > at && strings.ContainsRune(".,:;!?", []rune(clusters[last].text)[0]) {
		last--
	}
	return clusters[first].start, clusters[last].start + clusters[last].cols - 1, true
}

// highlight paints the selected cells of line i in reverse video, as plain
// text: a selection shows where it runs, not the styles under it. Only the
// line's text is painted, so the slab's margins and padding, a line's
// indentation, and a blank line stay as they are.
func (t *transcript) highlight(i int, line string, width int) string {
	if t.selection == nil {
		return line
	}
	start, end := t.selection.span()
	if i < start.line || i > end.line {
		return line
	}
	plain := ansi.Strip(line)
	text := strings.TrimRight(plain, " ")
	from := len(text) - len(strings.TrimLeft(text, " "))
	to := ansi.StringWidth(text)
	if i == start.line {
		from = max(from, start.col)
	}
	if i == end.line {
		to = min(to, end.col+1)
	}
	if from >= to {
		return line
	}
	return ansi.Cut(line, 0, from) + selectedStyle.Render(ansi.Cut(plain, from, to)) + ansi.Cut(line, to, width)
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
	// shown is the selected text as shown. It is what is copied of anything
	// but a reply, and of a reply's text the renderer adds, such as a link's
	// destination, which has no source to cut out.
	shown string
	// prompt marks the user's own words, quoted when a selection holds more.
	prompt bool
}

// selectedParts splits the cells from start to end by the message each line
// shows.
func (t *transcript) selectedParts(start, end point, width int) []selectedPart {
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
		shown := t.shownText(from, to)
		if r.block >= 0 && t.blocks[r.block].kind == blockAssistant {
			parts = append(parts, selectedPart{
				reply: t.blocks[r.block].text, width: width,
				from: point{from.line - r.first, from.col}, to: point{to.line - r.first, to.col},
				shown: shown,
			})
			continue
		}
		if shown != "" {
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
	if excerpt := p.excerpt(); excerpt != "" {
		return excerpt
	}
	return p.shown
}

// excerpt returns the Markdown behind a reply's selected cells, or "" when
// they hold no text from its source.
func (p selectedPart) excerpt() string {
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
