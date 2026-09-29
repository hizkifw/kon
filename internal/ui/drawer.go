package ui

import (
	"strings"

	tea "charm.land/bubbletea/v2"
	"charm.land/lipgloss/v2"
	"github.com/charmbracelet/x/ansi"
)

// A drawer is a surface painted over the whole screen, showing a transcript
// of its own: a side answer today, and anything else that reads as blocks of
// text. Drawers stack. Only the top one takes keys and the mouse, and
// everything under it is dimmed, so there is never more than one surface to
// scroll or select in. Esc, Ctrl+C, or a click on the dimmed area closes the
// top drawer; what is behind it keeps running and repainting.
type drawer struct {
	title      string
	transcript *transcript
	view       scrollView
	// onClose releases what the drawer's owner holds, such as a request still
	// streaming into it. It runs after the drawer is off the stack.
	onClose func(m *Model)
}

// Drawers open from the right at full height. Each covers nine tenths of the
// width the one under it covers, so every level of the stack leaves a strip of
// the level below showing.
const drawerShareNum, drawerShareDen = 9, 10

// colorDim paints everything under the top drawer.
var colorDim = lipgloss.Color("#4A4A4A")

type rect struct{ x, y, w, h int }

func (r rect) contains(x, y int) bool {
	return x >= r.x && x < r.x+r.w && y >= r.y && y < r.y+r.h
}

// drawerRect is the screen area of the drawer at level in the stack, counted
// from the bottom.
func drawerRect(width, height, level int) rect {
	r := rect{0, 0, width, height}
	for range level + 1 {
		w := max(2, r.w*drawerShareNum/drawerShareDen)
		r.x, r.w = r.x+r.w-w, w
	}
	return r
}

// drawerBody is the part of a drawer's area its transcript scrolls in: below
// the title row, and right of the rule that edges the drawer.
func drawerBody(r rect) rect {
	return rect{r.x + 1, r.y + 1, max(1, r.w-1), max(1, r.h-1)}
}

// topDrawer is the drawer that takes input, or nil when none is open.
func (m *Model) topDrawer() *drawer {
	if len(m.drawers) == 0 {
		return nil
	}
	return m.drawers[len(m.drawers)-1]
}

// openDrawer pushes d onto the stack, scrolled to its end.
func (m *Model) openDrawer(d *drawer) {
	d.view = newScrollView()
	m.drawers = append(m.drawers, d)
	m.click = click{}
	m.layoutDrawers()
	d.view.GotoBottom()
}

// closeDrawer pops the top drawer.
func (m *Model) closeDrawer() {
	d := m.topDrawer()
	if d == nil {
		return
	}
	d.transcript.selection = nil
	m.drawers = m.drawers[:len(m.drawers)-1]
	m.click = click{}
	if d.onClose != nil {
		d.onClose(m)
	}
}

// closeDrawers pops every drawer, top first.
func (m *Model) closeDrawers() {
	for len(m.drawers) > 0 {
		m.closeDrawer()
	}
}

// layoutDrawers sizes each drawer to its place in the stack after the window
// or the stack changed.
func (m *Model) layoutDrawers() {
	for i, d := range m.drawers {
		body := drawerBody(drawerRect(m.width, m.height, i))
		d.view.SetWidth(body.w)
		d.view.SetHeight(body.h)
		d.view.SetContentLines(d.transcript.linesFor(body.w))
	}
}

// refreshDrawers repaints each drawer's transcript, following new output in
// the ones scrolled to their end.
func (m *Model) refreshDrawers() {
	for _, d := range m.drawers {
		follow := d.view.AtBottom()
		d.view.SetContentLines(d.transcript.linesFor(d.view.Width()))
		if follow {
			d.view.GotoBottom()
		}
	}
}

// drawerKey handles a key while a drawer is open. The prompt underneath is
// not reachable, so keys it would take do nothing.
func (m Model) drawerKey(key string) (tea.Model, tea.Cmd) {
	d := m.topDrawer()
	switch key {
	case "esc", "ctrl+c":
		m.closeDrawer()
	case "ctrl+d":
		m.closeDrawers()
		if m.turn != nil {
			m.turn.cancel()
		}
		return m, tea.Quit
	case "pgup", "up":
		d.view.PageUp()
	case "pgdown", "down":
		d.view.PageDown()
	case "home":
		d.view.SetYOffset(0)
	case "end":
		d.view.GotoBottom()
	}
	return m, nil
}

// paintDrawers draws the stack over screen, which holds one line per row.
// Everything under the top drawer, including lower drawers, is dimmed.
func (m Model) paintDrawers(screen []string) []string {
	for len(screen) < m.height {
		screen = append(screen, "")
	}
	screen = screen[:m.height]
	top := len(m.drawers) - 1
	for i, d := range m.drawers {
		if i == top {
			dimScreen(screen)
		}
		r := drawerRect(m.width, m.height, i)
		for row, line := range d.render(r) {
			screen[r.y+row] = spliceLine(screen[r.y+row], line, r.x, r.w, m.width)
		}
	}
	return screen
}

var (
	drawerTitleStyle = lipgloss.NewStyle().Foreground(colorBarFg).Background(colorBarBg)
	drawerRuleStyle  = lipgloss.NewStyle().Foreground(colorFaint)
	dimStyle         = lipgloss.NewStyle().Foreground(colorDim)
)

// render draws the drawer at r as exactly r.h lines of r.w cells: a rule down
// its left edge beside a title row and the transcript.
func (d *drawer) render(r rect) []string {
	body := drawerBody(r)
	lines := make([]string, 0, r.h)
	lines = append(lines, drawerTitleStyle.Width(body.w).Render(fitLine(" "+d.title, body.w)))
	view := d.view.View()
	if t := d.transcript; t.selection != nil {
		view = d.view.ViewWith(func(i int, line string) string { return t.highlight(i, line, body.w) })
	}
	for _, line := range strings.Split(view, "\n") {
		lines = append(lines, padLine(line, body.w))
	}
	rule := drawerRuleStyle.Render("│")
	for i := range lines {
		lines[i] = rule + lines[i]
	}
	return lines
}

// padLine fills line out to width cells, so nothing under the drawer shows
// through a short line.
func padLine(line string, width int) string {
	if gap := width - ansi.StringWidth(line); gap > 0 {
		return line + strings.Repeat(" ", gap)
	}
	return ansi.Truncate(line, width, "")
}

// spliceLine replaces the w cells of line from column x with over. Resets on
// both sides keep the under line's styles from bleeding into over or past it.
func spliceLine(line, over string, x, w, width int) string {
	left := padLine(ansi.Truncate(line, x, ""), x)
	return left + "\x1b[m" + over + "\x1b[m" + ansi.Cut(line, x+w, width)
}

// dimScreen repaints every line flat in the dim color, dropping its own
// colors so no slab or highlight stands out from under a drawer.
func dimScreen(lines []string) {
	for i, line := range lines {
		lines[i] = dimStyle.Render(ansi.Strip(line))
	}
}
