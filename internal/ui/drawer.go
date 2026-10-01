package ui

import (
	"image/color"
	"strings"

	tea "charm.land/bubbletea/v2"
	"charm.land/lipgloss/v2"
	"github.com/charmbracelet/x/ansi"
	"github.com/hizkifw/kon/internal/tui"
)

// A drawer is a surface painted over the whole screen, showing a transcript
// of its own, such as a side answer or a job's output, or a list to pick
// from. Drawers stack. Only the top one takes keys and the mouse, and
// everything under it is dimmed, so there is never more than one surface to
// scroll or select in. Esc, Ctrl+C, or a click on the dimmed area closes the
// top drawer; what is behind it keeps running and repainting.
type drawer struct {
	title string
	// transcript is what the drawer shows, unless list is set: then it
	// shows the list's rows, one per line, with the highlighted one marked.
	transcript *transcript
	list       *menu
	// actions returns what the drawer does beyond scrolling and closing, as
	// things stand now, so an action that no longer applies is not offered.
	// Nil means none.
	actions func(m *Model) []drawerAction
	// armed is the key of an action pressed once that asks for a second
	// press; any other key disarms it.
	armed string
	view  tui.Scroll
	// onClose releases what the drawer's owner holds, such as a request still
	// streaming into it. It runs after the drawer is off the stack.
	onClose func(m *Model)
}

// drawerAction is a key a drawer answers to. The drawer's hint row names
// each one, and a click on its name runs it as the key would.
type drawerAction struct {
	// key is the key as tea.KeyPressMsg.String reports it, and hint how
	// the hint row writes it: "⇧K" for "K", so the Shift is plain to see.
	key, hint, label string
	// confirm, when set, is what the hint row asks after a first press;
	// only a second press runs the action. A destructive action has one.
	confirm string
	run     func(m *Model) tea.Cmd
}

// Drawers open from the right at full height. Each covers nine tenths of the
// width the one under it covers, so every level of the stack leaves a strip of
// the level below showing.
const drawerShareNum, drawerShareDen = 9, 10

// colorDim paints everything under the top drawer.
var colorDim = lipgloss.Color("#4A4A4A")

// drawerRect is the screen area of the drawer at level in the stack, counted
// from the bottom.
func drawerRect(width, height, level int) tui.Rect {
	r := tui.Rect{W: width, H: height}
	for range level + 1 {
		w := max(2, r.W*drawerShareNum/drawerShareDen)
		r.X, r.W = r.X+r.W-w, w
	}
	return r
}

// drawerBody is the part of a drawer's area its content scrolls in: between
// the title row and the hint row, and right of the rule that edges the
// drawer.
func drawerBody(r tui.Rect) tui.Rect {
	return tui.Rect{X: r.X + 1, Y: r.Y + 1, W: max(1, r.W-1), H: max(1, r.H-2)}
}

// topDrawer is the drawer that takes input, or nil when none is open.
func (m *Model) topDrawer() *drawer {
	if len(m.drawers) == 0 {
		return nil
	}
	return m.drawers[len(m.drawers)-1]
}

// openDrawer pushes d onto the stack.
func (m *Model) openDrawer(d *drawer) {
	d.view = tui.NewScroll()
	m.drawers = append(m.drawers, d)
	m.click = click{}
	m.layoutDrawers()
	// A list starts at its top, anything else at its latest line.
	if d.list == nil {
		d.view.GotoBottom()
	}
}

// closeDrawer pops the top drawer.
func (m *Model) closeDrawer() {
	d := m.topDrawer()
	if d == nil {
		return
	}
	if d.transcript != nil {
		d.transcript.selection = nil
	}
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
		d.view.SetWidth(body.W)
		d.view.SetHeight(body.H)
		d.view.SetContentLines(d.lines(body.W))
	}
}

// refreshDrawers repaints each drawer, following new output in the
// transcripts scrolled to their end.
func (m *Model) refreshDrawers() {
	for _, d := range m.drawers {
		follow := d.list == nil && d.view.AtBottom()
		d.view.SetContentLines(d.lines(d.view.Width()))
		if follow {
			d.view.GotoBottom()
		}
	}
}

// drawerKey handles a key while a drawer is open. The prompt underneath is
// not reachable, so keys it would take do nothing, and plain letters are free
// for scrolling and for the drawer's actions.
func (m Model) drawerKey(key string) (tea.Model, tea.Cmd) {
	d := m.topDrawer()
	armed := d.armed
	d.armed = ""
	switch key {
	case "esc", "ctrl+c":
		m.closeDrawer()
		return m, nil
	case "ctrl+d":
		m.closeDrawers()
		if m.turn != nil {
			m.turn.cancel()
		}
		return m, tea.Quit
	}
	for _, action := range d.currentActions(&m) {
		if action.key == key {
			if action.confirm != "" && armed != key {
				d.armed = key
				return m, nil
			}
			cmd := action.run(&m)
			m.refreshDrawers()
			return m, cmd
		}
	}
	if d.list != nil {
		d.moveList(key)
		return m, nil
	}
	switch key {
	case "up", "k":
		d.view.SetYOffset(d.view.YOffset() - 1)
	case "down", "j":
		d.view.SetYOffset(d.view.YOffset() + 1)
	case "pgup":
		d.view.PageUp()
	case "pgdown":
		d.view.PageDown()
	case "home":
		d.view.SetYOffset(0)
	case "end":
		d.view.GotoBottom()
	}
	return m, nil
}

// moveList moves a list drawer's highlight for key, scrolling to keep it in
// view.
func (d *drawer) moveList(key string) {
	items := d.list.items
	// step moves from row i by one row in dir, past headings, staying put
	// at either end.
	step := func(i, dir int) int {
		for j := i + dir; j >= 0 && j < len(items); j += dir {
			if !items[j].Heading {
				return j
			}
		}
		return i
	}
	index := d.list.index
	switch key {
	case "up", "k":
		index = step(index, -1)
	case "down", "j":
		index = step(index, 1)
	case "pgup":
		for range d.view.Height() {
			index = step(index, -1)
		}
	case "pgdown":
		for range d.view.Height() {
			index = step(index, 1)
		}
	case "home":
		index = step(-1, 1)
	case "end":
		index = step(len(items), -1)
	default:
		return
	}
	if index >= 0 && index < len(items) {
		d.list.index = index
	}
	d.showSelected()
}

// showSelected repaints a list drawer and scrolls its highlighted row into
// view.
func (d *drawer) showSelected() {
	d.view.SetContentLines(d.lines(d.view.Width()))
	switch row := d.list.index; {
	case row < d.view.YOffset():
		d.view.SetYOffset(row)
	case row >= d.view.YOffset()+d.view.Height():
		d.view.SetYOffset(row - d.view.Height() + 1)
	}
}

// currentActions is what the drawer offers now.
func (d *drawer) currentActions(m *Model) []drawerAction {
	if d.actions == nil {
		return nil
	}
	return d.actions(m)
}

// lines is the drawer's content at width: its transcript, or its list.
func (d *drawer) lines(width int) []string {
	if d.list == nil {
		return d.transcript.linesFor(width)
	}
	lines := make([]string, len(d.list.items))
	for i, item := range d.list.items {
		lines[i] = listRow(item, i == d.list.index, width)
	}
	return lines
}

// listRow renders one row of a drawer's list at width. Each part is fitted
// before it is styled, and a highlighted row carries its background through
// every part, so a part's colors cannot punch a hole in the highlight.
func listRow(item menuItem, selected bool, width int) string {
	if item.Heading {
		return lipgloss.NewStyle().Foreground(colorFaint).Bold(true).Render(tui.Fit(" "+item.Label, width))
	}
	base := lipgloss.NewStyle()
	if selected {
		base = base.Foreground(lipgloss.Color("#DADADA")).Background(lipgloss.Color("#333333"))
	}
	var out strings.Builder
	used := 0
	add := func(text string, fg color.Color) {
		if text == "" || used >= width {
			return
		}
		text = tui.Fit(text, width-used)
		used += ansi.StringWidth(text)
		style := base
		if fg != nil {
			style = style.Foreground(fg)
		}
		out.WriteString(style.Render(text))
	}
	add(" "+item.Label, nil)
	if item.Badge != "" {
		add("  ", nil)
		add(item.Badge, item.BadgeColor)
	}
	if item.Description != "" {
		add("  ", nil)
		add(item.Description, colorFaint)
	}
	if selected && used < width {
		out.WriteString(base.Render(strings.Repeat(" ", width-used)))
	}
	return out.String()
}

// hint is one entry of a drawer's hint row, the key a click on it presses,
// and the cells it spans there.
type hint struct {
	text, key string
	x, w      int
}

// hints lays out the hint row: each action's key and label, then Esc, which
// goes back to the drawer below or, from the last one, closes. After an
// action's first press the row instead asks for the second.
func (m *Model) hints(d *drawer) []hint {
	var out []hint
	x := 1
	add := func(text, key string) {
		if len(out) > 0 {
			x += len(" · ")
		}
		out = append(out, hint{text: text, key: key, x: x, w: ansi.StringWidth(text)})
		x += ansi.StringWidth(text)
	}
	actions := d.currentActions(m)
	for _, action := range actions {
		if action.key == d.armed {
			add(action.confirm, action.key)
			add("any other key cancels", "")
			return out
		}
	}
	for _, action := range actions {
		add(action.hint+" "+action.label, action.key)
	}
	esc := "esc close"
	if len(m.drawers) > 1 {
		esc = "esc back"
	}
	add(esc, "esc")
	return out
}

// clickDrawer handles a press inside the top drawer that is not on its
// content: a hint runs its action, as its key would.
func (m Model) clickDrawer(x, y int) (tea.Model, tea.Cmd, bool) {
	d := m.topDrawer()
	body := drawerBody(drawerRect(m.width, m.height, len(m.drawers)-1))
	switch {
	case y == body.Y+body.H:
		for _, h := range m.hints(d) {
			if x >= body.X+h.x && x < body.X+h.x+h.w {
				updated, cmd := m.drawerKey(h.key)
				return updated, cmd, true
			}
		}
		return m, nil, true
	case d.list != nil && body.Contains(x, y):
		// A click highlights a row, and a click on the highlighted row
		// opens it, as Enter would.
		row := d.view.YOffset() + y - body.Y
		if row >= len(d.list.items) || d.list.items[row].Heading {
			return m, nil, true
		}
		if row != d.list.index {
			d.list.index = row
			d.armed = ""
			d.showSelected()
			return m, nil, true
		}
		updated, cmd := m.drawerKey("enter")
		return updated, cmd, true
	}
	return m, nil, false
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
		for row, line := range m.renderDrawer(d, r) {
			screen[r.Y+row] = tui.Splice(screen[r.Y+row], line, r.X, r.W, m.width)
		}
	}
	return screen
}

var (
	drawerTitleStyle = lipgloss.NewStyle().Foreground(colorBarFg).Background(colorBarBg)
	drawerRuleStyle  = lipgloss.NewStyle().Foreground(colorFaint)
	dimStyle         = lipgloss.NewStyle().Foreground(colorDim)
)

// render draws the drawer at r as exactly r.H lines of r.W cells: a rule down
// its left edge beside a title row, the content, and the hint row.
func (m *Model) renderDrawer(d *drawer, r tui.Rect) []string {
	body := drawerBody(r)
	lines := make([]string, 0, r.H)
	lines = append(lines, drawerTitleStyle.Width(body.W).Render(tui.Fit(" "+d.title, body.W)))
	view := d.view.View()
	if t := d.transcript; d.list == nil && t.selection != nil {
		view = d.view.ViewWith(func(i int, line string) string { return t.highlight(i, line, body.W) })
	}
	for _, line := range strings.Split(view, "\n") {
		lines = append(lines, tui.Pad(line, body.W))
	}
	lines = append(lines, tui.Pad(m.hintRow(d), body.W))
	rule := drawerRuleStyle.Render("│")
	for i := range lines {
		lines[i] = rule + lines[i]
	}
	return lines
}

// hintRow renders the hint row, a question asked for a confirmation in the
// warning color.
func (m *Model) hintRow(d *drawer) string {
	style := drawerRuleStyle
	if d.armed != "" {
		style = lipgloss.NewStyle().Foreground(colorWarn)
	}
	var parts []string
	for _, h := range m.hints(d) {
		parts = append(parts, h.text)
	}
	return " " + style.Render(strings.Join(parts, " · "))
}

// dimScreen repaints every line flat in the dim color, dropping its own
// colors so no slab or highlight stands out from under a drawer.
func dimScreen(lines []string) {
	for i, line := range lines {
		lines[i] = dimStyle.Render(ansi.Strip(line))
	}
}
