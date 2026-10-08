package ui

import (
	tea "charm.land/bubbletea/v2"
	"charm.land/lipgloss/v2"
	"kon.kitsu.red/internal/tui"
)

// drawer is a tui drawer whose actions see the model. kon opens one for a side
// answer, the job list, a job's output, and the user guide.
type drawer = tui.Drawer[*Model]

type drawerAction = tui.Action[*Model]

type drawerCrumb = tui.Crumb[*Model]

// drawerTheme colors the drawer stack in kon's palette.
func drawerTheme() tui.Theme {
	return tui.Theme{
		Title:    lipgloss.NewStyle().Foreground(colorBarFg).Background(colorBarBg),
		Rule:     lipgloss.NewStyle().Foreground(colorFaint),
		Warn:     lipgloss.NewStyle().Foreground(colorWarn),
		Dim:      lipgloss.NewStyle().Foreground(colorDim),
		Pressed:  lipgloss.NewStyle().Foreground(colorBarBg).Background(colorLink),
		Heading:  lipgloss.NewStyle().Foreground(colorFaint).Bold(true),
		Selected: selectedRowStyle,
		Faint:    colorFaint,
	}
}

// topDrawer is the drawer that takes input, or nil when none is open.
func (m *Model) topDrawer() *drawer { return m.drawers.Top() }

// openDrawer pushes d onto the stack. A press on what is under it no longer
// counts toward a double click or a drag.
func (m *Model) openDrawer(d *drawer) {
	m.drawers.Open(d)
	m.click = click{}
}

// closeDrawer pops the top drawer, dropping any selection in it.
func (m *Model) closeDrawer() {
	d := m.drawers.Top()
	if d == nil {
		return
	}
	if t, ok := d.Content.(*transcript); ok {
		t.selection = nil
	}
	m.click = click{}
	m.drawers.Pop(m)
}

// closeDrawers pops every drawer, top first.
func (m *Model) closeDrawers() {
	for m.drawers.Len() > 0 {
		m.closeDrawer()
	}
}

// drawerKey handles a key while a drawer is open. The prompt underneath is
// not reachable, so keys it would take do nothing, and plain letters are free
// for scrolling and for the drawer's actions. Esc, Ctrl+C, or a click on the
// dimmed area closes the top drawer; what is behind it keeps running and
// repainting.
func (m Model) drawerKey(key string) (tea.Model, tea.Cmd) {
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
	cmd := m.drawers.Key(&m, key)
	return m, cmd
}

// clickDrawer handles a press inside the top drawer that is not on its
// content: a hint runs its action, as its key would.
func (m Model) clickDrawer(x, y int) (tea.Model, tea.Cmd, bool) {
	key, handled := m.drawers.Click(&m, x, y)
	if !handled || key == "" {
		return m, nil, handled
	}
	updated, cmd := m.drawerKey(key)
	return updated, cmd, true
}
