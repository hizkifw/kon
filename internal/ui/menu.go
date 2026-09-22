package ui

import (
	"strings"

	"charm.land/lipgloss/v2"
)

// maxMenuRows caps how many popup rows are shown at once.
const maxMenuRows = 8

// menuItem is one selectable row in a popup.
type menuItem struct {
	Value       string
	Description string
}

// menu is a generic selectable popup rendered above the prompt. It owns only
// presentation and navigation; committing a choice is the job of the
// menuSource that produced it. Slash-command completion is one source; file
// mentions, history pickers, or any other list can reuse the same widget
// without touching the update loop.
type menu struct {
	items []menuItem
	index int
}

func (m menu) open() bool { return len(m.items) > 0 }

func (m *menu) close() {
	m.items = nil
	m.index = 0
}

func (m menu) selected() menuItem {
	if !m.open() {
		return menuItem{}
	}
	return m.items[m.index]
}

// move advances the highlighted row by delta, wrapping at both ends.
func (m *menu) move(delta int) {
	if !m.open() {
		return
	}
	m.index = (m.index + delta + len(m.items)) % len(m.items)
}

// height is the number of terminal rows the popup occupies, so the viewport
// can shrink to make room for it. It never exceeds maxMenuRows.
func (m menu) height() int {
	if !m.open() {
		return 0
	}
	return min(len(m.items), maxMenuRows)
}

// render draws the popup, highlighting the selected row. An empty string means
// there is nothing to show.
func (m menu) render(width int) string {
	if !m.open() {
		return ""
	}
	selected := lipgloss.NewStyle().Foreground(lipgloss.Color("#DADADA")).Background(lipgloss.Color("#333333"))
	description := lipgloss.NewStyle().Foreground(colorFaint)
	// Keep the selected row visible when the list is longer than the window.
	start := 0
	if m.index >= maxMenuRows {
		start = m.index - maxMenuRows + 1
	}
	end := min(len(m.items), start+maxMenuRows)
	lines := make([]string, 0, end-start)
	for i := start; i < end; i++ {
		item := m.items[i]
		line := " " + item.Value
		if item.Description != "" {
			line += "  " + description.Render(item.Description)
		}
		if i == m.index {
			line = selected.Render(fitLine(line, width))
		}
		lines = append(lines, fitLine(line, width))
	}
	return strings.Join(lines, "\n")
}

// menuSource fills a popup for the current input and commits the chosen value.
// The model drives every popup through this one interface, so a new popup only
// needs to supply rows and say how a selection is applied.
type menuSource interface {
	// Candidates returns the rows to show for the current input. An empty
	// slice closes the popup.
	Candidates(m Model, input string) []menuItem
	// Accept commits value by updating the model's input.
	Accept(m *Model, value string)
}
