package tui

import (
	"image/color"
	"strings"

	"charm.land/lipgloss/v2"
	"github.com/charmbracelet/x/ansi"
)

// Item is one row of a drawer's list.
type Item struct {
	// Value identifies the row to whoever filled the list.
	Value, Label, Description string
	// Heading marks a row that names the section below it rather than
	// something to pick; it is never highlighted.
	Heading bool
	// Badge is a short status shown between the label and the description,
	// in BadgeColor, or the text color when that is nil.
	Badge      string
	BadgeColor color.Color
}

// List is the rows a drawer offers to pick from, with one highlighted.
type List struct {
	Items []Item
	Index int
}

// Selected is the highlighted row, if there is one.
func (l *List) Selected() (Item, bool) {
	if l.Index < 0 || l.Index >= len(l.Items) {
		return Item{}, false
	}
	return l.Items[l.Index], true
}

// step moves from row i by one row in dir, past headings, staying put at
// either end.
func (l *List) step(i, dir int) int {
	for j := i + dir; j >= 0 && j < len(l.Items); j += dir {
		if !l.Items[j].Heading {
			return j
		}
	}
	return i
}

// move moves the highlight for key, a page being pageRows rows. It reports
// whether key moves a list.
func (l *List) move(key string, pageRows int) bool {
	index := l.Index
	switch key {
	case "up", "k":
		index = l.step(index, -1)
	case "down", "j":
		index = l.step(index, 1)
	case "pgup":
		for range pageRows {
			index = l.step(index, -1)
		}
	case "pgdown":
		for range pageRows {
			index = l.step(index, 1)
		}
	case "home":
		index = l.step(-1, 1)
	case "end":
		index = l.step(len(l.Items), -1)
	default:
		return false
	}
	if index >= 0 && index < len(l.Items) {
		l.Index = index
	}
	return true
}

// rows renders every row at width.
func (l *List) rows(width int, theme Theme) []string {
	lines := make([]string, len(l.Items))
	for i, item := range l.Items {
		lines[i] = listRow(item, i == l.Index, width, theme)
	}
	return lines
}

// listRow renders one row at width. Each part is fitted before it is styled,
// and a highlighted row carries its background through every part, so a
// part's colors cannot punch a hole in the highlight.
func listRow(item Item, selected bool, width int, theme Theme) string {
	if item.Heading {
		return theme.Heading.Render(Fit(" "+item.Label, width))
	}
	base := lipgloss.NewStyle()
	if selected {
		base = theme.Selected
	}
	var out strings.Builder
	used := 0
	add := func(text string, fg color.Color) {
		if text == "" || used >= width {
			return
		}
		text = Fit(text, width-used)
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
		add(item.Description, theme.Faint)
	}
	if selected && used < width {
		out.WriteString(base.Render(strings.Repeat(" ", width-used)))
	}
	return out.String()
}
