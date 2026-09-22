package ui

import (
	"fmt"
	"os"
	"strings"

	tea "charm.land/bubbletea/v2"
	"charm.land/lipgloss/v2"
)

// maxInputLines caps how tall the prompt input grows, in visual rows
// (soft-wrapped rows included), before it scrolls internally.
const maxInputLines = 6

func (m *Model) resize() {
	if m.width <= 0 || m.height <= 0 {
		return
	}
	m.input.SetWidth(max(1, m.width))
	// DynamicHeight sizes the input to its visual rows, but the transcript
	// must keep at least one row, so the cap shrinks for small windows.
	inputHeight := min(maxInputLines, max(1, m.height-4-m.menu.height()), max(1, m.input.Height()))
	m.input.SetHeight(inputHeight)
	m.viewport.SetWidth(max(1, m.width))
	m.viewport.SetHeight(max(1, m.height-inputHeight-2-m.menu.height()))
}

// refreshTranscript updates the viewport contents. When toBottom is set, the
// transcript follows new output only if the user is already at the bottom;
// otherwise the scroll position is preserved so reading is not interrupted.
// The position check must run before SetContentLines: once the content grows, a
// user who was at the bottom no longer is.
func (m *Model) refreshTranscript(toBottom bool) {
	follow := toBottom && m.viewport.AtBottom()
	m.viewport.SetContentLines(m.activeTranscript().linesFor(m.width))
	if follow {
		m.viewport.GotoBottom()
	}
}

// activeTranscript is the transcript the viewport currently shows: a
// highlighted popup row's preview when one is active, otherwise the live
// conversation.
func (m *Model) activeTranscript() *transcript {
	if m.preview != nil {
		return m.preview
	}
	return &m.transcript
}

func (m Model) View() tea.View {
	barBg := lipgloss.Color("#1C1C1C")
	headerStyle := lipgloss.NewStyle().Foreground(lipgloss.Color("#C8C8C8")).Background(barBg).Width(max(1, m.width))
	statusStyle := lipgloss.NewStyle().Foreground(colorFaint).Background(barBg).Width(max(1, m.width))
	header := accentBrand(" kon")
	if m.active.Name != "" {
		header += " · " + m.active.Name
	}
	ctx := "ctx ?"
	if m.contextTokens >= 0 {
		prefix := ""
		if m.contextApprox {
			prefix = "~"
		}
		ctx = fmt.Sprintf("ctx %s%s", prefix, compactNumber(m.contextTokens))
	}
	if m.active.ContextWindow > 0 {
		ctx += "/" + compactNumber(m.active.ContextWindow)
	}
	status := " " + abbreviateHome(m.cwd) + "  ·  " + ctx + "  ·  " + m.status
	sections := []string{headerStyle.Render(fitLine(header, m.width)), m.viewport.View(), statusStyle.Render(fitLine(status, m.width))}
	if menu := m.menu.render(m.width); menu != "" {
		sections = append(sections, menu)
	}
	inputTop := 0
	for _, section := range sections {
		inputTop += strings.Count(section, "\n") + 1
	}
	sections = append(sections, inputView(m.input.View(), m.width))
	content := strings.Join(sections, "\n")
	view := tea.NewView(content)
	if m.terminalFocused {
		view.Cursor = m.input.Cursor()
		if view.Cursor != nil {
			// inputView adds one cell of left inset except when the terminal is
			// too narrow to afford it.
			if m.width > 2 {
				view.Cursor.X++
			}
			view.Cursor.Y += inputTop
		}
	}
	view.AltScreen = true
	view.MouseMode = tea.MouseModeCellMotion
	// Focus reports let kon hide the real cursor when the terminal window loses
	// focus. tmux independently hides the real cursor in inactive panes.
	view.ReportFocus = true
	view.WindowTitle = "kon"
	return view
}

// accentBrand paints the "kon" wordmark in the muted red accent.
func accentBrand(s string) string {
	return lipgloss.NewStyle().Bold(true).Foreground(colorAccent).Render(s)
}

// inputView insets the prompt one cell from each edge: the block is shifted
// right one cell and narrowed by one, and every line is re-padded so the
// textarea's full-width background still spans to the right edge.
func inputView(view string, width int) string {
	if width <= 2 {
		return view
	}
	style := lipgloss.NewStyle().Width(width - 1)
	lines := strings.Split(view, "\n")
	for i, line := range lines {
		lines[i] = " " + style.Render(line)
	}
	return strings.Join(lines, "\n")
}

func fitLine(value string, width int) string {
	if width <= 0 || lipgloss.Width(value) <= width {
		return value
	}
	if width == 1 {
		return "…"
	}
	var out strings.Builder
	used := 0
	for _, r := range value {
		runeWidth := lipgloss.Width(string(r))
		if used+runeWidth > width-1 {
			break
		}
		out.WriteRune(r)
		used += runeWidth
	}
	out.WriteRune('…')
	return out.String()
}

func compactNumber(n int) string {
	if n >= 1_000_000 {
		return fmt.Sprintf("%.1fm", float64(n)/1_000_000)
	}
	if n >= 1_000 {
		return fmt.Sprintf("%.1fk", float64(n)/1_000)
	}
	return fmt.Sprintf("%d", n)
}

func abbreviateHome(path string) string {
	if home, err := os.UserHomeDir(); err == nil && (path == home || strings.HasPrefix(path, home+string(os.PathSeparator))) {
		return "~" + strings.TrimPrefix(path, home)
	}
	return path
}

func sanitize(s string) string {
	var out strings.Builder
	for i := 0; i < len(s); {
		if s[i] == 0x1b {
			i++
			if i < len(s) && s[i] == '[' {
				i++
				for i < len(s) {
					b := s[i]
					i++
					if b >= 0x40 && b <= 0x7e {
						break
					}
				}
			}
			continue
		}
		if s[i] < 0x20 && s[i] != '\n' && s[i] != '\t' {
			i++
			continue
		}
		out.WriteByte(s[i])
		i++
	}
	return out.String()
}
