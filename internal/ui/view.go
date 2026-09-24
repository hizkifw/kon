package ui

import (
	"image/color"
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
	if m.login != nil {
		m.login.input.SetWidth(max(1, m.width-2))
	}
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
	headerStyle := lipgloss.NewStyle().Foreground(colorBarFg).Background(colorBarBg).Width(max(1, m.width))
	labelStyle := lipgloss.NewStyle().Foreground(colorBarFg).Background(colorBarBg)
	statusStyle := lipgloss.NewStyle().Foreground(colorFaint).Background(colorBarBg).Width(max(1, m.width))
	label := ""
	if m.active.Name != "" {
		name := m.active.DisplayName
		if name == "" {
			name = m.active.Name
		}
		if m.active.ConnectionID != "" {
			name = m.active.ConnectionID + " · " + name
		}
		label = " · " + name
		if len(m.active.ReasoningEfforts) > 0 {
			label += " · " + effortLabel(m.active.ReasoningEffort)
		}
	}
	// The brand and the label are painted as separate spans: the brand's style
	// reset would otherwise clear the bar background for the rest of the line.
	header := accentBrand(" kon", colorBarBg) + labelStyle.Render(label)
	ctx := "ctx ?"
	if m.contextTokens >= 0 {
		prefix := ""
		if m.contextApprox {
			prefix = "~"
		}
		ctx = "ctx " + prefix + m.contextTokens.String()
	}
	if m.active.ContextWindow > 0 {
		ctx += "/" + m.active.ContextWindow.String()
	}
	status := " " + abbreviateHome(m.cwd) + " · " + ctx + " · " + m.status
	sections := []string{headerStyle.Render(fitLine(header, m.width)), m.viewport.View(), statusStyle.Render(fitLine(status, m.width))}
	if menu := m.menu.render(m.width); menu != "" {
		sections = append(sections, menu)
	}
	inputTop := 0
	for _, section := range sections {
		inputTop += strings.Count(section, "\n") + 1
	}
	input := m.input.View()
	if m.login != nil {
		input = m.login.input.View()
	}
	sections = append(sections, inputView(input, m.width))
	content := strings.Join(sections, "\n")
	view := tea.NewView(content)
	if m.terminalFocused {
		view.Cursor = m.input.Cursor()
		if m.login != nil {
			view.Cursor = m.login.input.Cursor()
		}
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

// effortLabel names a reasoning effort for display. The empty level is the
// provider's default, and "none" is spelled out so turning reasoning off reads
// as a choice rather than a missing value.
func effortLabel(effort string) string {
	switch effort {
	case "":
		return "default"
	case "none":
		return "no thinking"
	}
	return effort
}

// accentBrand paints the "kon" wordmark in the muted red accent over bg. The
// background must be set here: rendering the brand inside an already-styled bar
// emits a reset that would otherwise clear the bar for the rest of the line.
func accentBrand(s string, bg color.Color) string {
	return lipgloss.NewStyle().Bold(true).Foreground(colorAccent).Background(bg).Render(s)
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
