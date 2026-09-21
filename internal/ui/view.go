package ui

import (
	"fmt"
	"os"
	"strings"

	tea "charm.land/bubbletea/v2"
	"charm.land/lipgloss/v2"
)

func (m *Model) resize() {
	if m.width <= 0 || m.height <= 0 {
		return
	}
	inputHeight := min(6, max(1, strings.Count(m.input.Value(), "\n")+1))
	m.input.SetWidth(max(1, m.width))
	m.input.SetHeight(inputHeight)
	m.viewport.SetWidth(max(1, m.width))
	m.viewport.SetHeight(max(1, m.height-inputHeight-2))
}

// refreshTranscript updates the viewport contents. When toBottom is set, the
// transcript follows new output only if the user is already at the bottom;
// otherwise the scroll position is preserved so reading is not interrupted.
// The position check must run before SetContent: once the content grows, a
// user who was at the bottom no longer is.
func (m *Model) refreshTranscript(toBottom bool) {
	follow := toBottom && m.viewport.AtBottom()
	m.viewport.SetContent(m.transcript.render(m.width))
	if follow {
		m.viewport.GotoBottom()
	}
}

func (m Model) View() tea.View {
	headerStyle := lipgloss.NewStyle().Bold(true).Foreground(lipgloss.Color("15")).Background(lipgloss.Color("4")).Width(max(1, m.width))
	statusStyle := lipgloss.NewStyle().Foreground(lipgloss.Color("0")).Background(lipgloss.Color("7")).Width(max(1, m.width))
	header := " kon"
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
	content := strings.Join([]string{headerStyle.Render(fitLine(header, m.width)), m.viewport.View(), statusStyle.Render(fitLine(status, m.width)), m.input.View()}, "\n")
	view := tea.NewView(content)
	view.AltScreen = true
	view.MouseMode = tea.MouseModeCellMotion
	view.WindowTitle = "kon"
	return view
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
