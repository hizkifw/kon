package ui

import (
	"fmt"
	"image/color"
	"os"
	"strings"

	tea "charm.land/bubbletea/v2"
	"charm.land/lipgloss/v2"

	"github.com/hizkifw/kon/internal/app"
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
	m.menu.rows = max(1, m.height-4-m.pendingHeight())
	panels := m.menu.height() + m.pendingHeight()
	inputHeight := min(maxInputLines, max(1, m.height-4-panels), max(1, m.input.Height()))
	m.input.SetHeight(inputHeight)
	m.viewport.SetWidth(max(1, m.width))
	m.viewport.SetHeight(max(1, m.height-inputHeight-2-panels))
	m.layoutDrawers()
}

// refreshTranscript updates the viewport contents. When toBottom is set, the
// transcript follows new output only if the user is already at the bottom;
// otherwise the scroll position is preserved so reading is not interrupted.
// The position check must run before SetContentLines: once the content grows, a
// user who was at the bottom no longer is.
func (m *Model) refreshTranscript(toBottom bool) {
	follow := toBottom && m.viewport.AtBottom()
	m.viewport.SetContentLines(m.mainTranscript().linesFor(m.width))
	if follow {
		m.viewport.GotoBottom()
	}
	m.refreshDrawers()
}

// activeTranscript is the transcript the mouse works on: the top drawer's when
// one is open, which is nil for a list, otherwise the one the main viewport
// shows.
func (m *Model) activeTranscript() *transcript {
	if d := m.topDrawer(); d != nil {
		return d.transcript
	}
	return m.mainTranscript()
}

// mainTranscript is the transcript the main viewport shows: a highlighted
// popup row's preview when one is active, otherwise the live conversation.
func (m *Model) mainTranscript() *transcript {
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
		label = " · " + modelTitle(m.active)
	}
	// The brand and the label are painted as separate spans: the brand's style
	// reset would otherwise clear the bar background for the rest of the line.
	header := accentBrand(" kon", colorBarBg) + labelStyle.Render(label)
	ctx := "ctx ?"
	if m.contextTokens >= 0 {
		prefix := ""
		if m.contextApprox || m.streamedContext > 0 {
			prefix = "~"
		}
		ctx = "ctx " + prefix + (m.contextTokens + m.streamedContext).String()
	}
	if m.active.ContextWindow > 0 {
		ctx += "/" + m.active.ContextWindow.String()
	}
	status := " " + abbreviateHome(m.cwd) + " · " + ctx
	if m.jobs > 0 {
		status += fmt.Sprintf(" · ⚙ %d", m.jobs)
	}
	streaming := float64(m.streamed) * m.active.OutputPrice / 1e6
	if spent := m.spent + m.subagentSpent + m.sideSpent + streaming; spent > 0 {
		status += " · " + formatCost(spent)
	}
	// The transcript shows what a turn is doing, so the status line carries
	// only the mode kon is in and messages that answer the user, such as a
	// command's result.
	if text := m.statusText(); text != "" {
		status += " · " + text
	}
	line := statusStyle.Render(fitLine(status, m.width))
	if t := m.messageTone(); t != toneInfo && m.search == nil {
		line = statusStyle.Render(toneLine(fitLine(status, m.width), len(status)-len(oneLine(m.message)), t))
	}
	transcript := m.viewport.View()
	if t := m.mainTranscript(); t.selection != nil {
		transcript = m.viewport.ViewWith(func(i int, line string) string { return t.highlight(i, line, m.width) })
	}
	sections := []string{headerStyle.Render(fitLine(header, m.width)), transcript}
	if pending := m.pendingView(); pending != "" {
		sections = append(sections, pending)
	}
	sections = append(sections, line)
	if menu := m.menu.render(m.width); menu != "" {
		sections = append(sections, menu)
	}
	inputTop := 0
	for _, section := range sections {
		inputTop += strings.Count(section, "\n") + 1
	}
	// View works on a copy, so the placeholder can follow the run state
	// without every transition having to remember to update it.
	m.input.Placeholder = m.placeholder()
	input := m.input.View()
	if m.login != nil {
		input = m.login.input.View()
	}
	sections = append(sections, inputView(input, m.width))
	content := strings.Join(sections, "\n")
	if len(m.drawers) > 0 {
		content = strings.Join(m.paintDrawers(strings.Split(content, "\n")), "\n")
	}
	view := tea.NewView(content)
	if m.terminalFocused && len(m.drawers) == 0 {
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

// modelTitle names a model the way the header shows it: its connection, its
// display name, and the selected reasoning effort when the model has levels.
// It deliberately leaves out the external ID, which is long and provider
// specific; that belongs in the model picker.
func modelTitle(active app.Model) string {
	name := active.DisplayName
	if name == "" {
		name = active.Name
	}
	if active.ConnectionID != "" {
		name = active.ConnectionID + " · " + name
	}
	if len(active.ReasoningEfforts) > 0 {
		name += " · " + effortLabel(active.ReasoningEffort)
	}
	return name
}

// modelChangedText is the transcript line for a model switch, live or replayed.
func modelChangedText(model app.Model) string { return " Model changed to " + modelTitle(model) }

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

// toneLine colors a fitted status line from byte offset at, where the
// message starts, to its end. The two spans are painted separately with the
// bar background, since a span's style reset would otherwise clear it for the
// rest of the line.
func toneLine(line string, at int, t tone) string {
	if at >= len(line) {
		// Truncation cut the message off entirely.
		return line
	}
	faint := lipgloss.NewStyle().Foreground(colorFaint).Background(colorBarBg)
	toned := lipgloss.NewStyle().Foreground(t.color()).Background(colorBarBg)
	return faint.Render(line[:at]) + toned.Render(line[at:])
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

// sanitize removes escape sequences and control characters other than newline
// and tab from text bound for the screen. It shares ansiStripper's state
// machine so an OSC (a window title, a hyperlink) is dropped whole rather than
// leaving its payload behind as text once the ESC is gone.
func sanitize(s string) string {
	var strip ansiStripper
	return dropC1(strings.ReplaceAll(strip.strip(s), "\r", ""))
}

// dropC1 removes C1 control characters (U+0080-U+009F), which some terminals
// act on like the ESC sequences they abbreviate: U+009B is CSI. In UTF-8 each
// is 0xC2 followed by 0x80-0x9F, and 0xC2 is only ever a lead byte, so a byte
// scan cannot split another character.
func dropC1(s string) string {
	if strings.IndexByte(s, 0xc2) < 0 {
		return s
	}
	var out strings.Builder
	out.Grow(len(s))
	for i := 0; i < len(s); i++ {
		if s[i] == 0xc2 && i+1 < len(s) && s[i+1] >= 0x80 && s[i+1] <= 0x9f {
			i++
			continue
		}
		out.WriteByte(s[i])
	}
	return out.String()
}
