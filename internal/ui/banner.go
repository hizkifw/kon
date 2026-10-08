package ui

import (
	"image/color"
	"strings"

	"charm.land/lipgloss/v2"
)

// welcomeMessage is the assistant-style introduction shown once on a first run
// that still needs configuration. It is a normal assistant block, not a
// banner: it renders through the markdown path so it reads as kon speaking,
// and it stays out of the session store because transcript blocks are
// presentation only.
func welcomeMessage(configPath string) string {
	return "Hi, I'm kon — a coding agent for your terminal.\n\n" +
		"I'm not configured yet, so I can't answer prompts. Run `/login`\n" +
		"to connect a provider, then use `/model` to choose a model. You can also\n" +
		"edit `" + configPath + "` directly.\n\n" +
		"- Press `/` to browse commands.\n" +
		"- Run `/help` to read the user guide.\n" +
		"- Once I'm set up, ask me how kon works.\n" +
		"- Press `Ctrl+D` to exit, or `Esc` to interrupt a running turn."
}

// banner is a mark shown at the top of every transcript: a figure, then a
// caption on its last line. It is presentation only: the transcript draws it
// as a stable prefix rather than a block, so it stays out of session records
// and model context while still appearing above a resumed conversation.
type banner struct {
	text string
	// figure colors every line but the caption, which is always faint.
	figure color.Color
}

var welcomeBanner = banner{figure: colorAccent, text: `┌──┐              ┌──┐
│  ├──┬─────┬─────┤  │
│  ┌─<│  _  │     ├──┤
└──┴──┴─────┴──┴──┴──┘
harness for foxes =˄▾˄=`}

// incognitoBanner replaces the welcome banner when nothing is being saved.
// The same mark, dashed and faint, reads as an outline of a session that is
// not really there, so the difference shows at a glance.
var incognitoBanner = banner{figure: colorFaint, text: `┌╌╌┐              ┌╌╌┐
╎  ├╌╌┬╌╌╌╌╌┬╌╌╌╌╌┤  ╎
╎  ┌╌<╎  _  ╎     ├╌╌┤
└╌╌┴╌╌┴╌╌╌╌╌┴╌╌┴╌╌┴╌╌┘
incognito · session not saved =˄⌐■▾■˄=`}

// bannerText renders the banner inset one cell from the transcript's left
// edge, or "" when there is nothing to show. A single blank line pads the mark
// from the top of the viewport, the figure carries the banner's color, and the
// trailing caption is drawn faintly, so the mark reads as a header rather than
// content. The result depends only on the banner and the width, so it is
// memoized; bannerText runs on every frame.
func (t *transcript) bannerText(width int) string {
	if t.bannerValid && t.bannerWidth == width {
		return t.bannerCache
	}
	t.bannerValid, t.bannerWidth = true, width
	t.bannerCache = t.renderBanner(width)
	return t.bannerCache
}

func (t *transcript) renderBanner(width int) string {
	if t.banner.text == "" {
		return ""
	}
	lines := strings.Split(t.banner.text, "\n")
	widest := 0
	for _, line := range lines {
		if w := lipgloss.Width(line); w > widest {
			widest = w
		}
	}
	// A figure cannot wrap, so a terminal too narrow to hold it plus its left
	// padding shows nothing rather than a mangled mark.
	if widest+1 > width {
		return ""
	}
	out := make([]string, 0, len(lines)+1)
	out = append(out, "")
	for i, line := range lines {
		style := lipgloss.NewStyle().Foreground(t.banner.figure)
		if i == len(lines)-1 {
			style = style.Foreground(colorFaint)
		}
		// One cell of left padding aligns the mark with message slabs, which
		// carry the same inset.
		out = append(out, " "+style.Render(line))
	}
	return strings.Join(out, "\n")
}
