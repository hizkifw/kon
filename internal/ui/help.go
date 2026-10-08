package ui

import (
	"sort"
	"strconv"
	"strings"
	"unicode"

	tea "charm.land/bubbletea/v2"
	"github.com/charmbracelet/x/ansi"
	productdocs "kon.kitsu.red/docs/product"
	"kon.kitsu.red/internal/markdown"
)

// helpIndex is the page /help opens on, which links to every other.
const helpIndex = "index.md"

// help is the /help drawer: one page of the bundled user guide at a time.
// A link to another page or to a heading is followed in place, so the guide
// reads in one drawer instead of a stack of them.
type help struct {
	drawer *drawer
	page   string
	// back is the places left by following links, latest last.
	back []helpPlace
	// focus is the link Tab stopped on, as an index into links, or -1.
	focus int

	// The page as laid out at width. text holds its lines, which are nil
	// when it must be laid out again, after the page or the focus changed.
	// They are kept in a transcript, which is what the mouse selects in, so
	// a page is selected and copied as a reply is.
	width    int
	text     transcript
	links    []helpLink
	headings []helpHeading
}

// helpMargin is the blank lines a page is shown under.
const helpMargin = 1

// helpPlace is somewhere in the guide to come back to.
type helpPlace struct {
	page          string
	offset, focus int
}

// helpLink is a link the drawer follows itself, and the cells that show it.
type helpLink struct {
	dest string
	runs []helpRun
}

// helpRun is the cells from column from up to column to of a line.
type helpRun struct{ line, from, to int }

// helpHeading is a heading of the page: the line it starts on and the anchor
// a link names it by.
type helpHeading struct {
	line         int
	text, anchor string
}

// helpTitle is a page's name in the trail: its first heading.
func helpTitle(page string) string {
	text, _ := productdocs.Page(page)
	first, _, _ := strings.Cut(text, "\n")
	return strings.TrimSpace(strings.TrimLeft(first, "#"))
}

// helpTarget resolves a link on page to the bundled page and the heading it
// names. ok is false for any other link, such as a web address.
func helpTarget(page, dest string) (target, anchor string, ok bool) {
	target, anchor, _ = strings.Cut(dest, "#")
	if target == "" {
		if anchor == "" {
			return "", "", false
		}
		target = page
	}
	_, ok = productdocs.Page(target)
	return target, anchor, ok
}

// helpAnchor is the anchor a heading gets where the guide is published:
// lowercase, spaces as hyphens, and punctuation dropped.
func helpAnchor(heading string) string {
	var b strings.Builder
	for _, r := range strings.ToLower(heading) {
		switch {
		case unicode.IsLetter(r), unicode.IsDigit(r), r == '-', r == '_':
			b.WriteRune(r)
		case r == ' ':
			b.WriteByte('-')
		}
	}
	return b.String()
}

// openHelp runs /help: it opens the guide in a drawer, on its index.
func (m Model) openHelp() (tea.Model, tea.Cmd) {
	m.message = ""
	m.input.Reset()
	m.resetMenu()
	h := &help{page: helpIndex, focus: -1}
	h.drawer = &drawer{Title: "help", Trail: h.trail, Content: h, Actions: h.actions}
	m.openDrawer(h.drawer)
	// A drawer opens at its latest line, and a page reads from its first.
	h.drawer.View().SetYOffset(0)
	return m, nil
}

// helpTheme renders a page with the destinations of the links the drawer
// follows itself left out.
func helpTheme(page string) markdown.Theme {
	return markdown.Theme{HideURL: func(dest string) bool {
		_, _, ok := helpTarget(page, dest)
		return ok
	}}
}

// Highlight marks the selection, or is nil when there is none.
func (h *help) Highlight(width int) func(i int, line string) string {
	return h.text.Highlight(width)
}

// selectedParts is the cells from start to end as one part to copy: the
// Markdown of the page behind them.
func (h *help) selectedParts(start, end point, width int) []selectedPart {
	page, _ := productdocs.Page(h.page)
	part := selectedPart{reply: page, theme: helpTheme(h.page), width: width, shown: h.text.shownText(start, end)}
	// The page's own lines start under the margin.
	if end.line < helpMargin {
		return nil
	}
	if start.line < helpMargin {
		start = point{helpMargin, 0}
	}
	part.from, part.to = point{start.line - helpMargin, start.col}, point{end.line - helpMargin, end.col}
	return []selectedPart{part}
}

// Lines is the page at width, for the drawer.
func (h *help) Lines(width int) []string {
	if width != h.width {
		// A selection is anchored in the lines as wrapped before.
		h.text.selection = nil
	}
	if h.text.lines == nil || width != h.width {
		h.layout(width)
	}
	return h.text.lines
}

// layout renders the page at width the way a reply is rendered, and notes
// where its headings and its links to the guide came out.
func (h *help) layout(width int) {
	text, _ := productdocs.Page(h.page)
	rendered := markdown.Render(text, helpTheme(h.page), markdownContentWidth(width))
	h.width, h.links, h.headings = width, nil, nil
	// The page starts a line below the trail, with the gap part of the page:
	// scrolling takes it away, and a heading gone to sits right under the
	// trail. The lines are new ones, since the scroll container still reads
	// the last layout's.
	h.text.lines = make([]string, helpMargin, helpMargin+len(rendered))
	wasHeading := false
	for i, line := range rendered {
		i += helpMargin
		isHeading := len(line.Spans) > 0 && line.Spans[0].Text == "" && line.Spans[0].Style == markdown.StyleHeading
		switch {
		case isHeading && wasHeading:
			// A heading too long for one line wrapped onto this one.
			last := &h.headings[len(h.headings)-1]
			last.text += " " + line.Text
		case isHeading:
			h.headings = append(h.headings, helpHeading{line: i, text: line.Text})
		}
		wasHeading = isHeading

		segments := markdownSegments(line, markdownPalette, colorAgentFg)
		col := 1 // the slab's left padding cell
		for j := range segments {
			segment := &segments[j]
			w := ansi.StringWidth(segment.text)
			if _, _, ok := helpTarget(h.page, segment.link); ok {
				if h.addRun(segment.link, helpRun{line: i, from: col, to: col + w}) == h.focus {
					segment.fg, segment.bg = colorBarBg, colorLink
				}
				// A page of the guide is nothing a terminal could open.
				segment.link = ""
			}
			col += w
		}
		h.text.lines = append(h.text.lines, slabLineContinuous(colorAgentBg, width, segments...))
	}
	// Links are told apart by where they wrap, so a focus kept from another
	// width can count past the links there are at this one.
	if h.focus >= len(h.links) {
		h.focus = -1
	}
	seen := map[string]int{}
	for i := range h.headings {
		anchor := helpAnchor(h.headings[i].text)
		// A repeated heading is told apart by a count, as where the guide is
		// published.
		if n := seen[anchor]; n > 0 {
			h.headings[i].anchor = anchor + "-" + strconv.Itoa(n)
		} else {
			h.headings[i].anchor = anchor
		}
		seen[anchor]++
	}
}

// addRun records cells showing a link to dest and returns the link's index.
// The cells join the link before them when they lead to the same place from
// the same line or the next: that is one link styled in parts or wrapped.
func (h *help) addRun(dest string, run helpRun) int {
	if n := len(h.links); n > 0 {
		last := &h.links[n-1]
		if end := last.runs[len(last.runs)-1]; last.dest == dest && run.line-end.line <= 1 {
			last.runs = append(last.runs, run)
			return n - 1
		}
	}
	h.links = append(h.links, helpLink{dest: dest, runs: []helpRun{run}})
	return len(h.links) - 1
}

// turnTo shows the page at p.
func (h *help) turnTo(p helpPlace) {
	h.page, h.focus, h.text.lines, h.text.selection = p.page, p.focus, nil, nil
	view := h.drawer.View()
	view.SetContentLines(h.Lines(view.Width()))
	view.SetYOffset(p.offset)
}

// visit goes to the heading anchor names on page, or to the top of page when
// it names none, remembering the place left.
func (h *help) visit(page, anchor string) {
	view := h.drawer.View()
	h.back = append(h.back, helpPlace{page: h.page, offset: view.YOffset(), focus: h.focus})
	h.turnTo(helpPlace{page: page, focus: -1})
	for _, heading := range h.headings {
		if heading.anchor == anchor {
			view.SetYOffset(heading.line)
		}
	}
}

// follow goes where link index leads.
func (h *help) follow(index int) {
	if page, anchor, ok := helpTarget(h.page, h.links[index].dest); ok {
		h.visit(page, anchor)
	}
}

// goBack returns to the place the last link was followed from.
func (h *help) goBack() {
	p := h.back[len(h.back)-1]
	h.back = h.back[:len(h.back)-1]
	h.turnTo(p)
}

// linkAt is the link shown at p, or -1.
func (h *help) linkAt(p point) int {
	for i, link := range h.links {
		for _, run := range link.runs {
			if run.line == p.line && p.col >= run.from && p.col < run.to {
				return i
			}
		}
	}
	return -1
}

// setFocus moves the focus to link index, or drops it for -1. The lines
// keep their places, so a selection in them holds.
func (h *help) setFocus(index int) {
	if index == h.focus {
		return
	}
	h.focus, h.text.lines = index, nil
	view := h.drawer.View()
	view.SetContentLines(h.Lines(view.Width()))
}

// The mouse works on a page as it does in a web browser. A press on a link
// shows it in focus while the button is held, and the release follows it,
// unless the pointer was dragged in between, which selects text instead. A
// press anywhere else drops the focus Tab left.

// mouseDown takes a press at p, the first of its run of clicks when single.
func (h *help) mouseDown(p point, single bool) {
	if single {
		h.setFocus(h.linkAt(p))
		return
	}
	h.setFocus(-1)
}

// mouseUp takes the release of a single press at p that selected nothing.
func (h *help) mouseUp(p point) {
	h.setFocus(-1)
	if link := h.linkAt(p); link >= 0 {
		h.follow(link)
	}
}

// step moves the focus one link on in dir, around the ends of the page, and
// scrolls the link into view. Moving from a link scrolled out of sight would
// jump away from what is being read, so then the focus starts over from the
// links in view.
func (h *help) step(dir int) {
	n := len(h.links)
	if n == 0 {
		return
	}
	view := h.drawer.View()
	top, bottom := view.YOffset(), view.YOffset()+view.Height()-1
	line := func(i int) int { return h.links[i].runs[0].line }
	next := 0
	switch {
	case h.focus >= 0 && line(h.focus) >= top && line(h.focus) <= bottom:
		next = (h.focus + dir + n) % n
	case dir > 0:
		next = sort.Search(n, func(i int) bool { return line(i) >= top }) % n
	default:
		next = (sort.Search(n, func(i int) bool { return line(i) > bottom }) - 1 + n) % n
	}
	h.setFocus(next)
	if at := line(h.focus); at < top || at > bottom {
		view.SetYOffset(at - view.Height()/2)
	}
}

// actions is what the drawer does beyond scrolling: moving between the
// page's links, following the one in focus, and going back.
func (h *help) actions(*Model) []drawerAction {
	var actions []drawerAction
	if len(h.links) > 0 {
		actions = append(actions,
			drawerAction{Key: "tab", Hint: "⇥", Label: "next link", Run: func(*Model) tea.Cmd {
				h.step(1)
				return nil
			}},
			drawerAction{Key: "shift+tab", Hint: "⇧⇥", Label: "previous link", Run: func(*Model) tea.Cmd {
				h.step(-1)
				return nil
			}},
		)
	}
	if h.focus >= 0 {
		actions = append(actions, drawerAction{Key: "enter", Hint: "⏎", Label: "follow", Run: func(*Model) tea.Cmd {
			h.follow(h.focus)
			return nil
		}})
	}
	if len(h.back) > 0 {
		actions = append(actions, drawerAction{Key: "backspace", Hint: "⌫", Label: "back", Run: func(*Model) tea.Cmd {
			h.goBack()
			return nil
		}})
	}
	return actions
}

// trail is the way to what the drawer shows: the guide, the page unless it
// is the index, and the section scrolled to. A click on the guide or the
// page goes to its top.
func (h *help) trail(*Model) []drawerCrumb {
	top := func(*Model) { h.drawer.View().SetYOffset(0) }
	if h.page == helpIndex {
		return h.withSection(drawerCrumb{Label: helpTitle(helpIndex), Go: top})
	}
	return h.withSection(
		drawerCrumb{Label: helpTitle(helpIndex), Go: func(*Model) { h.visit(helpIndex, "") }},
		drawerCrumb{Label: helpTitle(h.page), Go: top},
	)
}

// withSection adds to crumbs the section at the top of the view: the last
// heading at or above it, the page's own title aside.
func (h *help) withSection(crumbs ...drawerCrumb) []drawerCrumb {
	top := h.drawer.View().YOffset()
	section := ""
	for i, heading := range h.headings {
		if i > 0 && heading.line <= top {
			section = heading.text
		}
	}
	if section != "" {
		crumbs = append(crumbs, drawerCrumb{Label: section})
	}
	return crumbs
}
