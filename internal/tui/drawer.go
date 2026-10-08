package tui

import (
	"image/color"
	"strings"

	tea "charm.land/bubbletea/v2"
	"charm.land/lipgloss/v2"
	"github.com/charmbracelet/x/ansi"
)

// A Drawer is a surface painted over the whole screen, showing content of its
// own, such as a side answer or a job's output, or a list to pick from.
// Drawers stack. Only the top one takes keys and the mouse, and everything
// under it is dimmed, so there is never more than one surface to scroll or
// select in.
//
// H is the program's state as its actions see it, passed in when an action
// runs rather than captured when the drawer opens, since a Bubble Tea model
// is copied on every update.
type Drawer[H any] struct {
	Title string
	// Trail, when set, heads the drawer in place of Title with the way to
	// what it shows now, such as a page and the section scrolled to.
	Trail func(H) []Crumb[H]
	// Content is what the drawer shows, unless List is set: then it shows
	// the list's rows, one per line, with the highlighted one marked.
	Content Content
	List    *List
	// Actions returns what the drawer does beyond scrolling and closing, as
	// things stand now, so an action that no longer applies is not offered.
	// Nil means none.
	Actions func(H) []Action[H]
	// OnClose releases what the drawer's owner holds, such as a request
	// still streaming into it. It runs after the drawer is off the stack.
	OnClose func(H)

	// armed is the key of an action pressed once that asks for a second
	// press; any other key disarms it.
	armed string
	// pressed is the crumb the mouse button is held on, counted from 1, or
	// 0 for none.
	pressed int
	view    Scroll
}

// Content is what a drawer shows when it is not a list: lines wrapped to a
// width.
type Content interface {
	Lines(width int) []string
}

// Highlighter is Content that can mark part of what it shows, such as a
// selection. Highlight returns how to decorate each shown line, or nil when
// nothing is marked.
type Highlighter interface {
	Highlight(width int) func(i int, line string) string
}

// Action is a key a drawer answers to. The drawer's hint row names each one,
// and a click on its name runs it as the key would.
type Action[H any] struct {
	// Key is the key as tea.KeyPressMsg.String reports it, and Hint how the
	// hint row writes it: "⇧K" for "K", so the Shift is plain to see.
	Key, Hint, Label string
	// Confirm, when set, is what the hint row asks after a first press;
	// only a second press runs the action. A destructive action has one.
	Confirm string
	Run     func(H) tea.Cmd
}

// Crumb is one step of a drawer's trail.
type Crumb[H any] struct {
	Label string
	// Go, when set, is where a click on the crumb leads. The crumb shows as
	// pressed while the button is held, and the release on it goes there.
	Go func(H)
}

// crumbSeparator sits between the steps of a trail.
const crumbSeparator = " › "

// View is the drawer's scroll container.
func (d *Drawer[H]) View() *Scroll { return &d.view }

// Theme colors a drawer stack.
type Theme struct {
	Title, Rule, Warn, Dim lipgloss.Style
	// Pressed styles a crumb while the mouse button is held on it.
	Pressed lipgloss.Style
	// Heading and Selected style a list's section headings and its
	// highlighted row, and Faint its descriptions.
	Heading, Selected lipgloss.Style
	Faint             color.Color
}

// Stack is the drawers open over a screen, top last. The zero value is an
// empty stack; Resize gives it the screen's size.
type Stack[H any] struct {
	Theme         Theme
	drawers       []*Drawer[H]
	width, height int
}

// Drawers open from the right at full height. Each covers nine tenths of the
// width the one under it covers, so every level of the stack leaves a strip of
// the level below showing.
const drawerShareNum, drawerShareDen = 9, 10

// Len is how many drawers are open.
func (s *Stack[H]) Len() int { return len(s.drawers) }

// Top is the drawer that takes input, or nil when none is open.
func (s *Stack[H]) Top() *Drawer[H] {
	if len(s.drawers) == 0 {
		return nil
	}
	return s.drawers[len(s.drawers)-1]
}

// Open pushes d onto the stack. A list starts at its top, anything else at
// its latest line.
func (s *Stack[H]) Open(d *Drawer[H]) {
	d.view = NewScroll()
	s.drawers = append(s.drawers, d)
	s.layout()
	if d.List == nil {
		d.view.GotoBottom()
	}
}

// Pop removes the top drawer, then runs its OnClose with h.
func (s *Stack[H]) Pop(h H) {
	d := s.Top()
	if d == nil {
		return
	}
	s.drawers = s.drawers[:len(s.drawers)-1]
	if d.OnClose != nil {
		d.OnClose(h)
	}
}

// Resize fits every drawer to a screen of width by height cells.
func (s *Stack[H]) Resize(width, height int) {
	s.width, s.height = width, height
	s.layout()
}

// layout sizes each drawer to its place in the stack after the screen or the
// stack changed.
func (s *Stack[H]) layout() {
	for i, d := range s.drawers {
		body := s.Body(i)
		d.view.SetWidth(body.W)
		d.view.SetHeight(body.H)
		d.view.SetContentLines(d.lines(body.W, s.Theme))
	}
}

// Refresh repaints each drawer, following new output in content scrolled to
// its end.
func (s *Stack[H]) Refresh() {
	for _, d := range s.drawers {
		follow := d.List == nil && d.view.AtBottom()
		d.view.SetContentLines(d.lines(d.view.Width(), s.Theme))
		if follow {
			d.view.GotoBottom()
		}
	}
}

// Rect is the screen area of the drawer at level in the stack, counted from
// the bottom.
func (s *Stack[H]) Rect(level int) Rect {
	r := Rect{W: s.width, H: s.height}
	for range level + 1 {
		w := max(2, r.W*drawerShareNum/drawerShareDen)
		r.X, r.W = r.X+r.W-w, w
	}
	return r
}

// Body is the part of the drawer at level its content scrolls in: between the
// title row and the hint row, and right of the rule that edges the drawer.
func (s *Stack[H]) Body(level int) Rect {
	r := s.Rect(level)
	return Rect{X: r.X + 1, Y: r.Y + 1, W: max(1, r.W-1), H: max(1, r.H-2)}
}

// Key handles a key for the top drawer: one of its actions, which runs with
// h, or else moving its list or scrolling it. Closing the drawer is the
// caller's, since what else closes with it is the caller's too.
func (s *Stack[H]) Key(h H, key string) tea.Cmd {
	d := s.Top()
	if d == nil {
		return nil
	}
	armed := d.armed
	d.armed = ""
	// A key can change the trail, and the crumb held would no longer be the
	// one pressed.
	d.pressed = 0
	for _, action := range d.actions(h) {
		if action.Key == key {
			if action.Confirm != "" && armed != key {
				d.armed = key
				return nil
			}
			cmd := action.Run(h)
			s.Refresh()
			return cmd
		}
	}
	if d.List != nil {
		if d.List.move(key, d.view.Height()) {
			d.showSelected(s.Theme)
		}
		return nil
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
	return nil
}

// Click handles a press inside the top drawer that is not on its content. A
// crumb of its trail is held until the release, a hint presses its key, and a
// list row is highlighted, or pressed with "enter" when it already was. It
// returns the key to press, if any, and whether the press was the drawer's.
func (s *Stack[H]) Click(h H, x, y int) (key string, handled bool) {
	d := s.Top()
	if d == nil {
		return "", false
	}
	body := s.Body(len(s.drawers) - 1)
	switch {
	case y == body.Y-1 && d.Trail != nil:
		d.pressed = crumbAt(d.Trail(h), x-body.X) + 1
		return "", true
	case y == body.Y+body.H:
		for _, hint := range s.hints(h, d) {
			if x >= body.X+hint.X && x < body.X+hint.X+hint.W {
				return hint.Key, true
			}
		}
		return "", true
	case d.List != nil && body.Contains(x, y):
		row := d.view.YOffset() + y - body.Y
		if row >= len(d.List.Items) || d.List.Items[row].Heading {
			return "", true
		}
		if row != d.List.Index {
			d.List.Index = row
			d.armed = ""
			d.showSelected(s.Theme)
			return "", true
		}
		return "enter", true
	}
	return "", false
}

// Release handles the mouse button let go at (x, y). The crumb it was pressed
// on goes where it leads if the pointer is still on it, as a link does in a
// web browser.
func (s *Stack[H]) Release(h H, x, y int) {
	d := s.Top()
	if d == nil || d.pressed == 0 {
		return
	}
	pressed := d.pressed - 1
	d.pressed = 0
	body := s.Body(len(s.drawers) - 1)
	if trail := d.Trail(h); y == body.Y-1 && crumbAt(trail, x-body.X) == pressed {
		trail[pressed].Go(h)
		s.Refresh()
	}
}

// crumbCells is the cells of the title row, from its left, that show crumb
// i of trail: from the first up to, not including, the last.
func crumbCells[H any](trail []Crumb[H], i int) (from, to int) {
	from = 1 // the title row leads its text with one blank cell
	for _, crumb := range trail[:i] {
		from += ansi.StringWidth(crumb.Label) + ansi.StringWidth(crumbSeparator)
	}
	return from, from + ansi.StringWidth(trail[i].Label)
}

// crumbAt is the crumb with somewhere to go shown at cell x of the title
// row, or -1.
func crumbAt[H any](trail []Crumb[H], x int) int {
	for i, crumb := range trail {
		if from, to := crumbCells(trail, i); x >= from && x < to && crumb.Go != nil {
			return i
		}
	}
	return -1
}

// Hint is one entry of the top drawer's hint row: its text, the key a click
// on it presses, and the cells it spans from the left of the drawer's body.
type Hint struct {
	Text, Key string
	X, W      int
}

// Hints lays out the top drawer's hint row for h.
func (s *Stack[H]) Hints(h H) []Hint {
	d := s.Top()
	if d == nil {
		return nil
	}
	return s.hints(h, d)
}

// hints lays out the hint row: each action's key and label, then Esc, which
// goes back to the drawer below or, from the last one, closes. After an
// action's first press the row instead asks for the second.
func (s *Stack[H]) hints(h H, d *Drawer[H]) []Hint {
	var out []Hint
	x := 1
	add := func(text, key string) {
		if len(out) > 0 {
			x += len(" · ")
		}
		out = append(out, Hint{Text: text, Key: key, X: x, W: ansi.StringWidth(text)})
		x += ansi.StringWidth(text)
	}
	actions := d.actions(h)
	for _, action := range actions {
		if action.Key == d.armed {
			add(action.Confirm, action.Key)
			add("any other key cancels", "")
			return out
		}
	}
	for _, action := range actions {
		add(action.Hint+" "+action.Label, action.Key)
	}
	esc := "esc close"
	if len(s.drawers) > 1 {
		esc = "esc back"
	}
	add(esc, "esc")
	return out
}

// Paint draws the stack over screen, which holds one line per row, with h
// deciding the hints. Everything under the top drawer, including lower
// drawers, is dimmed.
func (s *Stack[H]) Paint(h H, screen []string) []string {
	for len(screen) < s.height {
		screen = append(screen, "")
	}
	screen = screen[:s.height]
	top := len(s.drawers) - 1
	for i, d := range s.drawers {
		if i == top {
			for row, line := range screen {
				screen[row] = s.Theme.Dim.Render(ansi.Strip(line))
			}
		}
		r := s.Rect(i)
		for row, line := range s.render(h, d, i) {
			screen[r.Y+row] = Splice(screen[r.Y+row], line, r.X, r.W, s.width)
		}
	}
	return screen
}

// render draws the drawer at level as exactly its area's height of lines,
// each as wide as the area: a rule down its left edge beside a title row,
// the content, and the hint row.
func (s *Stack[H]) render(h H, d *Drawer[H], level int) []string {
	r, body := s.Rect(level), s.Body(level)
	lines := make([]string, 0, r.H)
	lines = append(lines, s.titleRow(h, d, body.W))
	view := d.view.View()
	if highlighter, ok := d.Content.(Highlighter); ok && d.List == nil {
		if decorate := highlighter.Highlight(body.W); decorate != nil {
			view = d.view.ViewWith(decorate)
		}
	}
	for _, line := range strings.Split(view, "\n") {
		lines = append(lines, Pad(line, body.W))
	}
	lines = append(lines, Pad(s.hintRow(h, d), body.W))
	rule := s.Theme.Rule.Render("│")
	for i := range lines {
		lines[i] = rule + lines[i]
	}
	return lines
}

// titleRow renders the title row at width, with the crumb the mouse button
// is held on in the pressed style.
func (s *Stack[H]) titleRow(h H, d *Drawer[H], width int) string {
	text := Fit(" "+d.title(h), width)
	if d.pressed == 0 {
		return s.Theme.Title.Width(width).Render(text)
	}
	trail := d.Trail(h)
	if d.pressed > len(trail) {
		// The trail is read anew at each paint and may have grown shorter
		// since the press.
		return s.Theme.Title.Width(width).Render(text)
	}
	from, to := crumbCells(trail, d.pressed-1)
	from, to = min(from, width), min(to, width)
	// Each part is styled on its own: one part's reset would clear the
	// title's background for the rest of the row.
	return s.Theme.Title.Render(ansi.Cut(text, 0, from)) +
		s.Theme.Pressed.Render(ansi.Cut(text, from, to)) +
		s.Theme.Title.Width(width-to).Render(ansi.Cut(text, to, width))
}

// hintRow renders the hint row, a question asked for a confirmation in the
// warning style.
func (s *Stack[H]) hintRow(h H, d *Drawer[H]) string {
	style := s.Theme.Rule
	if d.armed != "" {
		style = s.Theme.Warn
	}
	var parts []string
	for _, hint := range s.hints(h, d) {
		parts = append(parts, hint.Text)
	}
	return " " + style.Render(strings.Join(parts, " · "))
}

// title is the text of the drawer's title row: its trail, if it has one.
func (d *Drawer[H]) title(h H) string {
	if d.Trail == nil {
		return d.Title
	}
	var labels []string
	for _, crumb := range d.Trail(h) {
		labels = append(labels, crumb.Label)
	}
	return strings.Join(labels, crumbSeparator)
}

// actions is what the drawer offers now.
func (d *Drawer[H]) actions(h H) []Action[H] {
	if d.Actions == nil {
		return nil
	}
	return d.Actions(h)
}

// lines is the drawer's content at width: its Content, or its list.
func (d *Drawer[H]) lines(width int, theme Theme) []string {
	if d.List != nil {
		return d.List.rows(width, theme)
	}
	if d.Content == nil {
		return nil
	}
	return d.Content.Lines(width)
}

// showSelected repaints a list drawer and scrolls its highlighted row into
// view.
func (d *Drawer[H]) showSelected(theme Theme) {
	d.view.SetContentLines(d.lines(d.view.Width(), theme))
	switch row := d.List.Index; {
	case row < d.view.YOffset():
		d.view.SetYOffset(row)
	case row >= d.view.YOffset()+d.view.Height():
		d.view.SetYOffset(row - d.view.Height() + 1)
	}
}
