package ui

import (
	"image/color"
	"strings"

	"charm.land/lipgloss/v2"
	"github.com/charmbracelet/x/ansi"
)

// liveStream renders the not-yet-finalized portion of the transcript (an active
// assistant message or reasoning trace) incrementally. It folds incoming deltas
// through a streaming normalizer and a streaming word wrapper, painting only the
// finished wrapped lines once and repainting the single current line per frame.
// This replaces re-normalizing and re-wrapping the whole accumulated message on
// every frame, which grew O(n) per frame and O(n^2) over one long stream.
type liveStream struct {
	painter linePainter
	wrap    *plainWrapper

	// normalizer state. It emits exactly what normalizeText would produce for
	// the concatenation of all deltas, without reprocessing them: trailing
	// spaces are held back, blank runs collapse to one line, and leading and
	// trailing blank lines are dropped.
	line         []byte // current line bytes, trailing spaces excluded
	spaces       []byte // spaces held until a non-space confirms them
	sent         int    // bytes of line already fed to the wrapper
	hasText      bool   // current line has a non-space character
	contentSeen  bool   // a non-blank line has been emitted
	blankRun     int    // consecutive blank source lines since the last content line
	pendingBreak bool   // a completed content line's newline is deferred
	skipLF       bool   // previous byte was a lone CR; swallow a following LF

	painted   []string // painted finished body lines, append-only
	paintedN  int      // finished wrapper lines already painted
	finCache  []string // label line + painted body lines, append-only
	live      string   // cached pending() result
	liveValid bool
}

// linePainter styles wrapped plain lines into full-width display lines. It
// mirrors messageSlab (bg set) and thinkingLines (italic, no padding).
type linePainter struct {
	width     int
	bg        color.Color // nil for no background
	fg        color.Color
	label     string
	labelFg   color.Color
	labelBold bool
	italic    bool
	padLeft   int // 1 for message slabs, 0 for thinking
}

func (p linePainter) labelLine() string {
	style := lipgloss.NewStyle().Foreground(p.labelFg).Background(p.bg)
	if p.labelBold {
		style = style.Bold(true)
	}
	if p.italic {
		style = style.Italic(true)
	}
	prefix := p.pad(p.padLeft) + style.Render(p.label)
	// The fill must be measured ANSI-aware: Render emits SGR escapes and a
	// reset, whose bytes are not printable cells, so len() would leave the
	// background short of the viewport edge.
	return prefix + p.pad(max(0, p.width-ansi.StringWidth(prefix)))
}

func (p linePainter) bodyLine(text string) string {
	style := lipgloss.NewStyle().Foreground(p.fg).Background(p.bg)
	if p.italic {
		style = style.Italic(true)
	}
	line := p.pad(p.padLeft) + style.Render(text)
	return line + p.pad(max(0, p.width-ansi.StringWidth(line)))
}

// pad returns n background-colored spaces (plain spaces when there is no bg).
func (p linePainter) pad(n int) string {
	if n <= 0 {
		return ""
	}
	if p.bg == nil {
		return strings.Repeat(" ", n)
	}
	return bgSpaces(p.bg, n)
}

// newMessageStream builds a live stream for an assistant message slab.
func newMessageStream(label string, bg, fg, labelFg color.Color, width int) *liveStream {
	return newLiveStream(linePainter{
		width: width, bg: bg, fg: fg, label: label,
		labelFg: labelFg, labelBold: true, padLeft: 1,
	})
}

// newThinkingStream builds a live stream for a reasoning trace.
func newThinkingStream(width int) *liveStream {
	return newLiveStream(linePainter{
		width: width, fg: colorFaint, labelFg: colorFaint, label: "thinking", italic: true,
	})
}

func newLiveStream(painter linePainter) *liveStream {
	return &liveStream{
		painter: painter,
		wrap:    newPlainWrapper(max(1, painter.width-2*painter.padLeft)),
	}
}

// append feeds a raw delta through the normalizer into the wrapper.
func (l *liveStream) append(raw string) {
	l.liveValid = false
	i := 0
	for i < len(raw) {
		b := raw[i]
		i++
		switch {
		case b == '\r':
			l.newline()
			l.skipLF = true
			continue
		case b == '\n':
			if l.skipLF {
				l.skipLF = false
				continue
			}
			l.newline()
			continue
		}
		l.skipLF = false
		if b == ' ' {
			l.spaces = append(l.spaces, b)
			continue
		}
		l.flushLine()
		if !l.contentSeen {
			l.contentSeen = true
		} else {
			breaks := 0
			if l.pendingBreak {
				breaks = 1
			}
			if l.blankRun > 0 {
				breaks = 2
			}
			for ; breaks > 0; breaks-- {
				l.wrap.Write("\n")
			}
		}
		l.pendingBreak = false
		l.blankRun = 0
		if len(l.spaces) > 0 {
			l.line = append(l.line, l.spaces...)
			l.spaces = l.spaces[:0]
		}
		l.hasText = true
		l.line = append(l.line, b)
	}
	l.flushLine()
}

// flushLine sends the confirmed current-line bytes (trailing spaces excluded) to
// the wrapper, tracking how much has already been sent.
func (l *liveStream) flushLine() {
	if len(l.line) > l.sent {
		l.wrap.Write(string(l.line[l.sent:]))
		l.sent = len(l.line)
	}
}

// newline ends the current line. The newline after a content line is deferred
// until the next content line arrives, so a trailing newline does not add a
// spurious empty line. A blank line only increments blankRun and is materialized
// as one separator, so blank runs collapse and leading/trailing blanks drop.
func (l *liveStream) newline() {
	l.spaces = l.spaces[:0]
	if l.hasText {
		l.flushLine()
		l.contentSeen = true
		l.pendingBreak = true
		l.blankRun = 0
	} else if l.contentSeen {
		l.blankRun++
	}
	l.line = l.line[:0]
	l.sent = 0
	l.hasText = false
}

// pending returns the live portion as a newline-joined string, matching what
// normalizeText plus messageSlab/thinkingLines would produce for the whole
// accumulated stream.
func (l *liveStream) pending() string {
	if !l.liveValid {
		l.live = strings.Join(l.Lines(), "\n")
		l.liveValid = true
	}
	return l.live
}

// Lines returns the painted display lines for the live portion: the label line
// followed by the wrapped body. All but the current body line are cached across
// frames.
func (l *liveStream) Lines() []string {
	fin := l.finalized()
	out := make([]string, 0, len(fin)+1)
	out = append(out, fin...)
	out = append(out, l.current())
	return out
}

// finalized returns the label line plus the finished body lines. The result is
// append-only across appends, so a caller can reuse the prefix.
func (l *liveStream) finalized() []string {
	wrapped := l.wrap.Finalized()
	for len(l.painted) < len(wrapped) {
		l.painted = append(l.painted, l.painter.bodyLine(wrapped[len(l.painted)]))
	}
	if l.finCache == nil {
		l.finCache = append(l.finCache, l.painter.labelLine())
	}
	for len(l.finCache) < len(l.painted)+1 {
		l.finCache = append(l.finCache, l.painted[len(l.finCache)-1])
	}
	return l.finCache
}

// current returns the still-growing body line.
func (l *liveStream) current() string {
	return l.painter.bodyLine(l.wrap.Current())
}
