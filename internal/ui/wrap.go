package ui

import (
	"strings"
	"unicode"
	"unicode/utf8"

	"github.com/charmbracelet/x/ansi"
)

// plainWrapper word-wraps plain (ANSI-free) text the way lipgloss.Wrap does with
// no breakpoints, but streams: Write can be called repeatedly with deltas and
// only the current trailing line is ever revised. That makes wrapping a growing
// live message linear in the deltas rather than O(message) per frame.
//
// The algorithm mirrors ansi.wrap's greedy behaviour: words are kept whole,
// over-long words are hard-wrapped, runs of spaces are held back so they are not
// emitted before a word that might still arrive, and a hyphen is a breakpoint.
type plainWrapper struct {
	limit int

	lines []string // finished lines, never revised
	cur   []byte   // bytes of the current, still-growing line
	word  []byte   // pending word bytes
	space []byte   // pending spaces

	curWidth   int // written width of cur
	wordLen    int // word width excluding ANSI (plain, so == rune count)
	spaceWidth int
}

func newPlainWrapper(limit int) *plainWrapper {
	if limit < 1 {
		limit = 1
	}
	return &plainWrapper{limit: limit}
}

func (w *plainWrapper) addSpace() {
	if len(w.space) == 0 {
		return
	}
	w.cur = append(w.cur, w.space...)
	w.curWidth += w.spaceWidth
	w.space = w.space[:0]
	w.spaceWidth = 0
}

func (w *plainWrapper) addWord() {
	if len(w.word) == 0 {
		return
	}
	w.addSpace()
	w.curWidth += w.wordLen
	w.cur = append(w.cur, w.word...)
	w.word = w.word[:0]
	w.wordLen = 0
}

func (w *plainWrapper) addNewline() {
	w.lines = append(w.lines, string(w.cur))
	w.cur = w.cur[:0]
	w.curWidth = 0
	w.space = w.space[:0]
	w.spaceWidth = 0
}

func (w *plainWrapper) hardwrapIfNeeded() {
	if w.wordLen == w.limit {
		w.addWord()
	}
}

func (w *plainWrapper) writeASCIIWordByte(b byte) {
	if w.curWidth == w.limit {
		w.addNewline()
	}
	w.word = append(w.word, b)
	w.wordLen++
	w.hardwrapIfNeeded()
	if w.curWidth+w.wordLen+w.spaceWidth > w.limit {
		w.addNewline()
	}
}

// writeASCIIWord appends a run of ASCII word bytes, using a bulk fast path when
// the run clearly fits without a wrap or hard-wrap boundary.
func (w *plainWrapper) writeASCIIWord(run string) {
	if w.wordLen+len(run) <= w.limit && w.curWidth+w.wordLen+w.spaceWidth+len(run) <= w.limit {
		w.word = append(w.word, run...)
		w.wordLen += len(run)
		w.hardwrapIfNeeded()
		return
	}
	for i := 0; i < len(run); i++ {
		w.writeASCIIWordByte(run[i])
	}
}

func (w *plainWrapper) writeSpace(b byte) {
	w.addWord()
	w.space = append(w.space, b)
	w.spaceWidth++
}

func (w *plainWrapper) writeHyphen() {
	w.addSpace()
	if w.curWidth+w.wordLen >= w.limit {
		w.word = append(w.word, '-')
		w.wordLen++
		return
	}
	w.addWord()
	w.cur = append(w.cur, '-')
	w.curWidth++
}

func (w *plainWrapper) writeNewline() {
	if w.wordLen == 0 {
		if w.curWidth+w.spaceWidth > w.limit {
			w.curWidth = 0
		} else {
			w.cur = append(w.cur, w.space...)
		}
		w.space = w.space[:0]
		w.spaceWidth = 0
	}
	w.addWord()
	w.addNewline()
}

func (w *plainWrapper) writeCluster(cluster string, width int, isSpace bool) {
	if isSpace {
		w.addWord()
		w.space = append(w.space, cluster...)
		w.spaceWidth += width
		return
	}
	if w.wordLen+width > w.limit {
		w.addWord()
	}
	w.word = append(w.word, cluster...)
	w.wordLen += width
	if w.curWidth+w.wordLen+w.spaceWidth > w.limit {
		w.addNewline()
	}
	w.hardwrapIfNeeded()
}

// Write feeds text into the wrapper.
func (w *plainWrapper) Write(s string) {
	i := 0
	for i < len(s) {
		b := s[i]
		if b < utf8.RuneSelf {
			switch {
			case b == '\n':
				w.writeNewline()
				i++
			case b == ' ' || (b >= '\t' && b <= '\r'):
				w.writeSpace(b)
				i++
			case b == '-':
				w.writeHyphen()
				i++
			default:
				j := i
				for j < len(s) && isASCIIWordByte(s[j]) {
					j++
				}
				if j == i {
					// Control or other ASCII byte: treat as a word byte.
					w.writeASCIIWordByte(b)
					i++
				} else {
					w.writeASCIIWord(s[i:j])
					i = j
				}
			}
			continue
		}
		cluster, width := ansi.FirstGraphemeCluster(s[i:], ansi.GraphemeWidth)
		if cluster == "" {
			break
		}
		i += len(cluster)
		r, _ := utf8.DecodeRuneInString(cluster)
		w.writeCluster(cluster, width, unicode.IsSpace(r) && r != '\u00a0')
	}
}

func isASCIIWordByte(b byte) bool {
	return b > ' ' && b < 0x7f && b != '-'
}

// Lines finishes wrapping and returns the display lines.
func (w *plainWrapper) Lines() []string {
	if w.wordLen == 0 {
		if w.curWidth+w.spaceWidth > w.limit {
			w.curWidth = 0
		} else {
			w.cur = append(w.cur, w.space...)
		}
		w.space = w.space[:0]
		w.spaceWidth = 0
	}
	w.addWord()
	w.lines = append(w.lines, string(w.cur))
	return w.lines
}

// Finalized returns the lines that can no longer change as more text is written.
// The returned slice is append-only across Write calls.
func (w *plainWrapper) Finalized() []string {
	return w.lines
}

// Current returns the still-growing line, including the pending word and spaces
// that have not yet been committed to it.
func (w *plainWrapper) Current() string {
	if len(w.space) == 0 && len(w.word) == 0 {
		return string(w.cur)
	}
	b := make([]byte, 0, len(w.cur)+len(w.space)+len(w.word))
	b = append(b, w.cur...)
	b = append(b, w.space...)
	b = append(b, w.word...)
	return string(b)
}

// wrapPlain wraps plain text to limit columns.
func wrapPlain(s string, limit int) []string {
	w := newPlainWrapper(limit)
	w.Write(s)
	return w.Lines()
}

// ansiStripper removes ANSI escape sequences (CSI, OSC, two-byte escapes) and
// C0 control bytes other than \n, \r, and \t from a stream of deltas.
// plainWrapper parses those bytes as printable word text and would split an
// escape mid-sequence, so its input must be plain text;
// transcript.appendStream and transcript.appendThinking feed every delta
// through one of these first.
//
// Sequences are dropped by state rather than by buffering, so one that is
// split across deltas is still removed whole: the machine stays in its
// sequence state, emitting nothing, until a terminating byte arrives.
type ansiStripper struct {
	state uint8
}

const (
	stripGround = iota // plain text
	stripEsc           // saw ESC; sequence bytes follow
	stripCSI           // inside CSI; consume through the final byte 0x40-0x7e
	stripOSC           // inside OSC; consume through BEL or ST (ESC \)
	stripOSCESC        // inside OSC; saw the ESC of an ST terminator
)

// strip returns text with escapes and control bytes removed. The result may be
// shorter than expected when text ends inside a sequence; the rest of that
// sequence is dropped when it arrives in a later delta.
func (s *ansiStripper) strip(text string) string {
	if s.state == stripGround {
		i := 0
		for i < len(text) {
			b := text[i]
			if b == 0x1b || b == 0x7f || (b < 0x20 && b != '\n' && b != '\r' && b != '\t') {
				break
			}
			i++
		}
		if i == len(text) {
			return text
		}
	}
	var out strings.Builder
	out.Grow(len(text) + 16)
	for i := 0; i < len(text); i++ {
		b := text[i]
		switch s.state {
		case stripGround:
			switch {
			case b == 0x1b:
				s.state = stripEsc
			case b == '\n' || b == '\r' || b == '\t':
				out.WriteByte(b)
			case b < 0x20 || b == 0x7f:
				// other control byte: drop
			default:
				out.WriteByte(b)
			}
		case stripEsc:
			switch {
			case b == '[':
				s.state = stripCSI
			case b == ']':
				s.state = stripOSC
			case b >= 0x20 && b <= 0x2f:
				// intermediate byte (as in ESC ( B): stay in the escape
			default:
				s.state = stripGround // two-byte escape finished
			}
		case stripCSI:
			if b >= 0x40 && b <= 0x7e {
				s.state = stripGround
			}
		case stripOSC:
			switch b {
			case 0x07:
				s.state = stripGround
			case 0x1b:
				s.state = stripOSCESC
			}
		case stripOSCESC:
			// ST is ESC \. Anything else after ESC inside an OSC is invalid;
			// consume the byte and resume ground either way.
			s.state = stripGround
		}
	}
	return out.String()
}
