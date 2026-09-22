package markdown

import (
	"github.com/charmbracelet/x/ansi"
)

// wrapper word-wraps plain text to a column limit, streaming: Write can be
// called repeatedly with deltas and only the current trailing line is ever
// revised. Lines is append-only; Current returns the growing tail.
//
// Forked from internal/ui's plainWrapper on purpose: the ui version must
// match lipgloss.Wrap byte-for-byte (guarded by TestPlainWrapperMatchesLipgloss)
// including its hyphen-breakpoint and control-byte quirks, while this
// package needs only deterministic greedy wrap for markdown blocks. When
// the transcript renders markdown blocks through this package the ui
// wrapper is bypassed entirely, so the two need not agree; sharing one
// implementation would couple markdown rendering to lipgloss's exact
// behavior. Revisit if the two must converge visually.
type wrapper struct {
	limit int

	lines []string
	cur   []byte
	word  []byte
	space []byte

	curWidth   int
	wordLen    int
	spaceWidth int
}

func newWrapper(limit int) *wrapper { return &wrapper{limit: max(1, limit)} }

func (w *wrapper) addSpace() {
	if len(w.space) == 0 {
		return
	}
	w.cur = append(w.cur, w.space...)
	w.curWidth += w.spaceWidth
	w.space = w.space[:0]
	w.spaceWidth = 0
}

func (w *wrapper) addWord() {
	if len(w.word) == 0 {
		return
	}
	w.addSpace()
	w.curWidth += w.wordLen
	w.cur = append(w.cur, w.word...)
	w.word = w.word[:0]
	w.wordLen = 0
}

func (w *wrapper) addNewline() {
	w.lines = append(w.lines, string(w.cur))
	w.cur = w.cur[:0]
	w.curWidth = 0
	w.space = w.space[:0]
	w.spaceWidth = 0
}

// Write feeds more text into the wrapper.
func (w *wrapper) Write(s string) {
	i := 0
	for i < len(s) {
		b := s[i]
		switch {
		case b == '\n':
			w.addWord()
			w.addNewline()
			i++
			continue
		case b == ' ' || b == '\t':
			w.addWord()
			w.space = append(w.space, ' ')
			w.spaceWidth++
			i++
			continue
		}
		j := i
		for j < len(s) && s[j] != ' ' && s[j] != '\t' && s[j] != '\n' {
			j++
		}
		w.word = append(w.word, s[i:j]...)
		w.wordLen += displayWidth(s[i:j])
		i = j
		if w.wordLen > w.limit {
			// Hard-split oversized words.
			w.addSpace()
			for w.wordLen > w.limit {
				space := w.limit - w.curWidth
				if space <= 0 {
					w.addNewline()
					continue
				}
				chunk := truncateWidth(w.word, space)
				cw := displayWidth(string(chunk))
				w.cur = append(w.cur, chunk...)
				w.curWidth += cw
				w.word = w.word[len(chunk):]
				w.wordLen -= cw
				w.addNewline()
			}
			continue
		}
		if w.curWidth+w.spaceWidth+w.wordLen > w.limit {
			w.addNewline()
		}
	}
}

// Lines finishes wrapping and returns all display lines. Append-only across
// Write calls.
func (w *wrapper) Lines() []string {
	w.addWord()
	w.lines = append(w.lines, string(w.cur))
	return w.lines
}

// Current returns the still-growing line.
func (w *wrapper) Current() string {
	if len(w.space) == 0 && len(w.word) == 0 {
		return string(w.cur)
	}
	b := make([]byte, 0, len(w.cur)+len(w.space)+len(w.word))
	b = append(b, w.cur...)
	b = append(b, w.space...)
	b = append(b, w.word...)
	return string(b)
}

// Finalized returns lines that can no longer change.
func (w *wrapper) Finalized() []string { return w.lines }

// displayWidth returns the printable cell width of s.
func displayWidth(s string) int { return ansi.StringWidth(s) }

// truncateWidth cuts s to at most n display cells, splitting between
// grapheme clusters (never inside an emoji ZWJ sequence or combining run).
// Width is tracked incrementally, so the cost is O(len(s)) once, not per
// candidate cut.
func truncateWidth(s []byte, n int) []byte {
	if n <= 0 {
		return nil
	}
	rest := s
	written := 0
	for len(rest) > 0 {
		cluster, width := ansi.FirstGraphemeCluster(rest, ansi.GraphemeWidth)
		if len(cluster) == 0 {
			break
		}
		if written+width > n {
			return s[:len(s)-len(rest)]
		}
		written += width
		rest = rest[len(cluster):]
	}
	return s
}
