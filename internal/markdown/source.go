package markdown

import (
	"bytes"
	"strings"

	"github.com/yuin/goldmark/ast"
)

// SourceRun ties part of a line's text to the source it was rendered from:
// Text[At:At+Len] stands for source[Source:Source+SourceLen]. Most runs match
// byte for byte; an escape or character reference decodes to text of another
// length, so its run maps only at its edges.
type SourceRun struct {
	At, Len           int
	Source, SourceLen int
}

// exact reports whether the run matches the source byte for byte.
func (r SourceRun) exact() bool { return r.Len == r.SourceLen }

// sourceAt returns where a selection starting at byte i of the line's text,
// inside the run, starts in the source.
func (r SourceRun) sourceAt(i int) int {
	if r.exact() {
		return r.Source + i - r.At
	}
	return r.Source
}

// sourceEnd returns where a selection ending before byte i of the line's
// text, inside the run or at its end, ends in the source.
func (r SourceRun) sourceEnd(i int) int {
	if r.exact() {
		return r.Source + i - r.At
	}
	return r.Source + r.SourceLen
}

// SelectionSource returns the part of the source that a selection of rendered
// lines covers, from byte startCol of lines[startRow] up to byte endCol of
// lines[endRow]: from the earliest source byte the selection shows to the
// latest. The screen can show source out of order, as a table does when its
// cells wrap, so the range is not simply where the selection starts and ends.
// Text the renderer added, such as a bullet or the blank line between blocks,
// has no source and counts for nothing, so the range starts and ends in text
// that came from the source, ready for Excerpt. ok is false when the
// selection holds no such text.
func SelectionSource(lines []Line, startRow, startCol, endRow, endCol int) (start, end int, ok bool) {
	startRow, endRow = max(startRow, 0), min(endRow, len(lines)-1)
	start, end = -1, -1
	for row := startRow; row <= endRow; row++ {
		from, until := 0, len(lines[row].Text)
		if row == startRow {
			from = startCol
		}
		if row == endRow {
			until = endCol
		}
		for _, r := range lines[row].Runs {
			if a, b := max(r.At, from), min(r.At+r.Len, until); a < b {
				if s := r.sourceAt(a); start < 0 || s < start {
					start = s
				}
				end = max(end, r.sourceEnd(b))
			}
		}
	}
	if start < 0 || end <= start {
		return 0, 0, false
	}
	return start, end, true
}

// appendRun appends r to runs, extending the last run instead when r
// continues it in both the text and the source. goldmark splits text where an
// inline element could start, such as at every space, and one run per stretch
// is easier to read and cheaper to clip.
func appendRun(runs []SourceRun, r SourceRun) []SourceRun {
	if n := len(runs); n > 0 {
		last := &runs[n-1]
		if last.exact() && r.exact() && last.At+last.Len == r.At && last.Source+last.SourceLen == r.Source {
			last.Len += r.Len
			last.SourceLen += r.SourceLen
			return runs
		}
	}
	return append(runs, r)
}

// appendClipped appends to dst the runs over text[start:end], re-based to
// start. runs must be sorted, and from is the index to search from; next is
// where the search for a later stretch of text can pick up.
func appendClipped(dst, runs []SourceRun, from, start, end int) (_ []SourceRun, next int) {
	next = from
	for i := from; i < len(runs); i++ {
		r := runs[i]
		if r.At >= end {
			break
		}
		if r.At+r.Len <= start {
			next = i + 1
			continue
		}
		a, b := max(r.At, start), min(r.At+r.Len, end)
		c := SourceRun{At: a - start, Len: b - a, Source: r.Source, SourceLen: r.SourceLen}
		if r.exact() {
			c.Source, c.SourceLen = r.Source+a-r.At, b-a
		}
		dst = append(dst, c)
	}
	return dst, next
}

// shiftRuns moves runs right by n bytes of text, in place, for a line that
// gains a prefix.
func shiftRuns(runs []SourceRun, n int) []SourceRun {
	for i := range runs {
		runs[i].At += n
	}
	return runs
}

// decode resolves a Text node's raw bytes like unescape, and maps the text it
// returns, which starts at bytes into the next piece, back to raw, which
// starts at src in the source. Escapes and character references are decoded
// one at a time, so each maps at its own edges and the plain text around
// them maps byte for byte.
func (t *inlineText) decode(raw []byte, at, src int) string {
	if !bytes.ContainsAny(raw, "&\\\x00") {
		t.mapText(at, len(raw), src, len(raw))
		return string(raw)
	}
	var text strings.Builder
	add := func(from, to int, decoded string) {
		t.mapText(at+text.Len(), len(decoded), src+from, to-from)
		text.WriteString(decoded)
	}
	plain := 0
	for i := 0; i < len(raw); {
		n := escapeLen(raw, i)
		if n == 0 {
			i++
			continue
		}
		add(plain, i, strings.Map(dropControl, string(raw[plain:i])))
		add(i, i+n, unescape(raw[i:i+n]))
		i += n
		plain = i
	}
	add(plain, len(raw), strings.Map(dropControl, string(raw[plain:])))
	return text.String()
}

// escapeLen returns the length of the backslash escape, character reference,
// or NUL at raw[i], or 0 when there is none.
func escapeLen(raw []byte, i int) int {
	switch raw[i] {
	case 0:
		return 1
	case '\\':
		if i+1 < len(raw) && isPunct(raw[i+1]) {
			return 2
		}
	case '&':
		if semi := bytes.IndexByte(raw[i:min(len(raw), i+32)], ';'); semi > 0 {
			if ref := raw[i : i+semi+1]; unescape(ref) != string(ref) {
				return len(ref)
			}
		}
	}
	return 0
}

// autoLinkLabel returns where an autolink's label starts in source, or -1.
// goldmark keeps the label without its position, so it is searched for from
// where the node starts.
func autoLinkLabel(v *ast.AutoLink, source []byte) int {
	label := v.Label(source)
	from := max(0, v.Pos())
	i := bytes.Index(source[from:], label)
	if len(label) == 0 || i < 0 {
		return -1
	}
	return from + i
}
