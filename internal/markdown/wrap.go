package markdown

import (
	"github.com/charmbracelet/x/ansi"
)

// displayWidth returns the printable cell width of s.
func displayWidth(s string) int { return ansi.StringWidth(s) }

// hardWrapPieces splits a single styled piece into lines of at most limit
// display cells, preserving every character (runs of spaces, indentation).
// Splits fall between grapheme clusters; a cluster wider than the whole limit
// is placed alone so progress is guaranteed. Used for code, where collapsing
// whitespace the way word wrapping does would corrupt the source. runs map
// the piece's text back to the source.
func hardWrapPieces(p piece, runs []SourceRun, limit int) []Line {
	if limit < 1 {
		limit = 1
	}
	if displayWidth(p.text) <= limit {
		return []Line{{Text: p.text, Spans: spanFor(p), Runs: runs}}
	}
	var out []Line
	rest := p.text
	next := 0
	for rest != "" {
		chunk := truncateWidth([]byte(rest), limit)
		if len(chunk) == 0 {
			// A single cluster wider than the limit: take one cluster.
			cluster, _ := ansi.FirstGraphemeCluster([]byte(rest), ansi.GraphemeWidth)
			if len(cluster) == 0 {
				break
			}
			chunk = cluster
		}
		at := len(p.text) - len(rest)
		var chunkRuns []SourceRun
		chunkRuns, next = appendClipped(nil, runs, next, at, at+len(chunk))
		out = append(out, Line{Text: string(chunk), Spans: spanFor(piece{text: string(chunk), style: p.style, link: p.link}), Runs: chunkRuns})
		rest = rest[len(chunk):]
	}
	return out
}

// spanFor returns the styled span for a piece, or nil when it is unstyled.
func spanFor(p piece) []Styled {
	if (p.style == StyleNone && p.link == "") || p.text == "" {
		return nil
	}
	return []Styled{{Text: p.text, Style: p.style, Link: p.link}}
}

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
