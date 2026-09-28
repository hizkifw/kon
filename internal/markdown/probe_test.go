package markdown

import (
	"slices"
	"testing"

	"github.com/yuin/goldmark/ast"
)

// TestGoldmarkSegmentLayout pins the parts of goldmark's segment layout that the
// freeze-boundary logic in block.go is built on. A goldmark upgrade that
// fails here moves the ground under block.go's boundary rules, so revisit
// them before taking it.
func TestGoldmarkSegmentLayout(t *testing.T) {
	cases := []struct {
		name string
		doc  string
		kind ast.NodeKind // the first node of this kind in document order
		want [][2]int     // every segment in that node's subtree, [start, stop)
	}{
		// Containers carry no segments of their own, and their markers sit
		// outside their content's segments. So subtreeStart and subtreeStop
		// must walk down to the content to find a container's extent, and
		// that extent starts mid-line: renderChildren snaps each boundary
		// back with lineStartBefore, since resuming the parse at the segment
		// would drop the marker.
		{"list", "- a\n- b", ast.KindList, [][2]int{{2, 3}, {6, 7}}},
		{"blockquote", "> quoted", ast.KindBlockquote, [][2]int{{2, 8}}},
		// Prose stops before its line's newline, trailing spaces trimmed.
		// renderChildren's gap check counts newlines from lineStartBefore(stop),
		// which lands on the block's own last line only because of this; a
		// stop past the newline would undercount the gap by one line.
		{"paragraph stop", "para  \n\nnext", ast.KindParagraph, [][2]int{{0, 4}}},
		// A fence's segments are its content lines, newlines included. The
		// opener sits one line above the first start, so renderChildren backs
		// a boundary up one line to keep it, and the last stop is the closer's
		// line start, which is where the last-child check looks for the
		// closer.
		{"fence", "```go\ncode()\n```\n", ast.KindFencedCodeBlock, [][2]int{{6, 13}}},
		// A just-opened fence and a rule have no segments, so the block before
		// one has no known end. renderChildren holds that block open rather
		// than freezing it over the sibling's source.
		{"fence opener only", "para\n\n```go\n", ast.KindFencedCodeBlock, nil},
		{"rule", "para\n\n---\n", ast.KindThematicBreak, nil},
		// An empty item has no segments, so a list that opens with one starts
		// two lines below its first marker as far as segments show.
		// renderChildren's gap check refuses a boundary that would strand
		// those marker lines in the tail.
		{"empty first item", "- \n\n  - a\n", ast.KindList, [][2]int{{8, 9}}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			n := firstOfKind(parse([]byte(tc.doc)), tc.kind)
			if n == nil {
				t.Fatalf("doc=%q has no %s node", tc.doc, tc.kind)
			}
			if got := subtreeSegments(n); !slices.Equal(got, tc.want) {
				t.Fatalf("doc=%q %s segments=%v want=%v", tc.doc, tc.kind, got, tc.want)
			}
		})
	}

	// An HTML block keeps its closing line apart from its segments, newline
	// included, so renderChildren freezes a closed trailing HTML block at
	// lineStartBefore(ClosureLine.Stop): just past the closer.
	t.Run("html closer", func(t *testing.T) {
		src := []byte("<script>\nvar x;\n</script>\n")
		h, ok := firstOfKind(parse(src), ast.KindHTMLBlock).(*ast.HTMLBlock)
		if !ok || !h.HasClosure() {
			t.Fatalf("doc=%q parsed no closed HTML block", src)
		}
		if got := [2]int{h.ClosureLine.Start, h.ClosureLine.Stop}; got != [2]int{16, 26} {
			t.Fatalf("doc=%q closure line=%v want [16 26]", src, got)
		}
	})
}

// firstOfKind returns the first node of kind k in document order, or nil.
func firstOfKind(n ast.Node, k ast.NodeKind) ast.Node {
	if n.Kind() == k {
		return n
	}
	for c := n.FirstChild(); c != nil; c = c.NextSibling() {
		if f := firstOfKind(c, k); f != nil {
			return f
		}
	}
	return nil
}

// subtreeSegments returns the segments of every block in n's subtree, n
// included, in document order. Inline nodes carry no segments.
func subtreeSegments(n ast.Node) [][2]int {
	var out [][2]int
	if n.Type() == ast.TypeBlock {
		for i := 0; i < n.Lines().Len(); i++ {
			s := n.Lines().At(i)
			out = append(out, [2]int{s.Start, s.Stop})
		}
	}
	for c := n.FirstChild(); c != nil; c = c.NextSibling() {
		out = append(out, subtreeSegments(c)...)
	}
	return out
}
