package markdown

import (
	"fmt"
	"testing"

	"github.com/yuin/goldmark/ast"
)

// TestDumpSegments documents goldmark's segment layout for representative
// documents: which nodes carry segments (block paragraphs do, lists and
// thematic breaks don't), where stops fall relative to newlines, and how
// markers ("- ", "> ") sit outside content segments. The freeze-boundary
// logic in block.go depends on all of these; run with -v when touching it.
func TestDumpSegments(t *testing.T) {
	docs := []string{
		"hello world",
		"first\n\nsecond",
		"# Title\npara",
		"- a\n- b",
		"- a\n- b\n\npara",
		"---",
		"---\npara",
		"> quoted",
		"> quoted\n\npara",
		"```go\ncode()\n```\n\npara",
		"- a\n  continued",
		"- a\n\n  continued",
		"Title\n=====",
		"Title\n\npara",
	}
	for _, doc := range docs {
		t.Run(doc, func(t *testing.T) {
			source := []byte(doc)
			var dump func(n ast.Node, depth int)
			dump = func(n ast.Node, depth int) {
				if _, isInline := n.(ast.Node); isInline && n.Type() == ast.TypeInline {
					t.Logf("%*s%s %T (inline)", depth*2, "", n.Kind().String(), n)
					for c := n.FirstChild(); c != nil; c = c.NextSibling() {
						dump(c, depth+1)
					}
					return
				}
				segs := n.Lines()
				var segStr string
				for i := 0; i < segs.Len(); i++ {
					s := segs.At(i)
					segStr += fmt.Sprintf("[%d,%d)%q ", s.Start, s.Stop, source[s.Start:s.Stop])
				}
				t.Logf("%*s%s %T segs: %s", depth*2, "", n.Kind().String(), n, segStr)
				for c := n.FirstChild(); c != nil; c = c.NextSibling() {
					dump(c, depth+1)
				}
			}
			doc := parse(source)
			dump(doc, 0)
		})
	}
}
