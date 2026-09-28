package markdown

import (
	"strings"
	"testing"
)

// excerptCase marks its selection in source with ⟦ and ⟧. The markers are
// removed before the call, so the offsets they leave are the ones a source
// map of the rendering would hand over for that selection on screen.
type excerptCase struct {
	name   string
	marked string
	want   string
}

func runExcerptCases(t *testing.T, cases []excerptCase) {
	t.Helper()
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			source, start, end := unmark(t, c.marked)
			if got := Excerpt(source, start, end); got != c.want {
				t.Fatalf("Excerpt(%q, %d, %d)\n got: %q\nwant: %q", source, start, end, got, c.want)
			}
		})
	}
}

// unmark removes the selection markers and returns the byte offsets they
// stood at.
func unmark(t *testing.T, marked string) (source string, start, end int) {
	t.Helper()
	const open, close = "⟦", "⟧"
	start, end = strings.Index(marked, open), strings.Index(marked, close)
	if strings.Count(marked, open) != 1 || strings.Count(marked, close) != 1 || end < start {
		t.Fatalf("case needs one ⟦ before one ⟧: %q", marked)
	}
	source = marked[:start] + marked[start+len(open):end] + marked[end+len(close):]
	return source, start, end - len(open)
}

func TestExcerptPlainText(t *testing.T) {
	runExcerptCases(t, []excerptCase{
		{"word", "Plain ⟦words⟧ here", "words"},
		{"whole paragraph", "⟦Just a paragraph.⟧", "Just a paragraph."},
		{"empty", "Some ⟦⟧text", ""},
		{"edge whitespace dropped", "one⟦ two ⟧three", "two"},
		{"soft break kept", "first line ⟦ends\nsecond⟧ line", "ends\nsecond"},
		{"whole document", "⟦# Title\n\nSome **bold** text.\n\n- a\n- b⟧", "# Title\n\nSome **bold** text.\n\n- a\n- b"},
		{"raw HTML copies as shown", "press <kbd>⟦Ctrl⟧</kbd> now", "Ctrl"},
	})
}

func TestExcerptEmphasis(t *testing.T) {
	runExcerptCases(t, []excerptCase{
		{"inside strong", "**bo⟦ld te⟧xt**", "**ld te**"},
		{"starts inside strong", "**bold ⟦text** and⟧ more", "**text** and"},
		{"ends inside strong", "some ⟦text **bo⟧ld**", "text **bo**"},
		{"whole strong with delimiters", "a ⟦**bold**⟧ b", "**bold**"},
		{"strong text exactly", "a **⟦bold⟧** b", "**bold**"},
		{"underscores kept", "__bo⟦ld__ x⟧", "__ld__ x"},
		{"single asterisk", "*it⟦al* x⟧", "*al* x"},
		{"single underscore", "_em⟦ph_ x⟧", "_ph_ x"},
		{"triple delimiters", "***both ⟦here***⟧", "***here***"},
		{"nested different delimiters", "**bold _it⟦al_ more** tail⟧", "**_al_ more** tail"},
		{"both ends in one nested element", "**a _b⟦c⟧d_ e**", "**_c_**"},
		{"ends in different elements", "**a⟦b** plain *c⟧d*", "**b** plain *c*"},
		{"whitespace kept off delimiters", "**bold⟦ text ⟧more**", "**text**"},
		{"intraword underscores stay literal", "snake_ca⟦se_na⟧me", "se_na"},
		{"strikethrough", "~~str⟦ike~~ x⟧", "~~ike~~ x"},
		{"single tilde strikethrough", "~sin⟦gle~ x⟧", "~gle~ x"},
	})
}

func TestExcerptCodeSpans(t *testing.T) {
	runExcerptCases(t, []excerptCase{
		{"inside", "`some ⟦code⟧ here`", "`code`"},
		{"starts inside", "`co⟦de` and⟧ more", "`de` and"},
		{"ends inside", "run ⟦`make ch⟧eck`", "`make ch`"},
		{"markup inside stays literal", "`**not ⟦bold**`⟧", "`bold**`"},
		{"inside strong", "Paragraph with **bold text and `code block⟦ in` the middle** of⟧ it", "**` in` the middle** of"},
		{"backtick count kept", "``a ` ⟦b``⟧ x", "``b``"},
		// Content that now starts with a backtick needs the padding spaces,
		// or it would merge into the opening run.
		{"backtick at the cut is padded", "``a ⟦` b⟧``", "`` ` b ``"},
		{"padding kept", "`` `ti⟦ck` ``⟧", "`` ck` ``"},
		// goldmark strips a line ending next to the backticks like a space.
		{"line endings as padding", "a ``\n⟦x⟧\n`` b", "``x``"},
		{"content across lines", "`co⟦de\nspan` x⟧", "`de\nspan` x"},
	})
}

func TestExcerptLinks(t *testing.T) {
	runExcerptCases(t, []excerptCase{
		{"part of the label", "[my ⟦link⟧ here](https://x.test)", "[link](https://x.test)"},
		{"label exactly", "see [⟦docs⟧](https://x.test) now", "[docs](https://x.test)"},
		{"whole link", "see ⟦[docs](https://x.test)⟧ now", "[docs](https://x.test)"},
		{"starts inside the label", "[my li⟦nk](https://x.test) and more⟧", "[nk](https://x.test) and more"},
		{"ends inside the label", "⟦go to [the do⟧cs](https://x.test)", "go to [the do](https://x.test)"},
		{"title kept", "[a⟦b⟧c](https://x.test \"Title\")", "[b](https://x.test \"Title\")"},
		{"angle destination kept", "[a⟦b⟧c](<https://x.test/a b>)", "[b](<https://x.test/a b>)"},
		{"emphasis in the label", "[**bo⟦ld** li⟧nk](https://x.test)", "[**ld** li](https://x.test)"},
		{"code in the label", "[`some ⟦func`⟧](https://x.test)", "[`func`](https://x.test)"},
		{"inside emphasis", "**see [the ⟦docs](https://x.test) now**⟧", "**[docs](https://x.test) now**"},
		{"image", "![al⟦t te⟧xt](img.png)", "![t te](img.png)"},
		// A full reference names its target apart from the label, so the label
		// can be cut. The definition is left behind: it is not resolved.
		{"full reference", "[my ⟦link⟧ here][ref]\n\n[ref]: https://x.test", "[link][ref]"},
		// Collapsed and shortcut references find their target by the label
		// itself, so cutting it would break the link.
		{"collapsed reference whole", "[my ⟦link⟧][]\n\n[my link]: https://x.test", "[my link][]"},
		{"shortcut reference whole", "[my ⟦link⟧]\n\n[my link]: https://x.test", "[my link]"},
		{"undefined reference is text", "[my ⟦link⟧][ref]", "link"},
		{"autolink whole", "<https://x⟦.test/pa⟧th>", "<https://x.test/path>"},
		{"bare URL whole", "visit https://x.test/pa⟦th now⟧", "https://x.test/path now"},
	})
}

func TestExcerptEscapes(t *testing.T) {
	runExcerptCases(t, []excerptCase{
		{"escape kept", "a ⟦\\*not em\\*⟧ b", "\\*not em\\*"},
		{"start splitting an escape", "x \\⟦*y⟧", "\\*y"},
		{"end splitting an escape", "⟦b \\⟧* c", "b \\*"},
		{"entity whole", "&am⟦p; x⟧", "&amp; x"},
		// Mid-line text that would open a block at the start of a line.
		{"heading marker escaped", "the ⟦# of items⟧", "\\# of items"},
		{"bullet marker escaped", "a ⟦- b⟧", "\\- b"},
		{"star marker escaped", "a ⟦* b⟧", "\\* b"},
		{"ordered marker escaped", "x ⟦1. y⟧", "1\\. y"},
		{"quote marker escaped", "x ⟦> y⟧", "\\> y"},
		{"rule escaped", "a ⟦---⟧ b", "\\---"},
		{"hashtag not escaped", "see ⟦#hashtag⟧", "#hashtag"},
		{"no escape after an opener", "**the ⟦# of⟧ items**", "**# of**"},
		// Punctuation that could open or close nothing where it stood can once
		// the cut stands next to it.
		{"literal underscores at the cut", "snake⟦_case_⟧name", "\\_case\\_"},
		{"literal underscores next to an opener", "**⟦_a_⟧b**", "**\\_a\\_**"},
		{"literal asterisks at the cut", "a ⟦* b *⟧ c", "\\* b \\*"},
		{"literal backticks at the cut", "a`⟦`b`⟧c", "\\`b\\`"},
		{"literal backslash before a closer", "**⟦a\\⟧ b**", "**a\\\\**"},
	})
}

func TestExcerptHeadings(t *testing.T) {
	runExcerptCases(t, []excerptCase{
		{"inside", "## Tit⟦le wo⟧rds", "le wo"},
		{"into a paragraph", "## Tit⟦le\n\nBody te⟧xt", "## le\n\nBody te"},
		{"from its first letter", "## ⟦Title\n\nBody⟧", "## Title\n\nBody"},
		{"marker before an opener", "## **Bo⟦ld** title\n\nBody⟧", "## **ld** title\n\nBody"},
		{"setext", "Tit⟦le\n=====\n\nBody⟧", "le\n=====\n\nBody"},
	})
}

func TestExcerptLists(t *testing.T) {
	runExcerptCases(t, []excerptCase{
		{"inside one item", "- first ⟦item⟧\n- second", "item"},
		{"one item across lines", "- one ⟦two\n  three⟧ four", "two\nthree"},
		{"across items", "- fi⟦rst\n- sec⟧ond", "- rst\n- sec"},
		{"from the first letter", "- ⟦a\n- b⟧", "- a\n- b"},
		{"ordered numbers kept", "1. a\n2. b⟦b\n3. c⟧c", "2. b\n3. c"},
		{"nested siblings dedented", "- top\n  - a⟦a\n  - b⟧b", "- a\n- b"},
		{"nested into outer", "- top\n  - nes⟦ted\n- ne⟧xt", "- ted\n- ne"},
		{"deeply nested", "- a\n  - b\n    - c⟦c\n    - d⟧d", "- c\n- d"},
		{"continuation paragraph", "- item\n\n  more ⟦text\n\n- ne⟧xt", "text\n\n- ne"},
		{"inside one task", "- [ ] do ⟦this⟧", "this"},
		{"across tasks", "- [ ] do ⟦this\n- [x] do⟧ that", "- [ ] this\n- [x] do"},
	})
}

func TestExcerptQuotes(t *testing.T) {
	runExcerptCases(t, []excerptCase{
		{"inside", "> quoted ⟦words⟧ here", "words"},
		{"one paragraph across lines", "> one ⟦two\n> three⟧ four", "two\nthree"},
		{"across paragraphs", "> fi⟦rst\n>\n> sec⟧ond", "> rst\n>\n> sec"},
		{"list inside", "> - a⟦a\n> - b⟧b", "> - a\n> - b"},
		{"into a paragraph", "> quo⟦te\n\nafter⟧ text", "> te\n\nafter"},
		{"marker before an opener", "> **a⟦b** c\n\nd⟧", "> **b** c\n\nd"},
	})
}

func TestExcerptCodeBlocks(t *testing.T) {
	runExcerptCases(t, []excerptCase{
		{"inside, without fences", "```go\nfunc ⟦main() {\n\tfmt.Println()⟧\n}\n```", "main() {\n\tfmt.Println()"},
		{"indentation kept", "```py\ndef f():\n⟦    return 1\n    pass⟧\n```", "    return 1\n    pass"},
		{"into one", "Run ⟦this:\n\n```sh\nmake che⟧ck\n```", "this:\n\n```sh\nmake che\n```"},
		{"out of one", "```sh\nmake ⟦check\n```\n\nThen⟧ push", "```sh\ncheck\n```\n\nThen"},
		{"out of one from its first line", "```go\n⟦x := 1\n```\n\nText⟧", "```go\nx := 1\n```\n\nText"},
		{"out of one keeps indentation", "```py\ndef f():\n⟦    return 1\n```\n\nText⟧", "```py\n    return 1\n```\n\nText"},
		{"tilde fence", "Before ⟦x\n\n~~~\nco⟧de\n~~~", "x\n\n~~~\nco\n~~~"},
		// A shorter fence inside is content, so the closing fence must match
		// the opening one.
		{"longer fence", "Before ⟦x\n\n````md\n```\nin⟧ner\n```\n````", "x\n\n````md\n```\nin\n````"},
		{"inside a list item", "- step:\n\n  ```sh\n  make ⟦check\n  ```\n- ne⟧xt", "```sh\ncheck\n```\n- ne"},
		{"out of one in a quote", "> ```sh\n> make ⟦check\n> ```\n\nThen⟧ push", "> ```sh\n> check\n> ```\n\nThen"},
		{"into indented code", "Text ⟦here\n\n    code li⟧ne", "here\n\n    code li"},
		{"out of indented code", "    code ⟦line\n\nText⟧", "    line\n\nText"},
	})
}

func TestExcerptTables(t *testing.T) {
	runExcerptCases(t, []excerptCase{
		{"inside one cell", "| a | b |\n| - | - |\n| c⟦el⟧l | d |", "el"},
		{"emphasis in a cell", "| h |\n| - |\n| **bo⟦ld** x⟧ |", "**ld** x"},
		{"across cells", "| h1 | h2 |\n| --- | --- |\n| a⟦a | b⟧b |", "| h1 | h2 |\n| --- | --- |\n| aa | bb |"},
		{"across rows", "| h |\n| --- |\n| r⟦1 |\n| r⟧2 |\n| r3 |", "| h |\n| --- |\n| r1 |\n| r2 |"},
		{"header row", "| h⟦1 | h⟧2 |\n| --- | --- |\n| a | b |", "| h1 | h2 |\n| --- | --- |"},
		{"into one", "Te⟦xt\n\n| h |\n| --- |\n| r⟧1 |\n| r2 |", "xt\n\n| h |\n| --- |\n| r1 |"},
		{"out of one", "| h |\n| --- |\n| r⟦1 |\n\nAfter⟧ text", "| h |\n| --- |\n| r1 |\n\nAfter"},
	})
}

func TestExcerptAcrossBlocks(t *testing.T) {
	runExcerptCases(t, []excerptCase{
		{"paragraphs", "fi⟦rst\n\nsec⟧ond", "rst\n\nsec"},
		{"closed at both ends", "**a⟦b**\n\nc *d⟧e*", "**b**\n\nc *d*"},
		{"across a rule", "a⟦b\n\n---\n\nc⟧d", "b\n\n---\n\nc"},
	})
}

// A source map only hands over offsets in rendered text; these pin what
// happens to any other offset.
func TestExcerptOffsetsInSyntax(t *testing.T) {
	runExcerptCases(t, []excerptCase{
		{"start inside a delimiter", "a *⟦*bold** b⟧", "**bold** b"},
		{"end inside a delimiter", "⟦**bold*⟧* b", "**bold**"},
		{"end inside a destination", "⟦[a](https://x⟧.test) b", "[a](https://x.test)"},
	})
	for _, c := range []struct {
		start, end int
		want       string
	}{
		{-5, 99, "abc"},
		{2, 1, ""},
		{3, 3, ""},
	} {
		if got := Excerpt("abc", c.start, c.end); got != c.want {
			t.Errorf("Excerpt(%q, %d, %d) = %q, want %q", "abc", c.start, c.end, got, c.want)
		}
	}
}

// FuzzExcerpt checks that no document and no pair of offsets makes Excerpt
// panic: a selection can land anywhere in whatever a model writes.
func FuzzExcerpt(f *testing.F) {
	for _, seed := range []string{
		"# Title\n\nSome **bold _and em_** with `code` and [a link](https://x.test \"t\").\n\n- [ ] task\n  - nested `x`\n\n> quote\n> > deeper\n\n```go\nfunc main() {}\n```\n\n| a | b |\n| - | - |\n| **c** | d |\n\n---\n\n1. one\n2. two\n\n<https://x.test> &amp; \\* [ref][r] [short]\n\n[r]: https://r.test\n[short]: https://s.test",
		"``a ` b`` ~~s~~ ![i](j.png) <kbd>k</kbd> ***x***",
		"    indented\n\ntext\n~~~\nfence\n~~~",
	} {
		f.Add(seed, 0, len(seed))
		f.Add(seed, 5, 40)
	}
	f.Fuzz(func(t *testing.T, source string, start, end int) {
		Excerpt(source, start, end)
	})
}
