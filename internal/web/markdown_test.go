package web

import (
	"net/url"
	"strings"
	"testing"

	"golang.org/x/net/html"
)

func render(t *testing.T, page string) string {
	t.Helper()
	doc, err := html.Parse(strings.NewReader(page))
	if err != nil {
		t.Fatal(err)
	}
	base, err := url.Parse("https://example.test/docs/page")
	if err != nil {
		t.Fatal(err)
	}
	return Markdown(doc, base)
}

func TestMarkdown(t *testing.T) {
	tests := []struct {
		name, html, want string
	}{
		{
			name: "headings and paragraphs collapse whitespace",
			html: "<h1>  Title\n</h1><p>one\n   two</p><h3>Sub</h3><p>three</p>",
			want: "# Title\n\none two\n\n### Sub\n\nthree",
		},
		{
			name: "inline markup keeps its spaces outside",
			html: "<p>a<b> bold </b>and <em>it</em>, <code> x  y</code>.</p>",
			want: "a **bold** and *it*, `x y`.",
		},
		{
			name: "empty markup leaves no bare markers",
			html: "<p>a <b> </b><i></i>b</p>",
			want: "a b",
		},
		{
			name: "links within the site print as paths from its root",
			html: `<p><a href="/api">API</a> <a href="guide?v=2#use">guide</a> <a href="https://example.test">home</a></p>`,
			want: "[API](/api) [guide](/docs/guide?v=2#use) [home](/)",
		},
		{
			name: "links to another site or scheme stay absolute",
			html: `<p><a href="https://x.test/">https://x.test/</a> <a href="http://example.test/a">plain</a> <a href="mailto:me@example.test">mail</a></p>`,
			want: "https://x.test/ [plain](http://example.test/a) [mail](mailto:me@example.test)",
		},
		{
			name: "links that lead nowhere keep only their text",
			html: `<p><a href="#top">top</a> <a href="javascript:void(0)">run</a> <a href="/icon"><span></span></a>end</p>`,
			want: "top run end",
		},
		{
			name: "code that holds backticks gets a longer fence",
			html: "<p><code>a`b</code></p><pre class=\"language-go\"><code>x := 1\n```\n</code></pre>",
			want: "`` a`b ``\n\n````go\nx := 1\n```\n````",
		},
		{
			name: "language class on the code element",
			html: "<pre><code class=\"hl lang-sh\">\nls -l\n</code></pre>",
			want: "```sh\nls -l\n```",
		},
		{
			name: "nested and ordered lists indent continuations",
			html: `<ul><li>one<ul><li>inner</li></ul></li><li><p>two</p><p>more</p></li></ul><ol start="3"><li>c</li><li>d</li></ol>`,
			want: "- one\n  - inner\n- two\n  more\n\n3. c\n4. d",
		},
		{
			name: "blockquote",
			html: "<blockquote><p>a</p><p>b</p></blockquote>",
			want: "> a\n>\n> b",
		},
		{
			name: "data table takes its first row as header",
			html: "<table><tr><th>k</th><th>v</th></tr><tr><td>a|b</td></tr></table>",
			want: "| k | v |\n| --- | --- |\n| a\\|b |  |",
		},
		{
			name: "layout table renders its cells as blocks",
			html: "<table><tr><td><h2>Head</h2><table><tr><td>x</td></tr></table></td></tr></table>",
			want: "## Head\n\n| x |\n| --- |",
		},
		{
			name: "breaks within a paragraph",
			html: "<p>line one<br>  line two</p>",
			want: "line one\nline two",
		},
		{
			name: "images show their alt text and drop inline data",
			html: `<p><img src="/a.png" alt="diagram"> <img src="data:image/png;base64,AA" alt="inline"> <img src="/deco.png" alt=""></p>`,
			want: "![diagram](/a.png) inline",
		},
		{
			name: "block inside a link is set off by spaces",
			html: `<a href="/post"><h3>Post</h3><div>Summary</div></a>`,
			want: "[Post Summary](/post)",
		},
		{
			name: "custom elements are blocks",
			html: "<x-frame><h1>Title</h1><p>body</p></x-frame>",
			want: "# Title\n\nbody",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := render(t, tt.html); got != tt.want {
				t.Fatalf("got:\n%s\nwant:\n%s", got, tt.want)
			}
		})
	}
}

// TestMarkdownDropsOnlyWhatRendersNoText guards the split between what a
// reader never needs (scripts, controls, and decoration) and what it does:
// the site's navigation, and content hidden until a click, like a tab.
func TestMarkdownDropsOnlyWhatRendersNoText(t *testing.T) {
	got := render(t, `<html><head><title>T</title><style>p{}</style></head><body>
		<header><nav><a href="/">Home</a> <a href="/blog">Blog</a></nav></header>
		<article>
			<h1>Post title</h1>
			<p>Body<script>track()</script> text <button>Copy</button><span aria-hidden="true">#</span></p>
			<div hidden>Inactive tab</div>
		</article>
		<footer>Copyright</footer>
	</body></html>`)
	want := "[Home](/) [Blog](/blog)\n\n# Post title\n\nBody text\n\nInactive tab\n\nCopyright"
	if got != want {
		t.Fatalf("got:\n%s\nwant:\n%s", got, want)
	}
}

func TestMarkdownPrefersMainContent(t *testing.T) {
	got := render(t, `<body><div>Banner</div><div role="main"><p>Content</p></div><div>After</div></body>`)
	if got != "Content" {
		t.Fatalf("got %q, want only the main content", got)
	}
}
