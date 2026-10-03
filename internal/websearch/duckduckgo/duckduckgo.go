// Package duckduckgo searches with DuckDuckGo. DuckDuckGo has no search API,
// so the engine reads the result page it serves to browsers without
// JavaScript.
package duckduckgo

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"net/url"
	"strings"

	"golang.org/x/net/html"
	"golang.org/x/net/html/atom"

	"kon.kitsu.red/internal/websearch"
)

// errChallenge is DuckDuckGo answering with its bot check instead of results.
// The page is a 202 with no results on it, which would otherwise read as a
// query that matched nothing.
var errChallenge = errors.New("DuckDuckGo refused the automated request with a bot challenge; try again later or use another provider")

// Search implements websearch.Engine.
func Search(ctx context.Context, conn websearch.Connection, q websearch.Query) ([]websearch.Result, error) {
	body, err := conn.Get(ctx, "/html/", url.Values{"q": {q.Text}}, nil)
	if err != nil {
		return nil, err
	}
	doc, err := html.Parse(bytes.NewReader(body))
	if err != nil {
		return nil, fmt.Errorf("parse response: %w", err)
	}
	if find(doc, isChallenge) != nil {
		return nil, errChallenge
	}
	results := []websearch.Result{}
	for n := range doc.Descendants() {
		if len(results) == q.Count {
			break
		}
		if !hasClass(n, "result") || hasClass(n, "result--ad") {
			continue
		}
		if result, ok := read(n); ok {
			results = append(results, result)
		}
	}
	return results, nil
}

// isChallenge reports whether n belongs to the bot check, whose elements all
// carry an anomaly-modal class.
func isChallenge(n *html.Node) bool {
	for _, class := range classes(n) {
		if strings.HasPrefix(class, "anomaly-modal") {
			return true
		}
	}
	return false
}

// read turns one result block into a Result. It reports false for a block
// without a usable link, and for an ad that is not marked as one.
func read(block *html.Node) (websearch.Result, bool) {
	link := find(block, func(n *html.Node) bool { return n.DataAtom == atom.A && hasClass(n, "result__a") })
	if link == nil {
		return websearch.Result{}, false
	}
	target := resolve(attr(link, "href"))
	if target == "" {
		return websearch.Result{}, false
	}
	result := websearch.Result{Title: text(link), URL: target}
	if snippet := find(block, func(n *html.Node) bool { return hasClass(n, "result__snippet") }); snippet != nil {
		result.Snippet = text(snippet)
	}
	// Some results print a timestamp in a bare span after the display URL.
	if extras := find(block, func(n *html.Node) bool { return hasClass(n, "result__extras__url") }); extras != nil {
		for child := range extras.ChildNodes() {
			if child.DataAtom == atom.Span && result.Date == "" {
				result.Date = websearch.Day(text(child))
			}
		}
	}
	return result, true
}

// resolve returns the page a result links to, or "" for an ad. DuckDuckGo
// links through its own redirect, which carries the target in uddg; ads
// redirect once more through y.js.
func resolve(href string) string {
	u, err := url.Parse(strings.TrimSpace(href))
	if err != nil || u.Host == "" {
		return ""
	}
	if u.Scheme == "" {
		u.Scheme = "https"
	}
	if !isDuckDuckGo(u) {
		return u.String()
	}
	switch u.Path {
	case "/y.js":
		return ""
	case "/l/":
		if target := u.Query().Get("uddg"); target != "" {
			return resolve(target)
		}
	}
	return u.String()
}

func isDuckDuckGo(u *url.URL) bool {
	host := strings.ToLower(u.Hostname())
	return host == "duckduckgo.com" || strings.HasSuffix(host, ".duckduckgo.com")
}

// find returns the first node at or below root that match accepts.
func find(root *html.Node, match func(*html.Node) bool) *html.Node {
	if match(root) {
		return root
	}
	for n := range root.Descendants() {
		if match(n) {
			return n
		}
	}
	return nil
}

// text is the visible text of n on one line, with the bold tags DuckDuckGo
// puts around matched words dropped.
func text(n *html.Node) string {
	var out strings.Builder
	for d := range n.Descendants() {
		if d.Type == html.TextNode {
			out.WriteString(d.Data)
		}
	}
	return strings.Join(strings.Fields(out.String()), " ")
}

func hasClass(n *html.Node, class string) bool {
	for _, c := range classes(n) {
		if c == class {
			return true
		}
	}
	return false
}

func classes(n *html.Node) []string {
	if n.Type != html.ElementNode {
		return nil
	}
	return strings.Fields(attr(n, "class"))
}

func attr(n *html.Node, key string) string {
	for _, a := range n.Attr {
		if a.Key == key {
			return a.Val
		}
	}
	return ""
}
