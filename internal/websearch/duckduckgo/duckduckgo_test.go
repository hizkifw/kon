package duckduckgo

import (
	"context"
	"errors"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"

	"kon.kitsu.red/internal/websearch"
)

func serve(t *testing.T, handler http.HandlerFunc) websearch.Connection {
	t.Helper()
	server := httptest.NewServer(handler)
	t.Cleanup(server.Close)
	return websearch.Connection{BaseURL: server.URL}
}

// fixture is a page saved from html.duckduckgo.com, trimmed of the region
// menu and of all but the first results.
func fixture(t *testing.T, name string) []byte {
	t.Helper()
	page, err := os.ReadFile(filepath.Join("testdata", name))
	if err != nil {
		t.Fatal(err)
	}
	return page
}

// page wraps result blocks in the list DuckDuckGo serves them in.
func page(blocks string) []byte {
	return []byte(`<html><body><div class="serp__results"><div id="links" class="results">` + blocks + `</div></div></body></html>`)
}

func TestSearchSendsQueryAndReadsResults(t *testing.T) {
	results := fixture(t, "results.html")
	conn := serve(t, func(w http.ResponseWriter, r *http.Request) {
		if r.Method != http.MethodGet || r.URL.Path != "/html/" {
			t.Errorf("request = %s %s", r.Method, r.URL.Path)
		}
		if q := r.URL.Query(); q.Get("q") != "golang context package" || len(q) != 1 {
			t.Errorf("query = %v", q)
		}
		w.Write(results)
	})
	got, err := Search(context.Background(), conn, websearch.Query{Text: "golang context package", Count: 5})
	if err != nil {
		t.Fatal(err)
	}
	// The fixture opens with an ad, which is not a result.
	want := []websearch.Result{
		{
			Title:   "context package - context - Go Packages",
			URL:     "https://pkg.go.dev/context",
			Snippet: "Package context defines the Context type, which carries deadlines, cancellation signals, and other request-scoped values across API boundaries and between processes.",
			Date:    "2026-09-01",
		},
		{
			Title:   "Go Concurrency Patterns: Context - The Go Programming Language",
			URL:     "https://go.dev/blog/context",
			Snippet: "An introduction to the Go context package.",
		},
		{
			Title:   "context - The Go Programming Language",
			URL:     "https://golangdoc.github.io/pkg/1.12/context/index.html",
			Snippet: "Overview Package context defines the Context type, which carries deadlines, cancelation signals, and other request-scoped values across API boundaries and between processes. Incoming requests to a server should create a Context, and outgoing calls to servers should accept a Context.",
		},
	}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("results = %+v, want %+v", got, want)
	}
}

func TestSearchStopsAtCount(t *testing.T) {
	results := fixture(t, "results.html")
	conn := serve(t, func(w http.ResponseWriter, r *http.Request) { w.Write(results) })
	got, err := Search(context.Background(), conn, websearch.Query{Text: "golang context package", Count: 2})
	if err != nil {
		t.Fatal(err)
	}
	if len(got) != 2 || got[1].URL != "https://go.dev/blog/context" {
		t.Fatalf("results = %+v, want the first two", got)
	}
}

// TestSearchResolvesLinks covers the hrefs the fixture has none of: a direct
// link, a target with its own query, and an ad without the ad class.
func TestSearchResolvesLinks(t *testing.T) {
	conn := serve(t, func(w http.ResponseWriter, r *http.Request) {
		w.Write(page(`
			<div class="result web-result"><a class="result__a" href="https://example.com/direct">Direct</a></div>
			<div class="result web-result"><a class="result__a" href="//duckduckgo.com/l/?uddg=https%3A%2F%2Fexample.com%2Fsearch%3Fq%3Da%2520b%26page%3D2&amp;rut=abc">Redirected &amp; <b>bold</b></a>
				<a class="result__snippet" href="#">Tom &amp; Jerry, <b>1 &lt; 2</b></a></div>
			<div class="result web-result"><a class="result__a" href="//duckduckgo.com/l/?uddg=https%3A%2F%2Fduckduckgo.com%2Fy.js%3Fad_domain%3Dexample.com&amp;rut=abc">Unmarked ad</a></div>
			<div class="result web-result"><a class="result__a" href="https://duckduckgo.com/y.js?ad_domain=example.com">Direct ad</a></div>
			<div class="result web-result"><a class="result__snippet" href="#">No link</a></div>`))
	})
	got, err := Search(context.Background(), conn, websearch.Query{Text: "x", Count: 5})
	if err != nil {
		t.Fatal(err)
	}
	want := []websearch.Result{
		{Title: "Direct", URL: "https://example.com/direct"},
		{Title: "Redirected & bold", URL: "https://example.com/search?q=a%20b&page=2", Snippet: "Tom & Jerry, 1 < 2"},
	}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("results = %+v, want %+v", got, want)
	}
}

// TestSearchWithoutResults covers a page with nothing on it, which is no
// results rather than an error.
func TestSearchWithoutResults(t *testing.T) {
	conn := serve(t, func(w http.ResponseWriter, r *http.Request) {
		w.Write(page(`<div class="no-results">No results found for <b>zxqv</b>.</div>`))
	})
	got, err := Search(context.Background(), conn, websearch.Query{Text: "zxqv", Count: 5})
	if err != nil || got == nil || len(got) != 0 {
		t.Fatalf("results = %#v, err = %v, want none", got, err)
	}
}

// TestSearchReportsChallenge covers the bot check, which arrives as a 202
// and so not as a StatusError.
func TestSearchReportsChallenge(t *testing.T) {
	challenge := fixture(t, "challenge.html")
	conn := serve(t, func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusAccepted)
		w.Write(challenge)
	})
	got, err := Search(context.Background(), conn, websearch.Query{Text: "x", Count: 5})
	if !errors.Is(err, errChallenge) || got != nil {
		t.Fatalf("results = %+v, err = %v, want the challenge error", got, err)
	}
	if !strings.Contains(err.Error(), "refused the automated request") {
		t.Fatalf("error = %q", err)
	}
}

func TestSearchReportsRefusal(t *testing.T) {
	conn := serve(t, func(w http.ResponseWriter, r *http.Request) {
		http.Error(w, "forbidden", http.StatusForbidden)
	})
	_, err := Search(context.Background(), conn, websearch.Query{Text: "x", Count: 1})
	var status *websearch.StatusError
	if !errors.As(err, &status) || status.Code != http.StatusForbidden {
		t.Fatalf("error = %v, want a 403 StatusError", err)
	}
}
