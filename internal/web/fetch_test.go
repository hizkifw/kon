package web

import (
	"context"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"

	"kon.kitsu.red/internal/buildinfo"
)

func serve(t *testing.T, handler http.HandlerFunc) *httptest.Server {
	t.Helper()
	server := httptest.NewServer(handler)
	t.Cleanup(server.Close)
	return server
}

func TestFetchConvertsHTMLAndReportsRedirect(t *testing.T) {
	server := serve(t, func(w http.ResponseWriter, r *http.Request) {
		if r.URL.Path == "/old" {
			http.Redirect(w, r, "/new/page", http.StatusFound)
			return
		}
		if got := r.Header.Get("User-Agent"); got != buildinfo.UserAgent() {
			t.Errorf("User-Agent = %q, want %q", got, buildinfo.UserAgent())
		}
		w.Header().Set("Content-Type", "text/html; charset=utf-8")
		w.Write([]byte(`<h1>Hi</h1><p><a href="next">next</a></p>`))
	})
	page, err := Fetch(context.Background(), server.URL+"/old")
	if err != nil {
		t.Fatal(err)
	}
	want := "# Hi\n\n[next](/new/next)\n"
	if page.Text != want || page.Truncated || page.RedirectedTo != server.URL+"/new/page" {
		t.Fatalf("page = %+v, want text %q redirected to /new/page", page, want)
	}
}

func TestFetchPassesTextThrough(t *testing.T) {
	for _, contentType := range []string{"text/plain", "application/json", "application/vnd.api+json", "text/markdown; charset=utf-8", ""} {
		server := serve(t, func(w http.ResponseWriter, r *http.Request) {
			w.Header().Set("Content-Type", contentType)
			w.Write([]byte(`{"a": "<b>"}`))
		})
		page, err := Fetch(context.Background(), server.URL)
		if err != nil {
			t.Fatalf("%q: %v", contentType, err)
		}
		if page.Text != "{\"a\": \"<b>\"}\n" || page.RedirectedTo != "" {
			t.Fatalf("%q: page = %+v", contentType, page)
		}
	}
}

// TestFetchSniffsGenericBinary covers servers that label source files
// application/octet-stream: text is still printed, real binaries are not.
func TestFetchSniffsGenericBinary(t *testing.T) {
	body := "package main\n"
	server := serve(t, func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/octet-stream")
		w.Write([]byte(body))
	})
	page, err := Fetch(context.Background(), server.URL)
	if err != nil || page.Text != body {
		t.Fatalf("page = %+v, err = %v; want the source as text", page, err)
	}

	body = "\x89PNG\r\n\x1a\n\x00\x00"
	if _, err := Fetch(context.Background(), server.URL); err == nil || !strings.Contains(err.Error(), "image/png, not text") {
		t.Fatalf("err = %v, want a not-text error naming image/png", err)
	}
}

func TestFetchReportsHTTPErrors(t *testing.T) {
	server := serve(t, func(w http.ResponseWriter, r *http.Request) {
		http.Error(w, "gone", http.StatusNotFound)
	})
	_, err := Fetch(context.Background(), server.URL+"/missing")
	if err == nil || !strings.Contains(err.Error(), "404 Not Found") || !strings.Contains(err.Error(), "/missing") {
		t.Fatalf("err = %v, want the status and URL", err)
	}
}

func TestFetchExplainsScriptOnlyPages(t *testing.T) {
	server := serve(t, func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "text/html")
		w.Write([]byte(`<html><body><div id="root"></div><script src="app.js"></script></body></html>`))
	})
	_, err := Fetch(context.Background(), server.URL)
	if err == nil || !strings.Contains(err.Error(), "JavaScript") {
		t.Fatalf("err = %v, want a hint that the page needs JavaScript", err)
	}
}

func TestFetchTruncatesLongBodies(t *testing.T) {
	server := serve(t, func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "text/plain")
		w.Write([]byte(strings.Repeat("a", MaxBodyBytes+10)))
	})
	page, err := Fetch(context.Background(), server.URL)
	if err != nil {
		t.Fatal(err)
	}
	if !page.Truncated || len(page.Text) != MaxBodyBytes+1 {
		t.Fatalf("truncated = %v, len = %d; want the first %d bytes and a newline", page.Truncated, len(page.Text), MaxBodyBytes)
	}
}

func TestParseURL(t *testing.T) {
	for raw, want := range map[string]string{
		"example.test/a?b=1":    "https://example.test/a?b=1",
		" HTTP://example.test ": "http://example.test",
		"localhost:8080/x":      "https://localhost:8080/x",
	} {
		got, err := parseURL(raw)
		if err != nil || got.String() != want {
			t.Fatalf("parseURL(%q) = %v, %v; want %s", raw, got, err, want)
		}
	}
	for _, raw := range []string{"", "ftp://example.test/f", "file:///etc/passwd", "https://"} {
		if _, err := parseURL(raw); err == nil {
			t.Fatalf("parseURL(%q) succeeded, want an error", raw)
		}
	}
}
