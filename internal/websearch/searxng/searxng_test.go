package searxng

import (
	"context"
	"errors"
	"net/http"
	"net/http/httptest"
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

func TestSearchSendsQueryAndReadsResults(t *testing.T) {
	conn := serve(t, func(w http.ResponseWriter, r *http.Request) {
		if r.Method != http.MethodGet || r.URL.Path != "/search" {
			t.Errorf("request = %s %s", r.Method, r.URL.Path)
		}
		if got, ok := r.Header["Authorization"]; ok {
			t.Errorf("Authorization = %q, want none without an API key", got)
		}
		if q := r.URL.Query(); q.Get("q") != "go generics" || q.Get("format") != "json" {
			t.Errorf("query = %v", q)
		}
		w.Write([]byte(`{"query": "go generics", "results": [
			{"title": "Generics & Go", "url": "https://go.dev/doc/generics", "content": "An intro to generics.", "publishedDate": "2024-05-01T12:00:00", "engine": "google"},
			{"title": "Undated", "url": "https://example.com/", "content": "", "publishedDate": null}
		]}`))
	})
	got, err := Search(context.Background(), conn, websearch.Query{Text: "go generics", Count: 3})
	if err != nil {
		t.Fatal(err)
	}
	want := []websearch.Result{
		{Title: "Generics & Go", URL: "https://go.dev/doc/generics", Snippet: "An intro to generics.", Date: "2024-05-01"},
		{Title: "Undated", URL: "https://example.com/"},
	}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("results = %+v, want %+v", got, want)
	}
}

// TestSearchKeepsCountResults covers the cut SearXNG leaves to its caller: a
// page is as long as the instance's engines made it.
func TestSearchKeepsCountResults(t *testing.T) {
	conn := serve(t, func(w http.ResponseWriter, r *http.Request) {
		w.Write([]byte(`{"results": [
			{"title": "One", "url": "https://example.com/1"},
			{"title": "Two", "url": "https://example.com/2"},
			{"title": "Three", "url": "https://example.com/3"}
		]}`))
	})
	got, err := Search(context.Background(), conn, websearch.Query{Text: "x", Count: 2})
	if err != nil {
		t.Fatal(err)
	}
	want := []websearch.Result{
		{Title: "One", URL: "https://example.com/1"},
		{Title: "Two", URL: "https://example.com/2"},
	}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("results = %+v, want %+v", got, want)
	}
}

func TestSearchWithoutResults(t *testing.T) {
	conn := serve(t, func(w http.ResponseWriter, r *http.Request) {
		w.Write([]byte(`{"query": "zxqv", "results": [], "unresponsive_engines": []}`))
	})
	got, err := Search(context.Background(), conn, websearch.Query{Text: "zxqv", Count: 5})
	if err != nil || len(got) != 0 {
		t.Fatalf("results = %+v, err = %v, want none", got, err)
	}
}

func TestSearchSendsAPIKeyAsBearerToken(t *testing.T) {
	conn := serve(t, func(w http.ResponseWriter, r *http.Request) {
		if got := r.Header.Get("Authorization"); got != "Bearer secret" {
			t.Errorf("Authorization = %q", got)
		}
		w.Write([]byte(`{"results": []}`))
	})
	conn.APIKey = "secret"
	if _, err := Search(context.Background(), conn, websearch.Query{Text: "x", Count: 1}); err != nil {
		t.Fatal(err)
	}
}

// TestSearchExplainsDisabledJSON covers a stock instance, which refuses the
// JSON format with a 403 that says nothing about why.
func TestSearchExplainsDisabledJSON(t *testing.T) {
	conn := serve(t, func(w http.ResponseWriter, r *http.Request) {
		http.Error(w, "Forbidden", http.StatusForbidden)
	})
	_, err := Search(context.Background(), conn, websearch.Query{Text: "x", Count: 1})
	var status *websearch.StatusError
	if !errors.As(err, &status) || status.Code != http.StatusForbidden {
		t.Fatalf("error = %v, want a 403 StatusError", err)
	}
	if !strings.Contains(err.Error(), "search.formats") {
		t.Fatalf("error = %v, want a hint about search.formats", err)
	}
}

func TestSearchReportsRefusal(t *testing.T) {
	conn := serve(t, func(w http.ResponseWriter, r *http.Request) {
		http.Error(w, "Too Many Requests", http.StatusTooManyRequests)
	})
	_, err := Search(context.Background(), conn, websearch.Query{Text: "x", Count: 1})
	var status *websearch.StatusError
	if !errors.As(err, &status) || status.Code != http.StatusTooManyRequests {
		t.Fatalf("error = %v, want a 429 StatusError", err)
	}
	if strings.Contains(err.Error(), "search.formats") {
		t.Fatalf("error = %v, want no format hint", err)
	}
}
