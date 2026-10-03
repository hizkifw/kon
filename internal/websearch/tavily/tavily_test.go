package tavily

import (
	"context"
	"encoding/json"
	"errors"
	"net/http"
	"net/http/httptest"
	"reflect"
	"testing"

	"kon.kitsu.red/internal/websearch"
)

func serve(t *testing.T, handler http.HandlerFunc) websearch.Connection {
	t.Helper()
	server := httptest.NewServer(handler)
	t.Cleanup(server.Close)
	return websearch.Connection{APIKey: "secret", BaseURL: server.URL}
}

func TestSearchSendsQueryAndReadsResults(t *testing.T) {
	conn := serve(t, func(w http.ResponseWriter, r *http.Request) {
		if r.Method != http.MethodPost || r.URL.Path != "/search" {
			t.Errorf("request = %s %s", r.Method, r.URL.Path)
		}
		if got := r.Header.Get("Authorization"); got != "Bearer secret" {
			t.Errorf("Authorization = %q", got)
		}
		var body map[string]any
		if err := json.NewDecoder(r.Body).Decode(&body); err != nil {
			t.Errorf("decode request: %v", err)
		}
		want := map[string]any{"query": "go generics", "max_results": float64(3), "search_depth": "basic", "include_published_date": true}
		if !reflect.DeepEqual(body, want) {
			t.Errorf("body = %v, want %v", body, want)
		}
		w.Write([]byte(`{"query": "go generics", "answer": null, "results": [
			{"title": "Generics & Go", "url": "https://go.dev/doc/generics", "content": "Use List<T> [...] an intro.", "score": 0.9, "published_date": "Wed, 01 May 2024 12:00:00 GMT"},
			{"title": "ISO date", "url": "https://example.com/iso", "content": "", "published_date": "2024-06-02T08:00:00Z"},
			{"title": "Undated", "url": "https://example.com/", "content": "", "published_date": null}
		]}`))
	})
	got, err := Search(context.Background(), conn, websearch.Query{Text: "go generics", Count: 3})
	if err != nil {
		t.Fatal(err)
	}
	want := []websearch.Result{
		{Title: "Generics & Go", URL: "https://go.dev/doc/generics", Snippet: "Use List<T> [...] an intro.", Date: "2024-05-01"},
		{Title: "ISO date", URL: "https://example.com/iso", Date: "2024-06-02"},
		{Title: "Undated", URL: "https://example.com/"},
	}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("results = %+v, want %+v", got, want)
	}
}

// TestSearchKeepsToCount covers a server that returns more than it was asked
// for, which an engine must not pass on.
func TestSearchKeepsToCount(t *testing.T) {
	conn := serve(t, func(w http.ResponseWriter, r *http.Request) {
		w.Write([]byte(`{"results": [{"title": "One", "url": "https://example.com/1"}, {"title": "Two", "url": "https://example.com/2"}]}`))
	})
	got, err := Search(context.Background(), conn, websearch.Query{Text: "x", Count: 1})
	want := []websearch.Result{{Title: "One", URL: "https://example.com/1"}}
	if err != nil || !reflect.DeepEqual(got, want) {
		t.Fatalf("results = %+v, err = %v, want %+v", got, err, want)
	}
}

func TestSearchWithoutResults(t *testing.T) {
	conn := serve(t, func(w http.ResponseWriter, r *http.Request) {
		w.Write([]byte(`{"query": "zxqv", "results": []}`))
	})
	got, err := Search(context.Background(), conn, websearch.Query{Text: "zxqv", Count: 5})
	if err != nil || len(got) != 0 {
		t.Fatalf("results = %+v, err = %v, want none", got, err)
	}
}

func TestSearchReportsRefusal(t *testing.T) {
	conn := serve(t, func(w http.ResponseWriter, r *http.Request) {
		http.Error(w, `{"detail": {"error": "Unauthorized: missing or invalid API key."}}`, http.StatusUnauthorized)
	})
	_, err := Search(context.Background(), conn, websearch.Query{Text: "x", Count: 1})
	var status *websearch.StatusError
	if !errors.As(err, &status) || status.Code != http.StatusUnauthorized {
		t.Fatalf("error = %v, want a 401 StatusError", err)
	}
}
