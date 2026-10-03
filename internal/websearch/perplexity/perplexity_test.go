package perplexity

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
		want := map[string]any{"query": "go generics", "max_results": float64(3), "max_tokens_per_page": float64(snippetTokens)}
		if !reflect.DeepEqual(body, want) {
			t.Errorf("body = %v, want %v", body, want)
		}
		w.Write([]byte(`{"id": "abc", "server_time": null, "results": [
			{"title": "Generics & Go", "url": "https://go.dev/doc/generics", "snippet": "## Intro\nUse Vec<T> for generics.", "date": "2024-05-01", "last_updated": "2025-01-02"},
			{"title": "Undated", "url": "https://example.com/", "snippet": "", "date": null, "last_updated": "2025-01-02"}
		]}`))
	})
	got, err := Search(context.Background(), conn, websearch.Query{Text: "go generics", Count: 3})
	if err != nil {
		t.Fatal(err)
	}
	want := []websearch.Result{
		{Title: "Generics & Go", URL: "https://go.dev/doc/generics", Snippet: "## Intro\nUse Vec<T> for generics.", Date: "2024-05-01"},
		{Title: "Undated", URL: "https://example.com/"},
	}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("results = %+v, want %+v", got, want)
	}
}

// TestSearchKeepsToCount covers a server that returns more than it was asked
// for, which the engine contract does not allow through.
func TestSearchKeepsToCount(t *testing.T) {
	conn := serve(t, func(w http.ResponseWriter, r *http.Request) {
		w.Write([]byte(`{"results": [{"title": "A", "url": "https://a.example/"}, {"title": "B", "url": "https://b.example/"}]}`))
	})
	got, err := Search(context.Background(), conn, websearch.Query{Text: "x", Count: 1})
	want := []websearch.Result{{Title: "A", URL: "https://a.example/"}}
	if err != nil || !reflect.DeepEqual(got, want) {
		t.Fatalf("results = %+v, err = %v, want %+v", got, err, want)
	}
}

func TestSearchWithoutResults(t *testing.T) {
	conn := serve(t, func(w http.ResponseWriter, r *http.Request) {
		w.Write([]byte(`{"id": "abc", "results": []}`))
	})
	got, err := Search(context.Background(), conn, websearch.Query{Text: "zxqv", Count: 5})
	if err != nil || len(got) != 0 {
		t.Fatalf("results = %+v, err = %v, want none", got, err)
	}
}

func TestSearchReportsRefusal(t *testing.T) {
	conn := serve(t, func(w http.ResponseWriter, r *http.Request) {
		http.Error(w, `{"error": {"message": "invalid api key"}}`, http.StatusUnauthorized)
	})
	_, err := Search(context.Background(), conn, websearch.Query{Text: "x", Count: 1})
	var status *websearch.StatusError
	if !errors.As(err, &status) || status.Code != http.StatusUnauthorized {
		t.Fatalf("error = %v, want a 401 StatusError", err)
	}
}
