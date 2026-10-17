package deepseek

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
		if r.Method != http.MethodPost || r.URL.Path != "/v1/tools/search" {
			t.Errorf("request = %s %s", r.Method, r.URL.Path)
		}
		if got := r.Header.Get("Authorization"); got != "Bearer secret" {
			t.Errorf("Authorization = %q", got)
		}
		var body map[string]any
		if err := json.NewDecoder(r.Body).Decode(&body); err != nil {
			t.Errorf("decode request: %v", err)
		}
		want := map[string]any{"query": "go 1.25", "count": float64(2)}
		if !reflect.DeepEqual(body, want) {
			t.Errorf("body = %v, want %v", body, want)
		}
		w.Write([]byte(`{"results": [
			{"title": "Go 1.25 is released", "url": "https://go.dev/blog/go1.25", "snippet": "Today the Go team is pleased to release Go 1.25.", "summary": "#### Long excerpt", "content": "", "date": "2025-08-11T16:00:00+00:00", "authority": 4, "rerank_score": 2.2},
			{"title": "Go 1.25 Release Notes", "url": "https://go.dev/doc/go1.25", "snippet": "", "date": null, "authority": -1}
		]}`))
	})
	got, err := Search(context.Background(), conn, websearch.Query{Text: "go 1.25", Count: 2})
	if err != nil {
		t.Fatal(err)
	}
	want := []websearch.Result{
		{Title: "Go 1.25 is released", URL: "https://go.dev/blog/go1.25", Snippet: "Today the Go team is pleased to release Go 1.25.", Date: "2025-08-11"},
		{Title: "Go 1.25 Release Notes", URL: "https://go.dev/doc/go1.25"},
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
		w.Write([]byte(`{"results": []}`))
	})
	got, err := Search(context.Background(), conn, websearch.Query{Text: "zxqv", Count: 5})
	if err != nil || len(got) != 0 {
		t.Fatalf("results = %+v, err = %v, want none", got, err)
	}
}

func TestSearchReportsRefusal(t *testing.T) {
	conn := serve(t, func(w http.ResponseWriter, r *http.Request) {
		http.Error(w, `{"error": {"message": "Authentication Fails", "type": "authentication_error", "param": null, "code": "invalid_request_error"}}`, http.StatusUnauthorized)
	})
	_, err := Search(context.Background(), conn, websearch.Query{Text: "x", Count: 1})
	var status *websearch.StatusError
	if !errors.As(err, &status) || status.Code != http.StatusUnauthorized {
		t.Fatalf("error = %v, want a 401 StatusError", err)
	}
}
