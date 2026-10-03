package brave

import (
	"context"
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
		if r.Method != http.MethodGet || r.URL.Path != "/res/v1/web/search" {
			t.Errorf("request = %s %s", r.Method, r.URL.Path)
		}
		if got := r.Header.Get("X-Subscription-Token"); got != "secret" {
			t.Errorf("X-Subscription-Token = %q", got)
		}
		if q := r.URL.Query(); q.Get("q") != "go generics" || q.Get("count") != "3" {
			t.Errorf("query = %v", q)
		}
		w.Write([]byte(`{"web": {"results": [
			{"title": "Generics &amp; Go", "url": "https://go.dev/doc/generics", "description": "An <strong>intro</strong> to generics.", "page_age": "2024-05-01T12:00:00"},
			{"title": "Undated", "url": "https://example.com/", "description": "", "age": "3 days ago"}
		]}}`))
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

// TestSearchWithoutWebResults covers a query Brave answers with no "web"
// section at all, which is no results rather than an error.
func TestSearchWithoutWebResults(t *testing.T) {
	conn := serve(t, func(w http.ResponseWriter, r *http.Request) {
		w.Write([]byte(`{"type": "search"}`))
	})
	got, err := Search(context.Background(), conn, websearch.Query{Text: "zxqv", Count: 5})
	if err != nil || len(got) != 0 {
		t.Fatalf("results = %+v, err = %v, want none", got, err)
	}
}

func TestSearchReportsRefusal(t *testing.T) {
	conn := serve(t, func(w http.ResponseWriter, r *http.Request) {
		http.Error(w, `{"error": {"detail": "invalid token"}}`, http.StatusUnauthorized)
	})
	_, err := Search(context.Background(), conn, websearch.Query{Text: "x", Count: 1})
	var status *websearch.StatusError
	if !errors.As(err, &status) || status.Code != http.StatusUnauthorized {
		t.Fatalf("error = %v, want a 401 StatusError", err)
	}
}
