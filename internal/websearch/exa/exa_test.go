package exa

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
		if got := r.Header.Get("x-api-key"); got != "secret" {
			t.Errorf("x-api-key = %q", got)
		}
		var body map[string]any
		if err := json.NewDecoder(r.Body).Decode(&body); err != nil {
			t.Errorf("body: %v", err)
		}
		want := map[string]any{
			"query":      "go generics",
			"numResults": float64(2),
			"contents":   map[string]any{"highlights": map[string]any{"maxCharacters": float64(500)}},
		}
		if !reflect.DeepEqual(body, want) {
			t.Errorf("body = %v, want %v", body, want)
		}
		w.Write([]byte(`{"requestId": "abc", "results": [
			{"id": "1", "title": "Generics & Go", "url": "https://go.dev/doc/generics", "publishedDate": "2024-05-01T12:00:00.000Z", "author": null,
				"highlights": ["Type parameters\n  let a function take <T>.", " ", "Constraints limit them."], "highlightScores": [0.6, 0.1, 0.4]},
			{"id": "2", "title": "Undated", "url": "https://example.com/", "publishedDate": null},
			{"id": "3", "title": "Beyond the count", "url": "https://example.org/"}
		]}`))
	})
	got, err := Search(context.Background(), conn, websearch.Query{Text: "go generics", Count: 2})
	if err != nil {
		t.Fatal(err)
	}
	want := []websearch.Result{
		{Title: "Generics & Go", URL: "https://go.dev/doc/generics", Snippet: "Type parameters let a function take <T>. … Constraints limit them.", Date: "2024-05-01"},
		{Title: "Undated", URL: "https://example.com/"},
	}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("results = %+v, want %+v", got, want)
	}
}

func TestSearchWithoutResults(t *testing.T) {
	conn := serve(t, func(w http.ResponseWriter, r *http.Request) {
		w.Write([]byte(`{"requestId": "abc", "results": []}`))
	})
	got, err := Search(context.Background(), conn, websearch.Query{Text: "zxqv", Count: 5})
	if err != nil || len(got) != 0 {
		t.Fatalf("results = %+v, err = %v, want none", got, err)
	}
}

func TestSearchReportsRefusal(t *testing.T) {
	conn := serve(t, func(w http.ResponseWriter, r *http.Request) {
		http.Error(w, `{"error": "Invalid API key"}`, http.StatusUnauthorized)
	})
	_, err := Search(context.Background(), conn, websearch.Query{Text: "x", Count: 1})
	var status *websearch.StatusError
	if !errors.As(err, &status) || status.Code != http.StatusUnauthorized {
		t.Fatalf("error = %v, want a 401 StatusError", err)
	}
}
