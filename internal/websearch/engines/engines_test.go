package engines

import (
	"context"
	"net/http"
	"net/http/httptest"
	"slices"
	"strings"
	"testing"

	"kon.kitsu.red/internal/websearch"
)

// TestEveryProviderHasAnEngine keeps the two tables in step: a name the
// config accepts must search, and an engine must be reachable from config.
func TestEveryProviderHasAnEngine(t *testing.T) {
	for _, name := range websearch.Names() {
		if engines[name] == nil {
			t.Errorf("provider %q has no engine", name)
		}
	}
	for name := range engines {
		if !slices.Contains(websearch.Names(), name) {
			t.Errorf("engine %q is missing from the websearch table", name)
		}
	}
}

func TestSearchRejectsBadInput(t *testing.T) {
	conn := websearch.Connection{APIKey: "key"}
	for _, test := range []struct {
		name, provider string
		conn           websearch.Connection
		query          websearch.Query
		want           string
	}{
		{"unknown provider", "bogus", conn, websearch.Query{Text: "x", Count: 1}, "supported: brave"},
		{"missing key", "brave", websearch.Connection{}, websearch.Query{Text: "x", Count: 1}, "brave requires api_key"},
		{"missing base URL", "searxng", websearch.Connection{}, websearch.Query{Text: "x", Count: 1}, "searxng requires base_url"},
		{"empty query", "brave", conn, websearch.Query{Text: " ", Count: 1}, "query must not be empty"},
		{"zero count", "brave", conn, websearch.Query{Text: "x"}, "count must be between"},
		{"count over the limit", "brave", conn, websearch.Query{Text: "x", Count: websearch.MaxCount + 1}, "count must be between"},
	} {
		_, err := Search(context.Background(), test.provider, test.conn, test.query)
		if err == nil || !strings.Contains(err.Error(), test.want) {
			t.Errorf("%s: error = %v, want it to contain %q", test.name, err, test.want)
		}
	}
}

func TestSearchTrimsToCountAndNamesTheProvider(t *testing.T) {
	status := http.StatusOK
	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(status)
		w.Write([]byte(`{"web": {"results": [{"title": "a", "url": "https://a.example"}, {"title": "b", "url": "https://b.example"}]}}`))
	}))
	defer server.Close()
	conn := websearch.Connection{APIKey: "key", BaseURL: server.URL + "/"}

	results, err := Search(context.Background(), "brave", conn, websearch.Query{Text: "x", Count: 1})
	if err != nil || len(results) != 1 || results[0].Title != "a" {
		t.Fatalf("results = %+v, err = %v, want only the first", results, err)
	}

	status = http.StatusUnauthorized
	_, err = Search(context.Background(), "brave", conn, websearch.Query{Text: "x", Count: 1})
	if err == nil || !strings.HasPrefix(err.Error(), "brave: 401") || !strings.Contains(err.Error(), "check its api_key") {
		t.Fatalf("error = %v, want the provider, the status, and a hint at the key", err)
	}
}
