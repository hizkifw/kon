package websearch

import (
	"context"
	"errors"
	"net/http"
	"net/http/httptest"
	"net/url"
	"strings"
	"testing"

	"kon.kitsu.red/internal/buildinfo"
)

func TestConnectAppliesDefaultsAndRequirements(t *testing.T) {
	keyed := Spec{DefaultBaseURL: "https://api.example", KeyRequired: true}
	conn, err := keyed.Connect(Connection{APIKey: "key"})
	if err != nil || conn.BaseURL != "https://api.example" {
		t.Fatalf("conn = %+v, err = %v, want the default base URL", conn, err)
	}
	if conn, _ := keyed.Connect(Connection{APIKey: "key", BaseURL: " http://localhost:8080/ "}); conn.BaseURL != "http://localhost:8080" {
		t.Fatalf("base URL = %q, want it trimmed", conn.BaseURL)
	}
	if _, err := keyed.Connect(Connection{}); err == nil || !strings.Contains(err.Error(), "api_key") {
		t.Fatalf("error = %v, want a missing api_key", err)
	}
	if _, err := (Spec{}).Connect(Connection{}); err == nil || !strings.Contains(err.Error(), "base_url") {
		t.Fatalf("error = %v, want a missing base_url", err)
	}
}

func TestWriteFormatsResults(t *testing.T) {
	var out strings.Builder
	err := Write(&out, []Result{
		{Title: "First\n page", URL: "https://a.example/", Snippet: "About  a.", Date: "2024-05-01"},
		{URL: "https://b.example/"},
		{Title: "Third", URL: "https://c.example/", Snippet: strings.Repeat("é", maxSnippetBytes)},
	})
	if err != nil {
		t.Fatal(err)
	}
	want := "1. First page\n   https://a.example/\n   2024-05-01 - About a.\n\n" +
		"2. https://b.example/\n   https://b.example/\n\n" +
		"3. Third\n   https://c.example/\n   " + strings.Repeat("é", maxSnippetBytes/2) + "…\n"
	if out.String() != want {
		t.Fatalf("output = %q, want %q", out.String(), want)
	}
}

func TestPlainAndDay(t *testing.T) {
	if got := Plain("Tom &amp; <strong>Jerry</strong>\n &lt;3"); got != "Tom & Jerry <3" {
		t.Fatalf("Plain = %q", got)
	}
	for timestamp, want := range map[string]string{
		"2024-05-01T12:00:00Z": "2024-05-01", "2024-05-01": "2024-05-01",
		"3 days ago": "", "2024-13-40T00:00:00": "", "": "",
	} {
		if got := Day(timestamp); got != want {
			t.Errorf("Day(%q) = %q, want %q", timestamp, got, want)
		}
	}
}

func TestRequestsCarryHeadersAndReportRefusals(t *testing.T) {
	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		switch r.URL.Path {
		case "/get":
			if r.Header.Get("User-Agent") != buildinfo.UserAgent() || r.Header.Get("X-Key") != "k" || r.URL.Query().Get("q") != "a b" {
				t.Errorf("GET headers = %v, query = %v", r.Header, r.URL.Query())
			}
			w.Write([]byte(`{"ok": true}`))
		case "/post":
			if r.Method != http.MethodPost || r.Header.Get("Content-Type") != "application/json" {
				t.Errorf("POST = %s %v", r.Method, r.Header)
			}
			w.Write([]byte(`{"ok": true}`))
		default:
			http.Error(w, "slow\n down", http.StatusTooManyRequests)
		}
	}))
	defer server.Close()
	conn := Connection{BaseURL: server.URL}

	var out struct{ OK bool }
	if err := conn.GetJSON(context.Background(), "/get", url.Values{"q": {"a b"}}, map[string]string{"X-Key": "k"}, &out); err != nil || !out.OK {
		t.Fatalf("GetJSON: out = %+v, err = %v", out, err)
	}
	out.OK = false
	if err := conn.PostJSON(context.Background(), "/post", nil, map[string]string{"query": "x"}, &out); err != nil || !out.OK {
		t.Fatalf("PostJSON: out = %+v, err = %v", out, err)
	}
	_, err := conn.Get(context.Background(), "/missing", nil, nil)
	var status *StatusError
	if !errors.As(err, &status) || status.Code != http.StatusTooManyRequests || status.Error() != "429 Too Many Requests: slow down" {
		t.Fatalf("error = %v, want a 429 StatusError quoting the body", err)
	}
}
