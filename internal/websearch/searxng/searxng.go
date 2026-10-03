// Package searxng searches with a self-hosted SearXNG instance.
package searxng

import (
	"context"
	"errors"
	"fmt"
	"net/http"
	"net/url"

	"kon.kitsu.red/internal/websearch"
)

// response is the part of SearXNG's JSON search response kon reads.
type response struct {
	Results []struct {
		Title   string `json:"title"`
		URL     string `json:"url"`
		Content string `json:"content"`
		// PublishedDate is null for most results.
		PublishedDate string `json:"publishedDate"`
	} `json:"results"`
}

// Search implements websearch.Engine.
func Search(ctx context.Context, conn websearch.Connection, q websearch.Query) ([]websearch.Result, error) {
	query := url.Values{
		"q":      {q.Text},
		"format": {"json"},
	}
	var headers map[string]string
	if conn.APIKey != "" {
		// SearXNG has no keys of its own; one is configured only for an
		// instance behind a proxy that asks for a token.
		headers = map[string]string{"Authorization": "Bearer " + conn.APIKey}
	}
	var resp response
	if err := conn.GetJSON(ctx, "/search", query, headers, &resp); err != nil {
		var status *websearch.StatusError
		if errors.As(err, &status) && status.Code == http.StatusForbidden {
			// Instances serve only HTML unless configured otherwise, and
			// refuse any other format with a bare 403.
			return nil, fmt.Errorf("%w (the instance must list json under search.formats in its settings.yml)", err)
		}
		return nil, err
	}
	// SearXNG has no count parameter: a page holds whatever its upstream
	// engines returned, so the cut happens here.
	results := make([]websearch.Result, 0, min(q.Count, len(resp.Results)))
	for _, hit := range resp.Results {
		if len(results) == q.Count {
			break
		}
		results = append(results, websearch.Result{
			Title:   hit.Title,
			URL:     hit.URL,
			Snippet: hit.Content,
			Date:    websearch.Day(hit.PublishedDate),
		})
	}
	return results, nil
}
