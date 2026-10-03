// Package tavily searches with the Tavily Search API.
package tavily

import (
	"context"
	"net/mail"
	"time"

	"kon.kitsu.red/internal/websearch"
)

// request is the part of Tavily's search request kon sets. The generated
// answer and raw page content stay at their defaults, which is off.
type request struct {
	Query      string `json:"query"`
	MaxResults int    `json:"max_results"`
	// SearchDepth is sent rather than left to the default so that a search
	// keeps costing one credit if Tavily changes what it defaults to.
	SearchDepth string `json:"search_depth"`
	// Tavily otherwise dates only the results of a news search.
	IncludePublishedDate bool `json:"include_published_date"`
}

// response is the part of Tavily's search response kon reads.
type response struct {
	Results []struct {
		Title         string `json:"title"`
		URL           string `json:"url"`
		Content       string `json:"content"`
		PublishedDate string `json:"published_date"`
	} `json:"results"`
}

// Search implements websearch.Engine.
func Search(ctx context.Context, conn websearch.Connection, q websearch.Query) ([]websearch.Result, error) {
	req := request{Query: q.Text, MaxResults: q.Count, SearchDepth: "basic", IncludePublishedDate: true}
	var resp response
	if err := conn.PostJSON(ctx, "/search", map[string]string{"Authorization": "Bearer " + conn.APIKey}, req, &resp); err != nil {
		return nil, err
	}
	hits := resp.Results
	if len(hits) > q.Count {
		hits = hits[:q.Count]
	}
	results := make([]websearch.Result, 0, len(hits))
	for _, hit := range hits {
		results = append(results, websearch.Result{
			Title:   hit.Title,
			URL:     hit.URL,
			Snippet: hit.Content,
			Date:    day(hit.PublishedDate),
		})
	}
	return results, nil
}

// day reduces Tavily's published date to YYYY-MM-DD. Tavily documents it as
// an HTTP-style date, "Tue, 11 Mar 2025 17:00:00 GMT", which websearch.Day
// does not read; that remains the fallback in case a result carries an ISO
// date instead.
func day(timestamp string) string {
	if parsed, err := mail.ParseDate(timestamp); err == nil {
		return parsed.Format(time.DateOnly)
	}
	return websearch.Day(timestamp)
}
