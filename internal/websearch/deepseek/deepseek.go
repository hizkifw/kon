// Package deepseek searches with the DeepSeek Search API.
package deepseek

import (
	"context"

	"kon.kitsu.red/internal/websearch"
)

// request is the part of DeepSeek's search request kon sets. Page content
// stays at its default, which is off, since webfetch reads the pages chosen.
type request struct {
	Query string `json:"query"`
	Count int    `json:"count"`
}

// response is the part of DeepSeek's search response kon reads. Its longer
// summary is raw excerpts of the page, so the short snippet describes a
// result better within the bytes a snippet prints.
type response struct {
	Results []struct {
		Title   string `json:"title"`
		URL     string `json:"url"`
		Snippet string `json:"snippet"`
		Date    string `json:"date"`
	} `json:"results"`
}

// Search implements websearch.Engine.
func Search(ctx context.Context, conn websearch.Connection, q websearch.Query) ([]websearch.Result, error) {
	var resp response
	if err := conn.PostJSON(ctx, "/v1/tools/search", map[string]string{"Authorization": "Bearer " + conn.APIKey}, request{Query: q.Text, Count: q.Count}, &resp); err != nil {
		return nil, err
	}
	results := make([]websearch.Result, 0, len(resp.Results))
	for _, hit := range resp.Results {
		if len(results) == q.Count {
			break
		}
		results = append(results, websearch.Result{
			Title:   hit.Title,
			URL:     hit.URL,
			Snippet: hit.Snippet,
			Date:    websearch.Day(hit.Date),
		})
	}
	return results, nil
}
