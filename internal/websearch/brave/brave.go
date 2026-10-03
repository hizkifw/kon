// Package brave searches with the Brave Search API.
package brave

import (
	"context"
	"net/url"
	"strconv"

	"kon.kitsu.red/internal/websearch"
)

// response is the part of Brave's web search response kon reads.
type response struct {
	Web struct {
		Results []struct {
			Title       string `json:"title"`
			URL         string `json:"url"`
			Description string `json:"description"`
			PageAge     string `json:"page_age"`
		} `json:"results"`
	} `json:"web"`
}

// Search implements websearch.Engine.
func Search(ctx context.Context, conn websearch.Connection, q websearch.Query) ([]websearch.Result, error) {
	query := url.Values{
		"q":     {q.Text},
		"count": {strconv.Itoa(q.Count)},
		// Brave otherwise wraps the matched words in <strong> tags.
		"text_decorations": {"false"},
	}
	var resp response
	if err := conn.GetJSON(ctx, "/res/v1/web/search", query, map[string]string{"X-Subscription-Token": conn.APIKey}, &resp); err != nil {
		return nil, err
	}
	results := make([]websearch.Result, 0, len(resp.Web.Results))
	for _, hit := range resp.Web.Results {
		results = append(results, websearch.Result{
			// Titles and descriptions arrive with HTML entities, and
			// sometimes tags despite text_decorations.
			Title:   websearch.Plain(hit.Title),
			URL:     hit.URL,
			Snippet: websearch.Plain(hit.Description),
			Date:    websearch.Day(hit.PageAge),
		})
	}
	return results, nil
}
