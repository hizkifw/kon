// Package perplexity searches with the Perplexity Search API.
package perplexity

import (
	"context"

	"kon.kitsu.red/internal/websearch"
)

// snippetTokens caps how much of each page Perplexity extracts. Its snippets
// are otherwise whole passages of the page, which a search result never shows
// in full.
const snippetTokens = 256

type request struct {
	Query            string `json:"query"`
	MaxResults       int    `json:"max_results"`
	MaxTokensPerPage int    `json:"max_tokens_per_page"`
}

// response is the part of Perplexity's search response kon reads.
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
	req := request{Query: q.Text, MaxResults: q.Count, MaxTokensPerPage: snippetTokens}
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
			Title: hit.Title,
			URL:   hit.URL,
			// The snippet is text extracted from the page, with Markdown
			// rather than HTML, so it is passed through as it is.
			Snippet: hit.Snippet,
			Date:    websearch.Day(hit.Date),
		})
	}
	return results, nil
}
