// Package exa searches with the Exa API.
package exa

import (
	"context"
	"strings"

	"kon.kitsu.red/internal/websearch"
)

// highlightChars bounds the highlights Exa returns for one page. A snippet
// prints at most 500 bytes, so longer excerpts would only be thrown away.
const highlightChars = 500

// request is the body of an Exa search.
type request struct {
	Query      string   `json:"query"`
	NumResults int      `json:"numResults"`
	Contents   contents `json:"contents"`
}

// contents asks for highlights alone. Exa bills each content view it returns,
// and highlights are the excerpts matching the query, which is what a snippet
// is for.
type contents struct {
	Highlights highlights `json:"highlights"`
}

type highlights struct {
	MaxCharacters int `json:"maxCharacters"`
}

// response is the part of Exa's search response kon reads.
type response struct {
	Results []struct {
		Title         string   `json:"title"`
		URL           string   `json:"url"`
		PublishedDate string   `json:"publishedDate"`
		Highlights    []string `json:"highlights"`
	} `json:"results"`
}

// Search implements websearch.Engine.
func Search(ctx context.Context, conn websearch.Connection, q websearch.Query) ([]websearch.Result, error) {
	req := request{
		Query:      q.Text,
		NumResults: q.Count,
		Contents:   contents{Highlights: highlights{MaxCharacters: highlightChars}},
	}
	var resp response
	if err := conn.PostJSON(ctx, "/search", map[string]string{"x-api-key": conn.APIKey}, req, &resp); err != nil {
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
			Snippet: snippet(hit.Highlights),
			Date:    websearch.Day(hit.PublishedDate),
		})
	}
	return results, nil
}

// snippet joins a page's highlights. They are separate passages of the page,
// so an ellipsis marks where one ends and the next begins.
func snippet(highlights []string) string {
	passages := make([]string, 0, len(highlights))
	for _, highlight := range highlights {
		if passage := strings.Join(strings.Fields(highlight), " "); passage != "" {
			passages = append(passages, passage)
		}
	}
	return strings.Join(passages, " … ")
}
