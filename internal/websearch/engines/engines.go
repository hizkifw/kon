// Package engines maps each web search provider name to its implementation.
// It is the one place that imports them all, so a new provider adds its
// package, a line here, and a line in the websearch table.
package engines

import (
	"context"
	"errors"
	"fmt"
	"net/http"
	"strings"

	"kon.kitsu.red/internal/websearch"
	"kon.kitsu.red/internal/websearch/brave"
	"kon.kitsu.red/internal/websearch/duckduckgo"
	"kon.kitsu.red/internal/websearch/exa"
	"kon.kitsu.red/internal/websearch/perplexity"
	"kon.kitsu.red/internal/websearch/searxng"
	"kon.kitsu.red/internal/websearch/tavily"
)

var engines = map[string]websearch.Engine{
	"brave":      brave.Search,
	"duckduckgo": duckduckgo.Search,
	"exa":        exa.Search,
	"perplexity": perplexity.Search,
	"searxng":    searxng.Search,
	"tavily":     tavily.Search,
}

// Search runs q against the named provider. It checks the connection and the
// query first, so every engine can rely on both, and holds the engine to the
// count it was asked for. Errors name the provider.
func Search(ctx context.Context, name string, conn websearch.Connection, q websearch.Query) ([]websearch.Result, error) {
	spec, known := websearch.Lookup(name)
	engine := engines[name]
	if !known || engine == nil {
		return nil, websearch.UnknownProvider(name)
	}
	conn, err := spec.Connect(conn)
	if err != nil {
		return nil, fmt.Errorf("%s %w", name, err)
	}
	q.Text = strings.TrimSpace(q.Text)
	if q.Text == "" {
		return nil, errors.New("query must not be empty")
	}
	if q.Count < 1 || q.Count > websearch.MaxCount {
		return nil, fmt.Errorf("count must be between 1 and %d", websearch.MaxCount)
	}
	results, err := engine(ctx, conn, q)
	if err != nil {
		var status *websearch.StatusError
		if errors.As(err, &status) && (status.Code == http.StatusUnauthorized || status.Code == http.StatusForbidden) && spec.KeyRequired {
			return nil, fmt.Errorf("%s: %w (check its api_key)", name, err)
		}
		return nil, fmt.Errorf("%s: %w", name, err)
	}
	if len(results) > q.Count {
		results = results[:q.Count]
	}
	return results, nil
}
