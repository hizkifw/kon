// Package websearch is the contract between `kon tool websearch` and the
// search providers behind it: what a provider is asked, what it returns, the
// table of providers the config may name, and how results print for a model.
// Each provider implements Engine in its own package under this one, and
// websearch/engines maps names to them. The package is a leaf so that config
// validates against the same table the engines are checked against.
package websearch

import (
	"context"
	"errors"
	"fmt"
	"slices"
	"strings"
)

const (
	// DefaultCount is how many results a search returns when the caller does
	// not say: enough to choose pages to fetch without flooding the context.
	DefaultCount = 5
	// MaxCount is the most results one search may ask for. Every provider
	// serves this many in a single request, so no engine pages.
	MaxCount = 20
)

// Query is one search. Count is between 1 and MaxCount.
type Query struct {
	Text  string
	Count int
}

// Result is one hit, in the provider's ranking order.
type Result struct {
	Title string
	URL   string
	// Snippet is plain text describing the page; providers that return
	// markup clean it with Plain.
	Snippet string
	// Date is when the page was published, as YYYY-MM-DD, or empty when the
	// provider does not say.
	Date string
}

// Engine runs a query against one provider. conn is already checked against
// the provider's Spec and its BaseURL is never empty. An engine returns at
// most q.Count results, and none without an error when nothing matched.
type Engine func(ctx context.Context, conn Connection, q Query) ([]Result, error)

// Connection is how to reach one provider, as the config gives it.
type Connection struct {
	APIKey string
	// BaseURL is the provider's root, without a trailing path. Tests point it
	// at a local server.
	BaseURL string
}

// Spec is what a provider needs from its connection.
type Spec struct {
	// DefaultBaseURL is the root used when a connection names none. Empty
	// means the provider is self-hosted, so a base URL is required.
	DefaultBaseURL string
	// KeyRequired marks providers that refuse a request without an API key.
	KeyRequired bool
}

// specs lists every provider the config may name. websearch/engines has an
// Engine for each, which its tests enforce.
var specs = map[string]Spec{
	"brave":      {DefaultBaseURL: "https://api.search.brave.com", KeyRequired: true},
	"duckduckgo": {DefaultBaseURL: "https://html.duckduckgo.com"},
	"exa":        {DefaultBaseURL: "https://api.exa.ai", KeyRequired: true},
	"perplexity": {DefaultBaseURL: "https://api.perplexity.ai", KeyRequired: true},
	"searxng":    {},
	"tavily":     {DefaultBaseURL: "https://api.tavily.com", KeyRequired: true},
}

// Lookup returns the spec of a provider name.
func Lookup(name string) (Spec, bool) {
	spec, ok := specs[name]
	return spec, ok
}

// Names lists the providers in alphabetical order.
func Names() []string {
	names := make([]string, 0, len(specs))
	for name := range specs {
		names = append(names, name)
	}
	slices.Sort(names)
	return names
}

// Connect checks conn against the provider's needs and fills in its default
// base URL. The errors name what is missing, not where the config keeps it.
func (s Spec) Connect(conn Connection) (Connection, error) {
	conn.BaseURL = strings.TrimRight(strings.TrimSpace(conn.BaseURL), "/")
	if conn.BaseURL == "" {
		conn.BaseURL = s.DefaultBaseURL
	}
	if conn.BaseURL == "" {
		return Connection{}, errors.New("requires base_url")
	}
	if s.KeyRequired && strings.TrimSpace(conn.APIKey) == "" {
		return Connection{}, errors.New("requires api_key")
	}
	return conn, nil
}

// UnknownProvider is the error for a name outside the table.
func UnknownProvider(name string) error {
	return fmt.Errorf("unknown web search provider %q (supported: %s)", name, strings.Join(Names(), ", "))
}
