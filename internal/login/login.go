// Package login maps a service the user asks to connect to onto a connection
// kon can save, and verifies that connection on explicit /login. It holds the
// service-identity quirks that the wire-format table deliberately leaves out.
package login

import (
	"net/url"
	"slices"
	"strings"

	"github.com/hizkifw/kon/internal/catalog"
	"github.com/hizkifw/kon/internal/config"
	"github.com/hizkifw/kon/internal/provider/wire"
)

// Entry is one /login choice: the connection it saves and what the user
// must supply for it. These are facts about the service being connected to;
// the wire format only contributes whether a base URL has a default.
type Entry struct {
	Connection config.Provider
	// AskURL asks for the service's base URL. DefaultURL, when set, is used
	// if the answer is blank.
	AskURL     bool
	DefaultURL string
	// AskKey asks for an API key. KeyOptional accepts a blank answer.
	AskKey      bool
	KeyOptional bool
}

// These services have no usable fixed API URL in models.dev, or need a
// different login flow. The normal path is the catalog's npm + api fields.
var loginOverrides = map[string]config.Provider{
	"deepinfra": {Type: wire.OpenAICompatible, BaseURL: "https://api.deepinfra.com/v1/openai"},
	"groq":      {Type: wire.OpenAICompatible, BaseURL: "https://api.groq.com/openai/v1"},
	"cerebras":  {Type: wire.OpenAICompatible, BaseURL: "https://api.cerebras.ai/v1"},
	"xai":       {Type: wire.OpenAICompatible, BaseURL: "https://api.x.ai/v1"},
	"azure":     {Type: wire.OpenAICompatible}, // Resource-specific endpoint.
}

// localEntries are servers the user runs, so login always asks where they are.
// A local Ollama takes no key; an arbitrary compatible server may or may not.
var localEntries = map[string]Entry{
	"ollama": {
		Connection: config.Provider{Type: wire.Ollama},
		AskURL:     true, DefaultURL: "http://localhost:11434",
	},
	"openai-compatible": {
		Connection: config.Provider{Type: wire.OpenAICompatible},
		AskURL:     true, AskKey: true, KeyOptional: true,
	},
}

// catalogWire maps a models.dev provider's AI SDK package to the wire format
// it speaks. A package without an entry needs a backend kon does not have.
var catalogWire = map[string]wire.Format{
	"@ai-sdk/openai":              wire.OpenAI,
	"@openrouter/ai-sdk-provider": wire.OpenRouter,
	"@ai-sdk/openai-compatible":   wire.OpenAICompatible,
}

// CatalogEntry maps a models.dev provider to a login entry. A catalog URL
// is used only when it is a concrete API base URL, not a template or an
// individual operation endpoint.
func CatalogEntry(entry catalog.Provider) (Entry, bool) {
	if entry.ID == "" {
		return Entry{}, false
	}
	connection, override := loginOverrides[entry.ID]
	if !override {
		format, ok := catalogWire[entry.NPM]
		if !ok {
			return Entry{}, false
		}
		connection.Type = format
		if entry.API != "" {
			if !usableAPIURL(entry.API) {
				return Entry{}, false
			}
			connection.BaseURL = strings.TrimRight(entry.API, "/")
		}
		// Only the services that define a format may lean on its default
		// server; any other service must bring its own URL.
		if connection.BaseURL == "" && entry.ID != "openai" && entry.ID != "openrouter" {
			return Entry{}, false
		}
	}
	connection.ID = entry.ID
	connection.CatalogProvider = entry.ID
	spec, _ := wire.Lookup(connection.Type)
	return Entry{
		Connection: connection,
		AskURL:     connection.BaseURL == "" && spec.RequiresBaseURL(),
		AskKey:     true,
	}, true
}

// LocalIDs lists the login entries for servers the user runs.
func LocalIDs() []string {
	ids := make([]string, 0, len(localEntries))
	for id := range localEntries {
		ids = append(ids, id)
	}
	slices.Sort(ids)
	return ids
}

// LocalEntry returns the login entry for a server the user runs.
func LocalEntry(id string) (Entry, bool) {
	entry, ok := localEntries[id]
	if ok {
		entry.Connection.ID = id
	}
	return entry, ok
}

func usableAPIURL(raw string) bool {
	if strings.Contains(raw, "${") {
		return false
	}
	u, err := url.Parse(raw)
	if err != nil || u.Hostname() == "" || u.User != nil || u.RawQuery != "" || u.Fragment != "" {
		return false
	}
	if u.Scheme != "https" && !(u.Scheme == "http" && (u.Hostname() == "localhost" || u.Hostname() == "127.0.0.1" || u.Hostname() == "::1")) {
		return false
	}
	return !strings.HasSuffix(strings.TrimRight(u.Path, "/"), "/chat/completions")
}
