package provider

import (
	"net/url"
	"slices"
	"strings"

	"github.com/hizkifw/kon/internal/catalog"
	"github.com/hizkifw/kon/internal/config"
	"github.com/hizkifw/kon/internal/provider/wire"
)

// LoginEntry is one /login choice: the connection it saves and what the user
// must supply for it. These are facts about the service being connected to;
// the wire format only contributes whether a base URL has a default.
type LoginEntry struct {
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

// localLogins are servers the user runs, so login always asks where they are.
// A local Ollama takes no key; an arbitrary compatible server may or may not.
var localLogins = map[string]LoginEntry{
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

// CatalogLoginEntry maps a models.dev provider to a login entry. A catalog URL
// is used only when it is a concrete API base URL, not a template or an
// individual operation endpoint.
func CatalogLoginEntry(entry catalog.Provider) (LoginEntry, bool) {
	if entry.ID == "" {
		return LoginEntry{}, false
	}
	connection, override := loginOverrides[entry.ID]
	if !override {
		format, ok := catalogWire[entry.NPM]
		if !ok {
			return LoginEntry{}, false
		}
		connection.Type = format
		if entry.API != "" {
			if !usableAPIURL(entry.API) {
				return LoginEntry{}, false
			}
			connection.BaseURL = strings.TrimRight(entry.API, "/")
		}
		// Only the services that define a format may lean on its default
		// server; any other service must bring its own URL.
		if connection.BaseURL == "" && entry.ID != "openai" && entry.ID != "openrouter" {
			return LoginEntry{}, false
		}
	}
	connection.ID = entry.ID
	connection.CatalogProvider = entry.ID
	spec, _ := wire.Lookup(connection.Type)
	return LoginEntry{
		Connection: connection,
		AskURL:     connection.BaseURL == "" && spec.RequiresBaseURL(),
		AskKey:     true,
	}, true
}

// LocalLoginIDs lists the login entries for servers the user runs.
func LocalLoginIDs() []string {
	ids := make([]string, 0, len(localLogins))
	for id := range localLogins {
		ids = append(ids, id)
	}
	slices.Sort(ids)
	return ids
}

// LocalLoginEntry returns the login entry for a server the user runs.
func LocalLoginEntry(id string) (LoginEntry, bool) {
	entry, ok := localLogins[id]
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
