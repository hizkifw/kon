package provider

import (
	"net/url"
	"strings"

	"github.com/hizkifw/kon/internal/catalog"
	"github.com/hizkifw/kon/internal/config"
)

// These providers have no usable fixed API URL in models.dev, or need a
// different login flow. The normal path is the catalog's npm + api fields.
var loginOverrides = map[string]config.Provider{
	"deepinfra": {Type: "openai-compatible", BaseURL: "https://api.deepinfra.com/v1/openai"},
	"groq":      {Type: "openai-compatible", BaseURL: "https://api.groq.com/openai/v1"},
	"cerebras":  {Type: "openai-compatible", BaseURL: "https://api.cerebras.ai/v1"},
	"xai":       {Type: "openai-compatible", BaseURL: "https://api.x.ai/v1"},
	"azure":     {Type: "openai-compatible"}, // Resource-specific endpoint.
}

var localLoginProviders = map[string]config.Provider{
	"ollama":            {Type: "ollama"},
	"openai-compatible": {Type: "openai-compatible"},
}

// LoginConnection maps a models.dev provider to a kon wire implementation.
// A catalog URL is used only when it is a concrete API base URL, not a
// template or an individual operation endpoint.
func LoginConnection(entry catalog.Provider) (config.Provider, bool) {
	if entry.ID == "" {
		return config.Provider{}, false
	}
	connection, override := loginOverrides[entry.ID]
	if !override {
		switch entry.NPM {
		case "@ai-sdk/openai":
			connection.Type = "openai"
		case "@openrouter/ai-sdk-provider":
			connection.Type = "openrouter"
		case "@ai-sdk/openai-compatible":
			connection.Type = "openai-compatible"
		default:
			return config.Provider{}, false
		}
		if entry.API != "" {
			if !usableAPIURL(entry.API) {
				return config.Provider{}, false
			}
			connection.BaseURL = strings.TrimRight(entry.API, "/")
		}
		if connection.BaseURL == "" && entry.ID != "openai" && entry.ID != "openrouter" {
			return config.Provider{}, false
		}
	}
	connection.ID = entry.ID
	connection.CatalogProvider = entry.ID
	return connection, true
}

func LocalLoginConnection(id string) (config.Provider, bool) {
	connection, ok := localLoginProviders[id]
	if ok {
		connection.ID = id
	}
	return connection, ok
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
