package provider

import (
	"testing"

	"github.com/hizkifw/kon/internal/catalog"
)

func TestLoginConnectionUsesCatalogWireAndURL(t *testing.T) {
	entry := catalog.Provider{ID: "fireworks-ai", NPM: "@ai-sdk/openai-compatible", API: "https://api.fireworks.ai/inference/v1/"}
	connection, ok := LoginConnection(entry)
	if !ok || connection.ID != entry.ID || connection.CatalogProvider != entry.ID || connection.Type != "openai-compatible" || connection.BaseURL != "https://api.fireworks.ai/inference/v1" {
		t.Fatalf("connection = %#v, %v", connection, ok)
	}
	entry.ID, entry.API = "another-provider", "https://another.example/v1"
	if connection, ok := LoginConnection(entry); !ok || connection.BaseURL != entry.API {
		t.Fatalf("new catalog entry was not mapped: %#v, %v", connection, ok)
	}
}

func TestLoginConnectionOverridesOnlyExceptions(t *testing.T) {
	for _, test := range []struct {
		entry catalog.Provider
		url   string
	}{
		{catalog.Provider{ID: "deepinfra", NPM: "@ai-sdk/deepinfra"}, "https://api.deepinfra.com/v1/openai"},
		{catalog.Provider{ID: "azure", NPM: "@ai-sdk/azure"}, ""},
	} {
		connection, ok := LoginConnection(test.entry)
		if !ok || connection.BaseURL != test.url {
			t.Fatalf("connection for %s = %#v, %v", test.entry.ID, connection, ok)
		}
	}
	if _, ok := LoginConnection(catalog.Provider{ID: "unknown", NPM: "@ai-sdk/anthropic"}); ok {
		t.Fatal("unsupported wire format was accepted")
	}
}

func TestLoginConnectionSkipsTemplateAndOperationURLs(t *testing.T) {
	for _, api := range []string{"https://${HOST}/v1", "https://example.com/v1/chat/completions", "http://example.com/v1"} {
		if _, ok := LoginConnection(catalog.Provider{ID: "example", NPM: "@ai-sdk/openai-compatible", API: api}); ok {
			t.Fatalf("accepted unusable URL %q", api)
		}
	}
}
