package login

import (
	"testing"

	"github.com/hizkifw/kon/internal/catalog"
	"github.com/hizkifw/kon/internal/provider/wire"
)

func TestCatalogLoginEntryUsesCatalogWireAndURL(t *testing.T) {
	entry := catalog.Provider{ID: "fireworks-ai", NPM: "@ai-sdk/openai-compatible", API: "https://api.fireworks.ai/inference/v1/"}
	login, ok := CatalogEntry(entry)
	connection := login.Connection
	if !ok || connection.ID != entry.ID || connection.CatalogProvider != entry.ID || connection.Type != wire.OpenAICompatible || connection.BaseURL != "https://api.fireworks.ai/inference/v1" {
		t.Fatalf("connection = %#v, %v", connection, ok)
	}
	if login.AskURL || !login.AskKey || login.KeyOptional {
		t.Fatalf("a hosted service with a known URL asks only for a required key: %#v", login)
	}
	entry.ID, entry.API = "another-provider", "https://another.example/v1"
	if login, ok := CatalogEntry(entry); !ok || login.Connection.BaseURL != entry.API {
		t.Fatalf("new catalog entry was not mapped: %#v, %v", login, ok)
	}
}

func TestCatalogLoginEntryOverridesOnlyExceptions(t *testing.T) {
	for _, test := range []struct {
		entry  catalog.Provider
		url    string
		askURL bool
	}{
		{catalog.Provider{ID: "deepinfra", NPM: "@ai-sdk/deepinfra"}, "https://api.deepinfra.com/v1/openai", false},
		// Azure endpoints are per resource, so login must ask for one.
		{catalog.Provider{ID: "azure", NPM: "@ai-sdk/azure"}, "", true},
		// OpenAI defines its format, whose default server needs no answer.
		{catalog.Provider{ID: "openai", NPM: "@ai-sdk/openai"}, "", false},
	} {
		login, ok := CatalogEntry(test.entry)
		if !ok || login.Connection.BaseURL != test.url || login.AskURL != test.askURL {
			t.Fatalf("login entry for %s = %#v, %v", test.entry.ID, login, ok)
		}
	}
	if _, ok := CatalogEntry(catalog.Provider{ID: "unknown", NPM: "@ai-sdk/anthropic"}); ok {
		t.Fatal("unsupported wire format was accepted")
	}
}

func TestCatalogLoginEntrySkipsTemplateAndOperationURLs(t *testing.T) {
	for _, api := range []string{"https://${HOST}/v1", "https://example.com/v1/chat/completions", "http://example.com/v1"} {
		if _, ok := CatalogEntry(catalog.Provider{ID: "example", NPM: "@ai-sdk/openai-compatible", API: api}); ok {
			t.Fatalf("accepted unusable URL %q", api)
		}
	}
}

func TestLocalEntriesAskForTheServer(t *testing.T) {
	ollama, ok := LocalEntry("ollama")
	if !ok || ollama.Connection.ID != "ollama" || ollama.Connection.Type != wire.Ollama || !ollama.AskURL || ollama.DefaultURL == "" || ollama.AskKey {
		t.Fatalf("ollama = %#v, %v", ollama, ok)
	}
	compatible, ok := LocalEntry("openai-compatible")
	if !ok || !compatible.AskURL || compatible.DefaultURL != "" || !compatible.AskKey || !compatible.KeyOptional {
		t.Fatalf("openai-compatible = %#v, %v", compatible, ok)
	}
	if _, ok := LocalEntry("fireworks-ai"); ok {
		t.Fatal("a catalog service was treated as local")
	}
}
