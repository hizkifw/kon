package catalog

import (
	"context"
	"encoding/json"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"sync"
	"testing"
	"time"
)

const testCatalog = `{"example":{"id":"example","name":"Example","api":"https://example.test/v1","npm":"@ai-sdk/openai-compatible","env":["EXAMPLE_KEY"],"models":{"new-model":{"id":"new-model","name":"New Model","tool_call":true,"modalities":{"input":["text","image"],"output":["text"]},"limit":{"context":123456},"cost":{"input":1.25,"output":2.5}}}}}`

func TestBundledCatalogIsAvailable(t *testing.T) {
	s, err := New(filepath.Join(t.TempDir(), "missing.gz"))
	if err != nil {
		t.Fatal(err)
	}
	provider, ok := s.Provider("openai")
	if !ok || provider.Name == "" {
		t.Fatalf("bundled provider = %+v, %v", provider, ok)
	}
	models := s.Models("openai")
	if len(models) == 0 {
		t.Fatal("bundled snapshot has no OpenAI models")
	}
	model, ok := s.Model("openai", models[0].ID)
	if !ok || model.ID != models[0].ID {
		t.Fatalf("bundled model = %+v, %v", model, ok)
	}
	models[0].Modalities.Input = append(models[0].Modalities.Input, "changed")
	provider.Env = append(provider.Env, "CHANGED")
	again, _ := s.Model("openai", model.ID)
	for _, input := range again.Modalities.Input {
		if input == "changed" {
			t.Fatal("caller mutated catalog model")
		}
	}
	againProvider, _ := s.Provider("openai")
	for _, env := range againProvider.Env {
		if env == "CHANGED" {
			t.Fatal("caller mutated catalog provider")
		}
	}
}

func TestRefreshAndCache(t *testing.T) {
	cachePath := filepath.Join(t.TempDir(), "models.json.gz")
	var requests int
	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		requests++
		if requests == 1 {
			if got := r.Header.Get("If-None-Match"); got != "" {
				t.Errorf("first conditional header = %q", got)
			}
			w.Header().Set("ETag", `"catalog-v1"`)
			w.Write([]byte(testCatalog))
			return
		}
		if got := r.Header.Get("If-None-Match"); got != `"catalog-v1"` {
			t.Errorf("conditional header = %q", got)
		}
		w.WriteHeader(http.StatusNotModified)
	}))
	defer server.Close()
	s, err := New(cachePath)
	if err != nil {
		t.Fatal(err)
	}
	s.client, s.url = server.Client(), server.URL
	if err := s.Refresh(context.Background()); err != nil {
		t.Fatal(err)
	}
	model, ok := s.Model("example", "new-model")
	if !ok || !model.ToolCall || model.Limit.Context != 123456 || model.Cost.Input != 1.25 {
		t.Fatalf("refreshed model = %+v, %v", model, ok)
	}
	if s.UpdatedAt().IsZero() {
		t.Fatal("refresh timestamp missing")
	}
	if err := s.Refresh(context.Background()); err != nil {
		t.Fatal(err)
	}
	firstUpdated := s.UpdatedAt()
	if err := os.Chtimes(cachePath, firstUpdated.Add(-time.Hour), firstUpdated.Add(-time.Hour)); err != nil {
		t.Fatal(err)
	}
	if err := s.Refresh(context.Background()); err != nil {
		t.Fatal(err)
	}
	if !s.UpdatedAt().After(firstUpdated) {
		t.Fatal("304 did not update cache freshness")
	}
	reopened, err := New(cachePath)
	if err != nil {
		t.Fatal(err)
	}
	if _, ok := reopened.Model("example", "new-model"); !ok {
		t.Fatal("cache was not loaded")
	}
	if reopened.UpdatedAt().IsZero() {
		t.Fatal("cache freshness was not retained")
	}
	if requests != 3 {
		t.Fatalf("requests = %d, want 3", requests)
	}
}

func TestOlderCacheDoesNotReplaceBundledSnapshot(t *testing.T) {
	cachePath := filepath.Join(t.TempDir(), "models.json.gz")
	if err := writeCache(cachePath, cacheFile{
		FetchedAt: time.Now().Add(-30 * 24 * time.Hour),
		Catalog:   json.RawMessage(testCatalog),
	}); err != nil {
		t.Fatal(err)
	}
	s, err := New(cachePath)
	if err != nil {
		t.Fatal(err)
	}
	if _, ok := s.Provider("openai"); !ok {
		t.Fatal("older cache replaced newer bundled snapshot")
	}
}

func TestInvalidRefreshKeepsPreviousCatalog(t *testing.T) {
	cachePath := filepath.Join(t.TempDir(), "models.json.gz")
	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Write([]byte(`{"example":{"id":"wrong","name":"Example","models":{}}}`))
	}))
	defer server.Close()
	s, err := New(cachePath)
	if err != nil {
		t.Fatal(err)
	}
	s.client, s.url = server.Client(), server.URL
	if err := s.Refresh(context.Background()); err == nil {
		t.Fatal("accepted malformed catalog")
	}
	if _, ok := s.Provider("openai"); !ok {
		t.Fatal("failed refresh replaced bundled catalog")
	}
	if _, err := os.Stat(cachePath); !os.IsNotExist(err) {
		t.Fatalf("bad cache was written: %v", err)
	}
	if err := os.WriteFile(cachePath, []byte("damaged"), 0o600); err != nil {
		t.Fatal(err)
	}
	reopened, err := New(cachePath)
	if err != nil {
		t.Fatal(err)
	}
	if _, ok := reopened.Provider("openai"); !ok {
		t.Fatal("damaged cache hid bundled catalog")
	}
}

func TestCacheWriteFailureStillPublishesRefresh(t *testing.T) {
	cachePath := filepath.Join(t.TempDir(), "cache")
	if err := os.Mkdir(cachePath, 0o700); err != nil {
		t.Fatal(err)
	}
	s, err := New(cachePath)
	if err != nil {
		t.Fatal(err)
	}
	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Write([]byte(testCatalog))
	}))
	defer server.Close()
	s.client, s.url = server.Client(), server.URL
	if err := s.Refresh(context.Background()); err == nil {
		t.Fatal("missing cache error")
	}
	if _, ok := s.Model("example", "new-model"); !ok {
		t.Fatal("cache failure hid refreshed catalog")
	}
}

func TestConcurrentReadsDuringRefresh(t *testing.T) {
	s, err := New("")
	if err != nil {
		t.Fatal(err)
	}
	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Write([]byte(testCatalog))
	}))
	defer server.Close()
	s.client, s.url = server.Client(), server.URL
	var wg sync.WaitGroup
	for range 4 {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for range 100 {
				_ = s.Providers()
				_ = s.Models("example")
				_, _ = s.Model("openai", "gpt-5-mini")
			}
		}()
	}
	if err := s.Refresh(context.Background()); err != nil {
		t.Fatal(err)
	}
	wg.Wait()
	if _, ok := s.Model("example", "new-model"); !ok {
		t.Fatal("new catalog was not published")
	}
}

func TestParseRejectsEmptyCatalog(t *testing.T) {
	for _, input := range []string{`{}`, `null`, `[]`} {
		if _, err := parse(json.RawMessage(input)); err == nil {
			t.Fatalf("accepted %s", input)
		}
	}
}

func BenchmarkNew(b *testing.B) {
	for range b.N {
		if _, err := New(""); err != nil {
			b.Fatal(err)
		}
	}
}
