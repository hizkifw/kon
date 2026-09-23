// Package catalog serves models.dev metadata from a bundled snapshot and a
// locally cached refresh. It does not decide which provider APIs kon supports.
package catalog

import (
	"bytes"
	"compress/gzip"
	"context"
	"embed"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/http"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"sync"
	"time"
)

const sourceURL = "https://models.dev/api.json"

const (
	requestTimeout = 15 * time.Second
	maxCatalogSize = 16 << 20
	maxCacheSize   = 8 << 20
)

//go:generate go run ./generate

//go:embed snapshot.json.gz
var snapshot embed.FS

type Provider struct {
	ID   string   `json:"id"`
	Name string   `json:"name"`
	API  string   `json:"api"`
	NPM  string   `json:"npm"`
	Env  []string `json:"env"`
}

type providerRecord struct {
	Provider
	rawModels json.RawMessage
	once      sync.Once
	models    map[string]Model
	err       error
}

type wireProvider struct {
	Provider
	Models json.RawMessage `json:"models"`
}

type storedProvider struct {
	Provider
	Models map[string]Model `json:"models"`
}

type Model struct {
	ID               string     `json:"id"`
	Name             string     `json:"name"`
	Description      string     `json:"description"`
	Family           string     `json:"family"`
	Attachment       bool       `json:"attachment"`
	Reasoning        bool       `json:"reasoning"`
	ToolCall         bool       `json:"tool_call"`
	StructuredOutput bool       `json:"structured_output"`
	Modalities       Modalities `json:"modalities"`
	Limit            Limit      `json:"limit"`
	Cost             Cost       `json:"cost"`
}

type Modalities struct {
	Input  []string `json:"input"`
	Output []string `json:"output"`
}

type Limit struct {
	Context int `json:"context"`
	Input   int `json:"input"`
	Output  int `json:"output"`
}

// Cost values are upstream prices per million tokens, in US dollars.
type Cost struct {
	Input      float64 `json:"input"`
	Output     float64 `json:"output"`
	CacheRead  float64 `json:"cache_read"`
	CacheWrite float64 `json:"cache_write"`
}

type cacheFile struct {
	FetchedAt time.Time       `json:"fetched_at"`
	ETag      string          `json:"etag,omitempty"`
	Catalog   json.RawMessage `json:"catalog"`
}

// Service answers reads without network work. Refresh is only called on an
// explicit user request. Returned values never share mutable slices or maps
// with the service.
type Service struct {
	mu        sync.RWMutex
	refreshMu sync.Mutex
	providers map[string]*providerRecord
	fetchedAt time.Time
	etag      string
	cachePath string
	client    *http.Client
	url       string
}

// New loads the bundled snapshot, then a valid local cache when present.
// A damaged or obsolete cache cannot prevent startup.
func New(cachePath string) (*Service, error) {
	b, err := snapshot.ReadFile("snapshot.json.gz")
	if err != nil {
		return nil, fmt.Errorf("read bundled catalog: %w", err)
	}
	raw, err := decompress(b, maxCatalogSize)
	if err != nil {
		return nil, fmt.Errorf("decode bundled catalog: %w", err)
	}
	var bundled cacheFile
	if err := json.Unmarshal(raw, &bundled); err != nil || bundled.FetchedAt.IsZero() {
		return nil, errors.New("invalid bundled catalog metadata")
	}
	providers, err := parse(bundled.Catalog)
	if err != nil {
		return nil, fmt.Errorf("parse bundled catalog: %w", err)
	}
	s := &Service{
		providers: providers,
		fetchedAt: bundled.FetchedAt,
		cachePath: cachePath,
		client:    &http.Client{Timeout: requestTimeout},
		url:       sourceURL,
	}
	if cachePath == "" {
		return s, nil
	}
	f, err := os.Open(cachePath)
	if err != nil {
		return s, nil
	}
	b, err = io.ReadAll(io.LimitReader(f, maxCacheSize+1))
	f.Close()
	if err != nil || len(b) > maxCacheSize {
		return s, nil
	}
	raw, err = decompress(b, maxCatalogSize+maxCacheSize)
	if err != nil {
		return s, nil
	}
	var cached cacheFile
	if err := json.Unmarshal(raw, &cached); err != nil || cached.FetchedAt.IsZero() {
		return s, nil
	}
	if cached.FetchedAt.Before(s.fetchedAt) {
		return s, nil
	}
	providers, err = parse(cached.Catalog)
	if err != nil {
		return s, nil
	}
	s.providers, s.fetchedAt, s.etag = providers, cached.FetchedAt, cached.ETag
	if info, err := os.Stat(cachePath); err == nil && info.ModTime().After(s.fetchedAt) {
		s.fetchedAt = info.ModTime()
	}
	return s, nil
}

// Refresh fetches and validates a complete catalog before publishing it.
// Callers must only use it for an explicit refresh command.
func (s *Service) Refresh(ctx context.Context) error {
	s.refreshMu.Lock()
	defer s.refreshMu.Unlock()

	s.mu.RLock()
	etag := s.etag
	s.mu.RUnlock()
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, s.url, nil)
	if err != nil {
		return err
	}
	if etag != "" {
		req.Header.Set("If-None-Match", etag)
	}
	res, err := s.client.Do(req)
	if err != nil {
		return err
	}
	defer res.Body.Close()
	if res.StatusCode == http.StatusNotModified {
		// The previous payload is already validated, so only its freshness changes.
		now := time.Now()
		s.mu.Lock()
		s.fetchedAt = now
		s.mu.Unlock()
		if s.cachePath != "" {
			if err := os.Chtimes(s.cachePath, now, now); err != nil {
				return fmt.Errorf("update catalog cache freshness: %w", err)
			}
		}
		return nil
	}
	if res.StatusCode != http.StatusOK {
		return fmt.Errorf("fetch catalog: HTTP %d", res.StatusCode)
	}
	raw, err := io.ReadAll(io.LimitReader(res.Body, maxCatalogSize+1))
	if err != nil {
		return fmt.Errorf("read catalog: %w", err)
	}
	if len(raw) > maxCatalogSize {
		return errors.New("catalog exceeds size limit")
	}
	providers, err := parse(raw)
	if err != nil {
		return fmt.Errorf("parse catalog: %w", err)
	}
	for id, provider := range providers {
		if err := provider.load(id); err != nil {
			return err
		}
	}
	fetchedAt := time.Now()
	etag = res.Header.Get("ETag")
	var cacheErr error
	if s.cachePath != "" {
		stored := make(map[string]storedProvider, len(providers))
		for id, record := range providers {
			stored[id] = storedProvider{Provider: record.Provider, Models: record.models}
		}
		projected, err := json.Marshal(stored)
		if err != nil {
			return err
		}
		cacheErr = writeCache(s.cachePath, cacheFile{FetchedAt: fetchedAt, ETag: etag, Catalog: projected})
	}
	s.mu.Lock()
	s.providers, s.fetchedAt, s.etag = providers, fetchedAt, etag
	s.mu.Unlock()
	return cacheErr
}

func (s *Service) UpdatedAt() time.Time {
	s.mu.RLock()
	defer s.mu.RUnlock()
	return s.fetchedAt
}

// Providers returns provider metadata sorted by ID.
func (s *Service) Providers() []Provider {
	s.mu.RLock()
	defer s.mu.RUnlock()
	result := make([]Provider, 0, len(s.providers))
	for _, record := range s.providers {
		provider := record.Provider
		provider.Env = slices.Clone(provider.Env)
		result = append(result, provider)
	}
	slices.SortFunc(result, func(a, b Provider) int { return strings.Compare(a.ID, b.ID) })
	return result
}

func (s *Service) Provider(id string) (Provider, bool) {
	s.mu.RLock()
	defer s.mu.RUnlock()
	record, ok := s.providers[id]
	if !ok {
		return Provider{}, false
	}
	provider := record.Provider
	provider.Env = slices.Clone(provider.Env)
	return provider, ok
}

// Models returns models sorted by their provider-specific IDs.
func (s *Service) Models(providerID string) []Model {
	s.mu.RLock()
	defer s.mu.RUnlock()
	provider, ok := s.providers[providerID]
	if !ok {
		return nil
	}
	if err := provider.load(providerID); err != nil {
		return nil
	}
	result := make([]Model, 0, len(provider.models))
	for _, model := range provider.models {
		result = append(result, cloneModel(model))
	}
	slices.SortFunc(result, func(a, b Model) int { return strings.Compare(a.ID, b.ID) })
	return result
}

func (s *Service) Model(providerID, modelID string) (Model, bool) {
	s.mu.RLock()
	defer s.mu.RUnlock()
	provider, ok := s.providers[providerID]
	if !ok {
		return Model{}, false
	}
	if err := provider.load(providerID); err != nil {
		return Model{}, false
	}
	model, ok := provider.models[modelID]
	return cloneModel(model), ok
}

func cloneModel(model Model) Model {
	model.Modalities.Input = slices.Clone(model.Modalities.Input)
	model.Modalities.Output = slices.Clone(model.Modalities.Output)
	return model
}

func parse(raw []byte) (map[string]*providerRecord, error) {
	var wire map[string]wireProvider
	if err := json.Unmarshal(raw, &wire); err != nil {
		return nil, err
	}
	if len(wire) == 0 {
		return nil, errors.New("catalog has no providers")
	}
	providers := make(map[string]*providerRecord, len(wire))
	for id, provider := range wire {
		if id == "" || provider.ID != id || provider.Name == "" || len(provider.Models) == 0 || string(provider.Models) == "null" {
			return nil, fmt.Errorf("invalid provider %q", id)
		}
		providers[id] = &providerRecord{Provider: provider.Provider, rawModels: provider.Models}
	}
	return providers, nil
}

func (r *providerRecord) load(providerID string) error {
	r.once.Do(func() {
		if err := json.Unmarshal(r.rawModels, &r.models); err != nil {
			r.err = fmt.Errorf("parse models for %q: %w", providerID, err)
			return
		}
		if r.models == nil {
			r.err = fmt.Errorf("provider %q has no model map", providerID)
			return
		}
		for id, model := range r.models {
			if id == "" || model.ID != id || model.Name == "" {
				r.err = fmt.Errorf("invalid model %q/%q", providerID, id)
				return
			}
		}
		r.rawModels = nil
	})
	return r.err
}

func decompress(b []byte, limit int64) ([]byte, error) {
	r, err := gzip.NewReader(bytes.NewReader(b))
	if err != nil {
		return nil, err
	}
	defer r.Close()
	raw, err := io.ReadAll(io.LimitReader(r, limit+1))
	if err != nil {
		return nil, err
	}
	if int64(len(raw)) > limit {
		return nil, errors.New("decompressed catalog exceeds size limit")
	}
	return raw, nil
}

func writeCache(path string, cached cacheFile) error {
	if err := os.MkdirAll(filepath.Dir(path), 0o700); err != nil {
		return err
	}
	tmp, err := os.CreateTemp(filepath.Dir(path), "catalog-*.tmp")
	if err != nil {
		return err
	}
	defer os.Remove(tmp.Name())
	if err := tmp.Chmod(0o600); err != nil {
		tmp.Close()
		return err
	}
	zw := gzip.NewWriter(tmp)
	if err := json.NewEncoder(zw).Encode(cached); err != nil {
		zw.Close()
		tmp.Close()
		return err
	}
	if err := zw.Close(); err != nil {
		tmp.Close()
		return err
	}
	if err := tmp.Sync(); err != nil {
		tmp.Close()
		return err
	}
	if err := tmp.Close(); err != nil {
		return err
	}
	return os.Rename(tmp.Name(), path)
}
