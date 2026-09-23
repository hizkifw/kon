package app

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"slices"
	"strings"

	"github.com/hizkifw/kon/internal/agent"
	"github.com/hizkifw/kon/internal/catalog"
	"github.com/hizkifw/kon/internal/config"
	"github.com/hizkifw/kon/internal/provider"
	"github.com/hizkifw/kon/internal/tools"
)

// Models combines explicit profiles with discovered and catalog entries.
// Catalog decompression happens only when the user requests this list.
func (r *Runtime) Models() []Model {
	r.mu.Lock()
	defer r.mu.Unlock()
	service := r.catalogService()
	result := make([]Model, 0, len(r.config.Models))
	for _, model := range r.config.Models {
		resolved, _ := r.config.ResolveModel(model.Name)
		entry := describe(resolved)
		entry.DisplayName = model.Name
		entry.Source = "configured"
		result = append(result, entry)
	}
	if len(r.config.Providers) == 0 {
		return result
	}
	for _, connection := range r.config.Providers {
		ids := make(map[string]string)
		displayNames := make(map[string]string)
		for _, id := range r.providerModels[connection.ID] {
			ids[id] = "provider list"
		}
		if service != nil && catalogKey(connection) != "" && catalogKey(connection) != "azure" {
			for _, model := range service.Models(catalogKey(connection)) {
				displayNames[model.ID] = model.Name
				if ids[model.ID] == "" {
					ids[model.ID] = "catalog"
				}
			}
		}
		for id, source := range ids {
			name := connection.ID + "/" + id
			profile, _ := r.resolveModel(name)
			entry := describe(profile)
			entry.ConnectionID = connection.ID
			entry.DisplayName = displayNames[id]
			if entry.DisplayName == "" {
				entry.DisplayName = id
			}
			entry.Source = source
			result = append(result, entry)
		}
	}
	slices.SortFunc(result, func(a, b Model) int { return strings.Compare(a.Name, b.Name) })
	return result
}

// catalogService decompresses the catalog at most once. It does not take r.mu,
// so LoadCatalog can run it in the background while other methods wait on the
// same load instead of starting their own. It returns nil if loading failed;
// r.catalog.Load() peeks without waiting.
func (r *Runtime) catalogService() *catalog.Service {
	r.catalogOnce.Do(func() {
		if service, err := catalog.New(r.paths.Catalog); err == nil {
			r.catalog.Store(service)
		}
	})
	return r.catalog.Load()
}

// LoadCatalog loads the catalog and applies its metadata to the active model.
// The UI calls it after the first frame so startup never waits for the load.
func (r *Runtime) LoadCatalog() {
	r.catalogService()
	r.mu.Lock()
	defer r.mu.Unlock()
	// A running turn owns the runner; Run resolves the model before its next
	// request instead.
	if r.phase == PhaseReady {
		_ = r.resolveActive()
	}
}

// resolveActive applies catalog capabilities to an active derived model and
// rebuilds its runner. Explicit profiles are resolved when selected.
func (r *Runtime) resolveActive() error {
	if r.activeResolved {
		return nil
	}
	profile, ok := r.resolveModel(r.active.Name)
	if !ok {
		return nil
	}
	client, err := provider.New(profile, r.store.ReadImage)
	if err != nil {
		return err
	}
	r.active = profile
	r.runner = agent.New(profile, r.config.Compaction, client, r.store, tools.New(r.cwd, profile.Vision))
	r.activeResolved = true
	return nil
}

// describeActive adds catalog display metadata when the catalog has loaded,
// without waiting for it.
func (r *Runtime) describeActive() Model {
	entry := describe(r.active)
	entry.DisplayName = r.active.Name
	if _, explicit := r.config.Model(r.active.Name); explicit || !r.config.DerivedModel(r.active.Name) {
		return entry
	}
	providerID, _, _ := strings.Cut(r.active.Name, "/")
	connection, _ := r.config.Provider(providerID)
	entry.ConnectionID = connection.ID
	entry.DisplayName = r.active.ModelID
	if service := r.catalog.Load(); r.activeResolved && service != nil && catalogKey(connection) != "azure" {
		if metadata, ok := service.Model(catalogKey(connection), r.active.ModelID); ok && metadata.Name != "" {
			entry.DisplayName = metadata.Name
		}
	}
	return entry
}

// LoginProviders resolves the supported catalog entries only when the login
// picker is opened; startup never decompresses the catalog for this purpose.
func (r *Runtime) LoginProviders() []string {
	r.mu.Lock()
	defer r.mu.Unlock()
	ids := []string{"ollama", "openai-compatible"}
	service := r.catalogService()
	if service == nil {
		return ids
	}
	for _, entry := range service.Providers() {
		if _, ok := provider.LoginConnection(entry); ok {
			ids = append(ids, entry.ID)
		}
	}
	slices.Sort(ids)
	return ids
}

func (r *Runtime) LoginConnection(id string) (config.Provider, bool) {
	r.mu.Lock()
	defer r.mu.Unlock()
	if connection, ok := provider.LocalLoginConnection(id); ok {
		return connection, true
	}
	service := r.catalogService()
	if service == nil {
		return config.Provider{}, false
	}
	entry, ok := service.Provider(id)
	if !ok {
		return config.Provider{}, false
	}
	return provider.LoginConnection(entry)
}

func (r *Runtime) resolveModel(name string) (config.Model, bool) {
	profile, ok := r.config.ResolveModel(name)
	if !ok {
		return profile, false
	}
	if _, explicit := r.config.Model(name); explicit {
		return profile, true
	}
	providerID, _, _ := strings.Cut(name, "/")
	connection, _ := r.config.Provider(providerID)
	if service := r.catalogService(); service != nil && catalogKey(connection) != "azure" {
		if metadata, found := service.Model(catalogKey(connection), profile.ModelID); found {
			if metadata.Limit.Context > r.config.Compaction.ReserveTokens+r.config.Compaction.KeepRecentTokens {
				profile.ContextWindowTokens = metadata.Limit.Context
			}
			profile.Vision = slices.Contains(metadata.Modalities.Input, "image")
		}
	}
	return profile, true
}

func catalogKey(connection config.Provider) string {
	if connection.CatalogProvider != "" {
		return connection.CatalogProvider
	}
	if connection.Type != "ollama" && connection.ID != "openai-compatible" {
		return connection.ID
	}
	return ""
}

// Login verifies a provider only in response to the user's /login command,
// then stores its connection and discovered model IDs separately.
func (r *Runtime) Login(ctx context.Context, connection config.Provider) (int, bool, error) {
	r.mu.Lock()
	if err := r.mutable(); err != nil {
		r.mu.Unlock()
		return 0, false, err
	}
	r.mu.Unlock()
	models, verified, err := provider.Discover(ctx, connection)
	if err != nil {
		return 0, false, err
	}
	r.mu.Lock()
	defer r.mu.Unlock()
	if err := r.mutable(); err != nil {
		return 0, false, err
	}
	if err := ctx.Err(); err != nil {
		return 0, false, err
	}
	current, err := config.Load(r.paths.ConfigFile)
	if err != nil {
		return 0, false, err
	}
	updated := false
	for i := range current.Providers {
		if current.Providers[i].ID == connection.ID {
			current.Providers[i] = connection
			updated = true
			break
		}
	}
	if !updated {
		current.Providers = append(current.Providers, connection)
	}
	if err := current.Validate(); err != nil {
		return 0, false, err
	}
	if err := current.Save(r.paths.ConfigFile); err != nil {
		return 0, false, err
	}
	r.config = current
	if r.providerModels == nil {
		r.providerModels = make(map[string][]string)
	}
	if verified {
		r.providerModels[connection.ID] = models
	} else {
		delete(r.providerModels, connection.ID)
	}
	if err := saveProviderModels(r.paths.ProviderModels, r.providerModels); err != nil {
		return len(models), verified, fmt.Errorf("provider saved, but model cache: %w", err)
	}
	// A credential change applies to the live runner without adding a model
	// switch to the durable session or rebuilding its system prompt.
	if r.store != nil && r.active.Name != "" {
		if profile, ok := r.resolveModel(r.active.Name); ok {
			if profile.Ready() == nil {
				client, err := provider.New(profile, r.store.ReadImage)
				if err == nil {
					r.active = profile
					r.runner = agent.New(profile, r.config.Compaction, client, r.store, tools.New(r.cwd, profile.Vision))
					r.problem, r.phase = nil, PhaseReady
					r.activeResolved = true
				}
			}
		}
	}
	return len(models), verified, nil
}

func loadProviderModels(path string) map[string][]string {
	if path == "" {
		return nil
	}
	f, err := os.Open(path)
	if err != nil {
		return nil
	}
	defer f.Close()
	b, err := io.ReadAll(io.LimitReader(f, 1<<20))
	if err != nil {
		return nil
	}
	var models map[string][]string
	if json.Unmarshal(b, &models) != nil {
		return nil
	}
	return models
}

func saveProviderModels(path string, models map[string][]string) error {
	if path == "" {
		return nil
	}
	if err := os.MkdirAll(filepath.Dir(path), 0700); err != nil {
		return err
	}
	b, err := json.Marshal(models)
	if err != nil {
		return err
	}
	tmp, err := os.CreateTemp(filepath.Dir(path), ".provider-models-*")
	if err != nil {
		return err
	}
	defer os.Remove(tmp.Name())
	if err := tmp.Chmod(0600); err != nil {
		tmp.Close()
		return err
	}
	if _, err := tmp.Write(b); err != nil {
		tmp.Close()
		return err
	}
	if err := tmp.Close(); err != nil {
		return err
	}
	return os.Rename(tmp.Name(), path)
}
