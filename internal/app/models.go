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
	"github.com/hizkifw/kon/internal/login"
	"github.com/hizkifw/kon/internal/provider"
	"github.com/hizkifw/kon/internal/session"
	"github.com/hizkifw/kon/internal/tokens"
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
		entry := describe(modelSpec{Model: resolved})
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
		if key := catalogKey(connection); service != nil && key != "" {
			for _, model := range service.Models(key) {
				displayNames[model.ID] = model.Name
				if ids[model.ID] == "" {
					ids[model.ID] = "catalog"
				}
			}
		}
		for id, source := range ids {
			name := connection.ID + "/" + id
			profile, _ := r.resolveModel(name)
			entry := describe(modelSpec{Model: profile})
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
	resolved, ok := r.resolveModel(r.active.Name)
	if !ok {
		return nil
	}
	profile := r.withEffort(resolved)
	client, err := provider.New(profile.providerSpec(), r.store.ReadImage)
	if err != nil {
		return err
	}
	r.active = profile
	r.runner = agent.New(profile.limits(r.config.Compaction), client, r.store, tools.New(r.cwd, profile.Vision))
	r.activeResolved = true
	return nil
}

// CycleEffort advances the active model to its next reasoning effort, saves it
// to the config like the active model, and returns it. After the last level
// the cycle returns to the provider default, reported as "". It adds nothing to
// the durable session, so the cached prompt prefix is untouched.
func (r *Runtime) CycleEffort() (string, error) {
	r.mu.Lock()
	defer r.mu.Unlock()
	if err := r.mutable(); err != nil {
		return "", err
	}
	if r.phase == PhaseNeedsConfiguration || r.runner == nil {
		return "", ErrNotReady
	}
	// A derived model learns its effort levels from the catalog.
	if err := r.resolveActive(); err != nil {
		return "", err
	}
	efforts := r.active.ReasoningEfforts
	if len(efforts) == 0 {
		return "", ErrNoEffort
	}
	profile := r.active
	profile.effort = ""
	if i := slices.Index(efforts, r.active.effort); i+1 < len(efforts) {
		profile.effort = efforts[i+1]
	}
	client, err := provider.New(profile.providerSpec(), r.store.ReadImage)
	if err != nil {
		return "", err
	}
	updated := r.config
	updated.ReasoningEffort = profile.effort
	if err := updated.Save(r.paths.ConfigFile); err != nil {
		return "", fmt.Errorf("saving reasoning effort: %w", err)
	}
	r.config = updated
	r.active = profile
	r.runner = agent.New(profile.limits(r.config.Compaction), client, r.store, tools.New(r.cwd, profile.Vision))
	return profile.effort, nil
}

// withEffort applies the saved effort to a profile of the active model. A
// level the model does not list, including one saved before its catalog
// levels were known, falls back to the provider default.
func (r *Runtime) withEffort(profile config.Model) modelSpec {
	spec := modelSpec{Model: profile}
	if slices.Contains(profile.ReasoningEfforts, r.config.ReasoningEffort) {
		spec.effort = r.config.ReasoningEffort
	}
	return spec
}

// describeActive adds catalog display metadata when the catalog has loaded,
// without waiting for it.
func (r *Runtime) describeActive() Model {
	return r.describeProfile(r.active, r.activeResolved)
}

// DescribeSelection names a recorded model selection the way the header names
// the live model, for a replayed model change. It never waits for the catalog:
// until the catalog loads, a derived model is named by its model ID and has no
// effort levels. The effort is always the provider default, which is where a
// switch starts.
func (r *Runtime) DescribeSelection(selection session.ModelSelection) Model {
	r.mu.Lock()
	defer r.mu.Unlock()
	profile, ok := r.config.ResolveModel(selection.Name)
	if !ok {
		// The profile or connection is gone; the record still names the model.
		return Model{
			Name: selection.Name, ConnectionID: selection.ConnectionID, DisplayName: selection.ExternalID.String(),
			WireFormat: selection.WireFormat, ExternalID: selection.ExternalID.String(),
		}
	}
	resolved := r.catalog.Load() != nil
	if resolved {
		// The catalog is already loaded, so this does not block on it.
		profile, _ = r.resolveModel(selection.Name)
	}
	return r.describeProfile(modelSpec{Model: profile}, resolved)
}

// describeProfile names a profile for display. A derived model is named by its
// catalog display name once withCatalog allows it, and by its model ID before.
func (r *Runtime) describeProfile(profile modelSpec, withCatalog bool) Model {
	entry := describe(profile)
	entry.DisplayName = profile.Name
	if _, explicit := r.config.Model(profile.Name); explicit || !r.config.DerivedModel(profile.Name) {
		return entry
	}
	providerID, _, _ := strings.Cut(profile.Name, "/")
	connection, _ := r.config.Provider(providerID)
	entry.ConnectionID = connection.ID
	entry.DisplayName = profile.ModelID
	if service, key := r.catalog.Load(), catalogKey(connection); withCatalog && service != nil && key != "" {
		if metadata, ok := service.Model(key, profile.ModelID); ok && metadata.Name != "" {
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
	ids := login.LocalIDs()
	service := r.catalogService()
	if service == nil {
		return ids
	}
	for _, entry := range service.Providers() {
		if _, ok := login.CatalogEntry(entry); ok {
			ids = append(ids, entry.ID)
		}
	}
	slices.Sort(ids)
	return ids
}

// LoginEntry describes one /login choice. It is the login package's type:
// the UI reads what to ask for without learning any wire format's rules.
type LoginEntry = login.Entry

func (r *Runtime) LoginEntry(id string) (LoginEntry, bool) {
	r.mu.Lock()
	defer r.mu.Unlock()
	if entry, ok := login.LocalEntry(id); ok {
		return entry, true
	}
	service := r.catalogService()
	if service == nil {
		return LoginEntry{}, false
	}
	entry, ok := service.Provider(id)
	if !ok {
		return LoginEntry{}, false
	}
	return login.CatalogEntry(entry)
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
	if service, key := r.catalogService(), catalogKey(connection); service != nil && key != "" {
		if metadata, found := service.Model(key, profile.ModelID); found {
			if window := tokens.Count(metadata.Limit.Context); window > r.config.Compaction.ReserveTokens+r.config.Compaction.KeepRecentTokens {
				profile.ContextWindowTokens = window
			}
			profile.Vision = slices.Contains(metadata.Modalities.Input, "image")
			profile.Reasoning = metadata.Reasoning
			profile.ReasoningEfforts = metadata.Efforts()
			if len(profile.ReasoningEfforts) == 0 && metadata.ReasoningToggle() {
				// A model that can only switch reasoning off gets one level:
				// "none" is the chat format's effort value for no reasoning.
				profile.ReasoningEfforts = []string{"none"}
			}
		}
	}
	return profile, true
}

// catalogKey is the models.dev provider whose model metadata describes a
// connection's models, or "" when none does. It depends only on which service
// the connection reaches, never on the wire format it speaks. A local login
// has no catalog entry. Azure's entry lists base models, while its requests
// name the user's own deployments, so its metadata would be misattributed.
func catalogKey(connection config.Provider) string {
	key := connection.CatalogProvider
	if key == "" {
		if _, local := login.LocalEntry(connection.ID); local {
			return ""
		}
		key = connection.ID
	}
	if key == "azure" {
		return ""
	}
	return key
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
	models, verified, err := login.Discover(ctx, connection)
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
		if resolved, ok := r.resolveModel(r.active.Name); ok {
			profile := r.withEffort(resolved)
			if profile.Ready() == nil {
				client, err := provider.New(profile.providerSpec(), r.store.ReadImage)
				if err == nil {
					r.active = profile
					r.runner = agent.New(profile.limits(r.config.Compaction), client, r.store, tools.New(r.cwd, profile.Vision))
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
