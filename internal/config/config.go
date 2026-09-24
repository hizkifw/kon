// Package config owns kon's small, versioned user configuration.
package config

import (
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"runtime"
	"slices"
	"strings"

	"github.com/hizkifw/kon/internal/tokens"
)

const filename = "config.json"

type Config struct {
	DefaultModel string `json:"default_model"`
	// ReasoningEffort is the effort last selected for the default model. It is
	// not validated: a level the model does not list falls back to the
	// provider default, so a stale value never blocks startup.
	ReasoningEffort string     `json:"reasoning_effort,omitempty"`
	Providers       []Provider `json:"providers,omitempty"`
	Models          []Model    `json:"models"`
	Compaction      Compaction `json:"compaction"`
	Instructions    string     `json:"instructions"`
	// ContextFiles enables discovery of AGENTS.md and CLAUDE.md files by walking
	// up from the working directory. It is on by default; set it to false to
	// keep the system prompt limited to the built-in rules and instructions.
	ContextFiles *bool `json:"context_files,omitempty"`
}

// Provider is a named connection shared by explicit and discovered models.
type Provider struct {
	ID   string `json:"id"`
	Type string `json:"type"`
	// CatalogProvider is the models.dev key when ID names another connection.
	CatalogProvider string            `json:"catalog_provider,omitempty"`
	BaseURL         string            `json:"base_url,omitempty"`
	APIKey          string            `json:"api_key,omitempty"`
	Headers         map[string]string `json:"headers,omitempty"`
}

// ContextFilesEnabled reports whether AGENTS.md discovery is on. An unset value
// means enabled, so the default config and older configs behave as expected.
func (c Config) ContextFilesEnabled() bool {
	return c.ContextFiles == nil || *c.ContextFiles
}

// Model is a standalone named profile. Name belongs to kon; ModelID belongs to
// the provider. Type is the wire format and never refers to providers[].
type Model struct {
	Name                string            `json:"name"`
	Type                string            `json:"type,omitempty"`
	ModelID             string            `json:"model"`
	BaseURL             string            `json:"base_url,omitempty"`
	APIKey              string            `json:"api_key"`
	Headers             map[string]string `json:"headers,omitempty"`
	ContextWindowTokens tokens.Count      `json:"context_window_tokens"`
	// Vision marks models that accept image content. It gates whether the read
	// tool attaches image parts to its results instead of a text notice.
	Vision bool `json:"vision,omitempty"`
	// Reasoning marks models that produce reasoning. Some servers hold such a
	// model to stricter rules for replayed history; see the provider package.
	Reasoning bool `json:"reasoning,omitempty"`
	// ReasoningEfforts lists the reasoning effort levels the model accepts, in
	// the order Shift+Tab cycles through them. Empty means kon never sends an
	// effort, so servers that reject the parameter keep working.
	ReasoningEfforts []string `json:"reasoning_efforts,omitempty"`
	// ReasoningEffort is the level a resolved profile sends. It is applied from
	// Config.ReasoningEffort at runtime and is never stored per model.
	ReasoningEffort string `json:"-"`
}

type Compaction struct {
	ReserveTokens    tokens.Count `json:"reserve_tokens"`
	KeepRecentTokens tokens.Count `json:"keep_recent_tokens"`
}

// supportedProviders lists the config values kon can serve. Every one of them
// speaks the OpenAI chat completions wire format; other formats (Anthropic,
// Google) return once a backend implements provider.Model.
var supportedProviders = []string{"openai", "openai-compatible", "openrouter", "ollama"}

func SupportedProviders() []string { return slices.Clone(supportedProviders) }

// DefaultModelType is the wire format of a model profile with no type.
const DefaultModelType = "openai-compatible"

// WireType reports the profile's wire format, applying the default.
func (m Model) WireType() string {
	if m.Type == "" {
		return DefaultModelType
	}
	return m.Type
}

// Default configures no model: a first launch waits for /login or /model
// rather than shipping a placeholder profile that cannot run.
func Default() Config {
	return Config{
		Models:     []Model{},
		Compaction: Compaction{ReserveTokens: 16_384, KeepRecentTokens: 20_000},
	}
}

type Paths struct {
	ConfigDir      string
	ConfigFile     string
	DataDir        string
	Sessions       string
	History        string
	Catalog        string
	ProviderModels string
}

func ResolvePaths() (Paths, error) {
	home, err := os.UserHomeDir()
	if err != nil {
		return Paths{}, fmt.Errorf("find home directory: %w", err)
	}
	configRoot, dataRoot := os.Getenv("XDG_CONFIG_HOME"), os.Getenv("XDG_DATA_HOME")
	if runtime.GOOS == "windows" {
		if configRoot == "" {
			configRoot = os.Getenv("APPDATA")
		}
		if dataRoot == "" {
			dataRoot = os.Getenv("LOCALAPPDATA")
		}
	}
	if configRoot == "" {
		configRoot = filepath.Join(home, ".config")
	}
	if dataRoot == "" {
		dataRoot = filepath.Join(home, ".local", "share")
	}
	configDir, dataDir := filepath.Join(configRoot, "kon"), filepath.Join(dataRoot, "kon")
	return Paths{
		ConfigDir: configDir, ConfigFile: filepath.Join(configDir, filename),
		DataDir: dataDir, Sessions: filepath.Join(dataDir, "sessions"),
		History: filepath.Join(dataDir, "history.jsonl"), Catalog: filepath.Join(dataDir, "models.json.gz"),
		ProviderModels: filepath.Join(dataDir, "provider-models.json"),
	}, nil
}

func Initialize(paths Paths) (Config, error) {
	for _, dir := range []string{paths.ConfigDir, paths.DataDir, paths.Sessions} {
		if err := os.MkdirAll(dir, 0o700); err != nil {
			return Config{}, fmt.Errorf("create %s: %w", dir, err)
		}
	}
	if err := ensureConfig(paths.ConfigFile); err != nil {
		return Config{}, err
	}
	if err := ensureFile(paths.History); err != nil {
		return Config{}, err
	}
	return Load(paths.ConfigFile)
}

// Load reads and validates an existing config without creating any files.
func Load(path string) (Config, error) {
	b, err := os.ReadFile(path)
	if err != nil {
		return Config{}, fmt.Errorf("read config %s: %w", path, err)
	}
	cfg := Default()
	dec := json.NewDecoder(strings.NewReader(string(b)))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&cfg); err != nil {
		return Config{}, fmt.Errorf("parse config %s: %w", path, err)
	}
	var extra any
	if err := dec.Decode(&extra); !errors.Is(err, io.EOF) {
		if err == nil {
			return Config{}, fmt.Errorf("parse config %s: multiple JSON values", path)
		}
		return Config{}, fmt.Errorf("parse config %s: %w", path, err)
	}
	if err := cfg.Validate(); err != nil {
		return Config{}, fmt.Errorf("validate config %s: %w", path, err)
	}
	return cfg, nil
}

func ensureConfig(path string) error {
	b, err := json.MarshalIndent(Default(), "", "  ")
	if err != nil {
		return fmt.Errorf("encode default config: %w", err)
	}
	f, err := os.OpenFile(path, os.O_WRONLY|os.O_CREATE|os.O_EXCL, 0o600)
	if errors.Is(err, os.ErrExist) {
		return nil
	}
	if err != nil {
		return fmt.Errorf("create config %s: %w", path, err)
	}
	defer f.Close()
	if _, err := f.Write(append(b, '\n')); err != nil {
		return fmt.Errorf("write config %s: %w", path, err)
	}
	return f.Sync()
}

func ensureFile(path string) error {
	f, err := os.OpenFile(path, os.O_WRONLY|os.O_CREATE, 0o600)
	if err != nil {
		return fmt.Errorf("create %s: %w", path, err)
	}
	return f.Close()
}

func (c Config) Validate() error {
	if c.Compaction.ReserveTokens <= 0 || c.Compaction.KeepRecentTokens <= 0 {
		return errors.New("compaction token budgets must be positive")
	}
	seen := make(map[string]bool, len(c.Models))
	connections := make(map[string]bool, len(c.Providers))
	for i, provider := range c.Providers {
		if !validName(provider.ID) {
			return fmt.Errorf("providers[%d].id must use letters, digits, '.', '_' or '-'", i)
		}
		if connections[provider.ID] {
			return fmt.Errorf("duplicate provider id %q", provider.ID)
		}
		if !slices.Contains(supportedProviders, provider.Type) {
			return fmt.Errorf("provider %q has unsupported type %q", provider.ID, provider.Type)
		}
		if provider.CatalogProvider != "" && !validName(provider.CatalogProvider) {
			return fmt.Errorf("provider %q has invalid catalog_provider", provider.ID)
		}
		if provider.Type == "openai-compatible" && strings.TrimSpace(provider.BaseURL) == "" {
			return fmt.Errorf("provider %q requires base_url", provider.ID)
		}
		connections[provider.ID] = true
	}
	for i, model := range c.Models {
		if !validName(model.Name) {
			return fmt.Errorf("models[%d].name must use letters, digits, '.', '_' or '-'", i)
		}
		if seen[model.Name] {
			return fmt.Errorf("duplicate model name %q", model.Name)
		}
		seen[model.Name] = true
		if !slices.Contains(supportedProviders, model.WireType()) {
			return fmt.Errorf("model %q has unsupported type %q (supported: %s)", model.Name, model.Type, strings.Join(supportedProviders, ", "))
		}
		if model.WireType() == "openai-compatible" && strings.TrimSpace(model.BaseURL) == "" {
			return fmt.Errorf("model %q requires base_url for openai-compatible", model.Name)
		}
		for j, effort := range model.ReasoningEfforts {
			if !validName(effort) {
				return fmt.Errorf("model %q reasoning_efforts[%d] must use letters, digits, '.', '_' or '-'", model.Name, j)
			}
			if slices.Contains(model.ReasoningEfforts[:j], effort) {
				return fmt.Errorf("model %q lists reasoning effort %q twice", model.Name, effort)
			}
		}
		if model.ContextWindowTokens < 0 {
			return fmt.Errorf("model %q context_window_tokens must be non-negative", model.Name)
		}
		if model.ContextWindowTokens > 0 && c.Compaction.ReserveTokens+c.Compaction.KeepRecentTokens >= model.ContextWindowTokens {
			return fmt.Errorf("model %q context window must exceed both compaction budgets", model.Name)
		}
	}
	// An empty default_model is the unconfigured first-run state; the runtime
	// reports it instead of refusing to start.
	if c.DefaultModel != "" && !seen[c.DefaultModel] && !c.DerivedModel(c.DefaultModel) {
		return fmt.Errorf("default_model %q does not name a configured model", c.DefaultModel)
	}
	return nil
}

func (c Config) Provider(id string) (Provider, bool) {
	for _, provider := range c.Providers {
		if provider.ID == id {
			return provider, true
		}
	}
	return Provider{}, false
}

// DerivedModel accepts a qualified provider/model ID without requiring a
// matching catalog entry. Catalogs can lag private or newly released models.
func (c Config) DerivedModel(name string) bool {
	providerID, modelID, ok := strings.Cut(name, "/")
	if !ok || modelID == "" {
		return false
	}
	_, connected := c.Provider(providerID)
	return connected
}

func validName(name string) bool {
	if name == "" {
		return false
	}
	for _, r := range name {
		if (r >= 'a' && r <= 'z') || (r >= 'A' && r <= 'Z') || (r >= '0' && r <= '9') || strings.ContainsRune("._-", r) {
			continue
		}
		return false
	}
	return true
}

func (c Config) Save(path string) error {
	b, err := json.MarshalIndent(c, "", "  ")
	if err != nil {
		return fmt.Errorf("encode config %s: %w", path, err)
	}
	dir := filepath.Dir(path)
	if err := os.MkdirAll(dir, 0o700); err != nil {
		return fmt.Errorf("create config directory %s: %w", dir, err)
	}
	tmp, err := os.CreateTemp(dir, filepath.Base(path)+".tmp*")
	if err != nil {
		return fmt.Errorf("create config %s: %w", path, err)
	}
	tmpName := tmp.Name()
	defer os.Remove(tmpName)
	if err := tmp.Chmod(0o600); err != nil {
		tmp.Close()
		return fmt.Errorf("set permissions on config %s: %w", path, err)
	}
	if _, err := tmp.Write(append(b, '\n')); err != nil {
		tmp.Close()
		return fmt.Errorf("write config %s: %w", path, err)
	}
	if err := tmp.Sync(); err != nil {
		tmp.Close()
		return fmt.Errorf("sync config %s: %w", path, err)
	}
	if err := tmp.Close(); err != nil {
		return fmt.Errorf("close config %s: %w", path, err)
	}
	if err := os.Rename(tmpName, path); err != nil {
		return fmt.Errorf("replace config %s: %w", path, err)
	}
	return nil
}

func (c Config) Model(name string) (Model, bool) {
	for _, model := range c.Models {
		if model.Name == name {
			return model, true
		}
	}
	return Model{}, false
}

// ResolveModel returns an explicit profile as written, with its wire type
// defaulted, or builds a profile from a qualified provider/model ID using
// that provider's connection. Explicit profiles never inherit from providers.
func (c Config) ResolveModel(name string) (Model, bool) {
	if model, ok := c.Model(name); ok {
		model.Type = model.WireType()
		return model, true
	}
	providerID, modelID, ok := strings.Cut(name, "/")
	if !ok || modelID == "" {
		return Model{}, false
	}
	connection, found := c.Provider(providerID)
	if !found {
		return Model{}, false
	}
	return Model{
		Name: name, Type: connection.Type, ModelID: modelID,
		BaseURL: connection.BaseURL, APIKey: connection.APIKey, Headers: connection.Headers,
	}, true
}

func (m Model) Ready() error {
	if m.Name == "" {
		// The UI appends the config path, so this reads as a complete hint.
		return errors.New("no model configured; run /login, or add a model")
	}
	if strings.TrimSpace(m.ModelID) == "" {
		return fmt.Errorf("configure model %q", m.Name)
	}
	return nil
}
