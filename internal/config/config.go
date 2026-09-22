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
)

const filename = "config.json"

type Config struct {
	DefaultModel string     `json:"default_model"`
	Models       []Model    `json:"models"`
	Compaction   Compaction `json:"compaction"`
	Instructions string     `json:"instructions"`
	// ContextFiles enables discovery of AGENTS.md and CLAUDE.md files by walking
	// up from the working directory. It is on by default; set it to false to
	// keep the system prompt limited to the built-in rules and instructions.
	ContextFiles *bool `json:"context_files,omitempty"`
}

// ContextFilesEnabled reports whether AGENTS.md discovery is on. An unset value
// means enabled, so the default config and older configs behave as expected.
func (c Config) ContextFilesEnabled() bool {
	return c.ContextFiles == nil || *c.ContextFiles
}

// Model is a named profile. Name belongs to kon; ModelID belongs to the provider.
type Model struct {
	Name                string            `json:"name"`
	Provider            string            `json:"provider"`
	ModelID             string            `json:"model"`
	BaseURL             string            `json:"base_url,omitempty"`
	APIKey              string            `json:"api_key"`
	Headers             map[string]string `json:"headers,omitempty"`
	ContextWindowTokens int               `json:"context_window_tokens"`
	// Vision marks models that accept image content. It gates whether the read
	// tool attaches image parts to its results instead of a text notice.
	Vision bool `json:"vision,omitempty"`
}

type Compaction struct {
	ReserveTokens    int `json:"reserve_tokens"`
	KeepRecentTokens int `json:"keep_recent_tokens"`
}

// supportedProviders lists the config values kon can serve. Every one of them
// speaks the OpenAI chat completions wire format; other formats (Anthropic,
// Google) return once a backend implements provider.Model.
var supportedProviders = []string{"openai", "openai-compatible", "openrouter", "ollama"}

func Default() Config {
	return Config{
		DefaultModel: "default",
		Models: []Model{{
			Name: "default", Provider: "openai", BaseURL: "https://api.openai.com/v1",
		}},
		Compaction: Compaction{ReserveTokens: 16_384, KeepRecentTokens: 20_000},
	}
}

type Paths struct {
	ConfigDir  string
	ConfigFile string
	DataDir    string
	Sessions   string
	History    string
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
	return Paths{configDir, filepath.Join(configDir, filename), dataDir, filepath.Join(dataDir, "sessions"), filepath.Join(dataDir, "history.jsonl")}, nil
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
	b, err := os.ReadFile(paths.ConfigFile)
	if err != nil {
		return Config{}, fmt.Errorf("read config %s: %w", paths.ConfigFile, err)
	}
	cfg := Default()
	dec := json.NewDecoder(strings.NewReader(string(b)))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&cfg); err != nil {
		return Config{}, fmt.Errorf("parse config %s: %w", paths.ConfigFile, err)
	}
	var extra any
	if err := dec.Decode(&extra); !errors.Is(err, io.EOF) {
		if err == nil {
			return Config{}, fmt.Errorf("parse config %s: multiple JSON values", paths.ConfigFile)
		}
		return Config{}, fmt.Errorf("parse config %s: %w", paths.ConfigFile, err)
	}
	if err := cfg.Validate(); err != nil {
		return Config{}, fmt.Errorf("validate config %s: %w", paths.ConfigFile, err)
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
	if strings.TrimSpace(c.DefaultModel) == "" {
		return errors.New("default_model must not be empty")
	}
	if len(c.Models) == 0 {
		return errors.New("models must contain at least one profile")
	}
	if c.Compaction.ReserveTokens <= 0 || c.Compaction.KeepRecentTokens <= 0 {
		return errors.New("compaction token budgets must be positive")
	}
	seen := make(map[string]bool, len(c.Models))
	for i, model := range c.Models {
		if !validName(model.Name) {
			return fmt.Errorf("models[%d].name must use letters, digits, '.', '_' or '-'", i)
		}
		if seen[model.Name] {
			return fmt.Errorf("duplicate model name %q", model.Name)
		}
		seen[model.Name] = true
		if !slices.Contains(supportedProviders, model.Provider) {
			return fmt.Errorf("model %q has unsupported provider %q (supported: %s)", model.Name, model.Provider, strings.Join(supportedProviders, ", "))
		}
		if model.Provider == "openai-compatible" && strings.TrimSpace(model.BaseURL) == "" {
			return fmt.Errorf("model %q requires base_url for openai-compatible", model.Name)
		}
		if model.ContextWindowTokens < 0 {
			return fmt.Errorf("model %q context_window_tokens must be non-negative", model.Name)
		}
		if model.ContextWindowTokens > 0 && c.Compaction.ReserveTokens+c.Compaction.KeepRecentTokens >= model.ContextWindowTokens {
			return fmt.Errorf("model %q context window must exceed both compaction budgets", model.Name)
		}
	}
	if !seen[c.DefaultModel] {
		return fmt.Errorf("default_model %q does not name a configured model", c.DefaultModel)
	}
	return nil
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

func (m Model) Ready() error {
	if strings.TrimSpace(m.ModelID) == "" {
		return fmt.Errorf("configure model %q", m.Name)
	}
	return nil
}
