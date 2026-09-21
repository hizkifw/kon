package config

import (
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestInitializeAndCredential(t *testing.T) {
	root := t.TempDir()
	t.Setenv("XDG_CONFIG_HOME", filepath.Join(root, "config"))
	t.Setenv("XDG_DATA_HOME", filepath.Join(root, "data"))
	paths, err := ResolvePaths()
	if err != nil {
		t.Fatal(err)
	}
	cfg, err := Initialize(paths)
	if err != nil {
		t.Fatal(err)
	}
	model, ok := cfg.Model(cfg.DefaultModel)
	if !ok {
		t.Fatal("default model missing")
	}
	if model.APIKey != "" {
		t.Fatalf("default model APIKey = %q", model.APIKey)
	}
	model.APIKey = "literal"
	if model.APIKey != "literal" {
		t.Fatalf("APIKey not preserved")
	}
	info, err := os.Stat(paths.ConfigFile)
	if err != nil {
		t.Fatal(err)
	}
	if info.Mode().Perm()&0o077 != 0 {
		t.Fatalf("config permissions are %o", info.Mode().Perm())
	}
	b, err := os.ReadFile(paths.ConfigFile)
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(string(b), `"api_key"`) {
		t.Fatalf("default config missing api_key field:\n%s", b)
	}
}

func TestInitializePreservesInvalidConfig(t *testing.T) {
	root := t.TempDir()
	paths := Paths{
		ConfigDir: filepath.Join(root, "config"), ConfigFile: filepath.Join(root, "config", filename),
		DataDir: filepath.Join(root, "data"), Sessions: filepath.Join(root, "data", "sessions"), History: filepath.Join(root, "data", "history.jsonl"),
	}
	if err := os.MkdirAll(paths.ConfigDir, 0o700); err != nil {
		t.Fatal(err)
	}
	const invalid = `{"unknown":true}`
	if err := os.WriteFile(paths.ConfigFile, []byte(invalid), 0o600); err != nil {
		t.Fatal(err)
	}
	if _, err := Initialize(paths); err == nil {
		t.Fatal("expected invalid config error")
	}
	b, err := os.ReadFile(paths.ConfigFile)
	if err != nil {
		t.Fatal(err)
	}
	if string(b) != invalid {
		t.Fatal("invalid config was rewritten")
	}
}

func TestSaveRewritesDefaultModel(t *testing.T) {
	cfg := Default()
	cfg.Models = append(cfg.Models, Model{Name: "review", Provider: "openai", ModelID: "gpt-4o", ContextWindowTokens: 100_000})
	path := filepath.Join(t.TempDir(), filename)
	if err := cfg.Save(path); err != nil {
		t.Fatal(err)
	}
	info, err := os.Stat(path)
	if err != nil {
		t.Fatal(err)
	}
	if info.Mode().Perm()&0o077 != 0 {
		t.Fatalf("config permissions are %o", info.Mode().Perm())
	}
	cfg.DefaultModel = "review"
	if err := cfg.Save(path); err != nil {
		t.Fatal(err)
	}
	b, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	var reread Config
	if err := json.Unmarshal(b, &reread); err != nil {
		t.Fatal(err)
	}
	if reread.DefaultModel != "review" {
		t.Fatalf("default_model = %q", reread.DefaultModel)
	}
}

func TestValidateNamedModels(t *testing.T) {
	cfg := Default()
	cfg.DefaultModel = "review"
	cfg.Models = append(cfg.Models, Model{Name: "review", Provider: "openai", ModelID: "gpt-4o", ContextWindowTokens: 100_000})
	if err := cfg.Validate(); err != nil {
		t.Fatal(err)
	}
	if model, ok := cfg.Model("review"); !ok || model.ModelID != "gpt-4o" {
		t.Fatalf("Model(review) = %#v, %v", model, ok)
	}
	cfg.Models[1].Name = "default"
	if err := cfg.Validate(); err == nil {
		t.Fatal("duplicate model name was accepted")
	}
}

func TestCompatibleModelRequiresBaseURL(t *testing.T) {
	cfg := Default()
	cfg.Models[0].Provider = "openai-compatible"
	cfg.Models[0].BaseURL = ""
	if err := cfg.Validate(); err == nil {
		t.Fatal("missing base URL was accepted")
	}
}
