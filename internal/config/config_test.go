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
	if cfg.DefaultModel != "" || len(cfg.Models) != 0 {
		t.Fatalf("default config ships a model: %#v", cfg)
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
	if !strings.Contains(string(b), `"models": []`) {
		t.Fatalf("default config missing empty models list:\n%s", b)
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
	cfg := testConfig()
	cfg.Models = append(cfg.Models, Model{Name: "review", Type: "openai", ModelID: "gpt-4o", ContextWindowTokens: 100_000})
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

func TestContextFilesEnabledDefaultsTrue(t *testing.T) {
	if !Default().ContextFilesEnabled() {
		t.Fatal("context files should be enabled by default")
	}
	disabled := false
	cfg := Default()
	cfg.ContextFiles = &disabled
	if cfg.ContextFilesEnabled() {
		t.Fatal("context files should honor an explicit false")
	}
}

// testConfig returns the default config with one explicit profile, the shape
// most validation tests start from.
func testConfig() Config {
	cfg := Default()
	cfg.DefaultModel = "default"
	cfg.Models = []Model{{Name: "default", Type: "openai-compatible", BaseURL: "https://api.openai.com/v1"}}
	return cfg
}

func TestEmptyDefaultIsValid(t *testing.T) {
	if err := Default().Validate(); err != nil {
		t.Fatal(err)
	}
	cfg := Default()
	cfg.DefaultModel = "missing"
	if err := cfg.Validate(); err == nil {
		t.Fatal("default_model naming no model was accepted")
	}
}

func TestLoadKeepsAnOmittedModelsListEmpty(t *testing.T) {
	path := filepath.Join(t.TempDir(), filename)
	if err := os.WriteFile(path, []byte(`{"compaction":{"reserve_tokens":1,"keep_recent_tokens":1}}`), 0o600); err != nil {
		t.Fatal(err)
	}
	cfg, err := Load(path)
	if err != nil {
		t.Fatal(err)
	}
	if len(cfg.Models) != 0 {
		t.Fatalf("models = %#v", cfg.Models)
	}
}

func TestValidateNamedModels(t *testing.T) {
	cfg := testConfig()
	cfg.DefaultModel = "review"
	cfg.Models = append(cfg.Models, Model{Name: "review", Type: "openai", ModelID: "gpt-4o", ContextWindowTokens: 100_000})
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

func TestValidateReasoningEfforts(t *testing.T) {
	cfg := testConfig()
	cfg.Models[0].ReasoningEfforts = []string{"low", "high"}
	// A saved effort the model does not list is ignored at runtime, not rejected.
	cfg.ReasoningEffort = "max"
	if err := cfg.Validate(); err != nil {
		t.Fatal(err)
	}
	cfg.Models[0].ReasoningEfforts = []string{"low", "low"}
	if err := cfg.Validate(); err == nil {
		t.Fatal("duplicate reasoning effort was accepted")
	}
}

func TestCompatibleModelRequiresBaseURL(t *testing.T) {
	cfg := testConfig()
	cfg.Models[0].Type = "openai-compatible"
	cfg.Models[0].BaseURL = ""
	if err := cfg.Validate(); err == nil {
		t.Fatal("missing base URL was accepted")
	}
}

func TestProviderConnectionResolvesDerivedModels(t *testing.T) {
	cfg := Default()
	cfg.Providers = []Provider{{ID: "work", Type: "openai-compatible", BaseURL: "https://example.test/v1", APIKey: "secret"}}
	cfg.DefaultModel = "work/private/model"
	if err := cfg.Validate(); err != nil {
		t.Fatal(err)
	}
	model, ok := cfg.ResolveModel("work/private/model")
	if !ok || model.Type != "openai-compatible" || model.ModelID != "private/model" || model.BaseURL != "https://example.test/v1" || model.APIKey != "secret" {
		t.Fatalf("ResolveModel = %#v, %v", model, ok)
	}
	if _, ok := cfg.ResolveModel("missing/model"); ok {
		t.Fatal("accepted an unconfigured provider")
	}
}

func TestExplicitModelIgnoresSameNamedProvider(t *testing.T) {
	cfg := testConfig()
	cfg.Providers = []Provider{{ID: "openai-compatible", Type: "openai-compatible", BaseURL: "https://remote.test/v1", APIKey: "remote", Headers: map[string]string{"X-Org": "a"}}}
	cfg.Models = append(cfg.Models, Model{Name: "local", Type: "openai-compatible", ModelID: "m", BaseURL: "http://localhost:8080/v1"})
	if err := cfg.Validate(); err != nil {
		t.Fatal(err)
	}
	model, ok := cfg.ResolveModel("local")
	if !ok || model.BaseURL != "http://localhost:8080/v1" || model.APIKey != "" || model.Headers != nil {
		t.Fatalf("explicit model inherited a connection: %#v", model)
	}
}

func TestModelTypeDefaultsToCompatible(t *testing.T) {
	cfg := testConfig()
	cfg.Models = append(cfg.Models, Model{Name: "local", ModelID: "m"})
	if err := cfg.Validate(); err == nil {
		t.Fatal("untyped model without base_url was accepted")
	}
	cfg.Models[1].BaseURL = "http://localhost:8080/v1"
	if err := cfg.Validate(); err != nil {
		t.Fatal(err)
	}
	if model, _ := cfg.ResolveModel("local"); model.Type != "openai-compatible" {
		t.Fatalf("type = %q", model.Type)
	}
}

func TestModelProviderFieldIsRejected(t *testing.T) {
	path := filepath.Join(t.TempDir(), filename)
	legacy := `{"default_model":"fast","models":[{"name":"fast","provider":"openai","model":"gpt"}],"compaction":{"reserve_tokens":1,"keep_recent_tokens":1}}`
	if err := os.WriteFile(path, []byte(legacy), 0o600); err != nil {
		t.Fatal(err)
	}
	if _, err := Load(path); err == nil || !strings.Contains(err.Error(), "provider") {
		t.Fatalf("legacy provider field: %v", err)
	}
}

func TestAliasedCatalogProviderConnection(t *testing.T) {
	cfg := Default()
	cfg.Providers = []Provider{{ID: "fireworks-2", CatalogProvider: "fireworks-ai", Type: "openai-compatible", BaseURL: "https://api.fireworks.ai/inference/v1", APIKey: "secret"}}
	cfg.DefaultModel = "fireworks-2/accounts/acme/models/example"
	if err := cfg.Validate(); err != nil {
		t.Fatal(err)
	}
	model, ok := cfg.ResolveModel(cfg.DefaultModel)
	if !ok || model.Type != "openai-compatible" || model.ModelID != "accounts/acme/models/example" || model.APIKey != "secret" {
		t.Fatalf("resolved = %#v, %v", model, ok)
	}
}

func TestDuplicateProviderIDsAreRejected(t *testing.T) {
	cfg := Default()
	cfg.Providers = []Provider{{ID: "openai", Type: "openai"}, {ID: "openai", Type: "openrouter"}}
	if err := cfg.Validate(); err == nil {
		t.Fatal("accepted duplicate provider IDs")
	}
}

func TestProviderOnlyConfigCanSelectDerivedDefault(t *testing.T) {
	cfg := Default()
	cfg.Models = nil
	cfg.Providers = []Provider{{ID: "openai", Type: "openai", APIKey: "secret"}}
	cfg.DefaultModel = "openai/private/model"
	if err := cfg.Validate(); err != nil {
		t.Fatal(err)
	}
	path := filepath.Join(t.TempDir(), filename)
	if err := cfg.Save(path); err != nil {
		t.Fatal(err)
	}
	loaded, err := Load(path)
	if err != nil {
		t.Fatal(err)
	}
	model, ok := loaded.ResolveModel(loaded.DefaultModel)
	if !ok || model.Type != "openai" || model.ModelID != "private/model" {
		t.Fatalf("derived default = %#v, %v", model, ok)
	}
}
