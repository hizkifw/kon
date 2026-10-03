package config

import (
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"kon.kitsu.red/core/session"
)

// TestConfigFilesArePrivate covers both ways kon writes its config: the
// first-run file from Initialize, and a Save that replaces it. The config holds
// API keys and the history holds past prompts, so only the owner may read them.
func TestConfigFilesArePrivate(t *testing.T) {
	root := t.TempDir()
	t.Setenv("XDG_CONFIG_HOME", filepath.Join(root, "config"))
	t.Setenv("XDG_DATA_HOME", filepath.Join(root, "data"))
	paths, err := ResolvePaths()
	if err != nil {
		t.Fatal(err)
	}
	private := func(path string) {
		t.Helper()
		info, err := os.Stat(path)
		if err != nil {
			t.Fatal(err)
		}
		if perm := info.Mode().Perm(); perm != 0o600 {
			t.Fatalf("%s permissions are %o, want 600", filepath.Base(path), perm)
		}
	}
	cfg, err := Initialize(paths)
	if err != nil {
		t.Fatal(err)
	}
	private(paths.ConfigFile)
	private(paths.History)

	// Loosen the file first, so the check shows that Save sets the mode itself
	// rather than keeping the mode of the file it replaces.
	if err := os.Chmod(paths.ConfigFile, 0o644); err != nil {
		t.Fatal(err)
	}
	if err := cfg.Save(paths.ConfigFile); err != nil {
		t.Fatal(err)
	}
	private(paths.ConfigFile)
}

// TestInitializeWritesEmptyModelsList checks that the first-run file lists
// models as an empty array rather than omitting it, since that array is where
// a profile added by hand goes.
func TestInitializeWritesEmptyModelsList(t *testing.T) {
	root := t.TempDir()
	t.Setenv("XDG_CONFIG_HOME", filepath.Join(root, "config"))
	t.Setenv("XDG_DATA_HOME", filepath.Join(root, "data"))
	paths, err := ResolvePaths()
	if err != nil {
		t.Fatal(err)
	}
	if _, err := Initialize(paths); err != nil {
		t.Fatal(err)
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

func TestValidateInputs(t *testing.T) {
	cfg := testConfig()
	cfg.Models[0].Inputs = []session.Modality{session.ModalityImage, session.ModalityAudio}
	if err := cfg.Validate(); err != nil {
		t.Fatal(err)
	}
	for _, inputs := range [][]session.Modality{{"text"}, {"image", "image"}} {
		cfg.Models[0].Inputs = inputs
		if err := cfg.Validate(); err == nil {
			t.Fatalf("inputs %v were accepted", inputs)
		}
	}
}

func TestModelCostMustNotBeNegative(t *testing.T) {
	cfg := Default()
	cfg.Models = []Model{{Name: "priced", ModelID: "m", BaseURL: "http://localhost", Cost: Cost{Input: 1, Output: 2, CacheRead: -0.1}}}
	if err := cfg.Validate(); err == nil || !strings.Contains(err.Error(), "cost") {
		t.Fatalf("negative cache read price: err = %v", err)
	}
	cfg.Models[0].Cost.CacheRead = 0.1
	if err := cfg.Validate(); err != nil {
		t.Fatal(err)
	}
}

// TestModelCostRoundTrips checks the cost field uses models.dev's names and
// stays out of a saved profile that has none.
func TestModelCostRoundTrips(t *testing.T) {
	path := filepath.Join(t.TempDir(), "config.json")
	cfg := Default()
	cfg.Models = []Model{
		{Name: "priced", ModelID: "m", BaseURL: "http://localhost", Cost: Cost{Input: 3, Output: 15, CacheRead: 0.3, CacheWrite: 3.75}},
		{Name: "free", ModelID: "m", BaseURL: "http://localhost"},
	}
	if err := cfg.Save(path); err != nil {
		t.Fatal(err)
	}
	raw, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(string(raw), `"cache_write": 3.75`) || strings.Count(string(raw), `"cost"`) != 1 {
		t.Fatalf("saved config:\n%s", raw)
	}
	loaded, err := Load(path)
	if err != nil {
		t.Fatal(err)
	}
	if loaded.Models[0].Cost != cfg.Models[0].Cost || loaded.Models[1].Cost != (Cost{}) {
		t.Fatalf("loaded costs = %+v, %+v", loaded.Models[0].Cost, loaded.Models[1].Cost)
	}
}

func TestContextWindowMustExceedCompactionBudgets(t *testing.T) {
	cfg := testConfig()
	cfg.Compaction = Compaction{ReserveTokens: 16_384, KeepRecentTokens: 20_000}
	budgets := cfg.Compaction.ReserveTokens + cfg.Compaction.KeepRecentTokens
	cfg.Models[0].ContextWindowTokens = budgets + 1
	if err := cfg.Validate(); err != nil {
		t.Fatal(err)
	}
	// Compaction starts once the context eats into the reserve and keeps the
	// recent budget, so a window no larger than both would compact without
	// ever getting back under the trigger.
	cfg.Models[0].ContextWindowTokens = budgets
	if err := cfg.Validate(); err == nil {
		t.Fatal("context window equal to the compaction budgets was accepted")
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
	if err := cfg.Validate(); err != nil {
		t.Fatal(err)
	}
	// catalog_provider is one of the names kon owns, which the schema limits to
	// letters, digits, '.', '_' and '-', like the connection ID beside it.
	cfg.Providers[0].CatalogProvider = "fireworks ai"
	if err := cfg.Validate(); err == nil {
		t.Fatal("invalid catalog_provider was accepted")
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

// The budgets are left out of a new config, so kon derives them from each
// model's window and a later release can tune the defaults for everyone.
func TestNewConfigLeavesCompactionDerived(t *testing.T) {
	path := filepath.Join(t.TempDir(), "config.json")
	if err := ensureConfig(path); err != nil {
		t.Fatal(err)
	}
	b, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	if strings.Contains(string(b), "compaction") {
		t.Fatalf("new config pins compaction budgets:\n%s", b)
	}
	cfg := testConfig()
	cfg.Compaction.KeepRecentTokens = -1
	if err := cfg.Validate(); err == nil {
		t.Fatal("a negative compaction budget was accepted")
	}
}

func TestWebSearchIsOffByDefault(t *testing.T) {
	cfg := Default()
	if cfg.WebSearch.Enabled() {
		t.Fatal("web search is on in the default config")
	}
	b, err := json.Marshal(cfg)
	if err != nil || strings.Contains(string(b), "web_search") {
		t.Fatalf("default config = %s, err = %v, want no web_search", b, err)
	}
}

func TestValidateWebSearch(t *testing.T) {
	brave := map[string]WebSearchProvider{"brave": {APIKey: "key"}}
	for _, test := range []struct {
		name   string
		search WebSearch
		want   string
	}{
		{"selected with a key", WebSearch{Provider: "brave", Providers: brave}, ""},
		{"off with connections kept", WebSearch{Providers: map[string]WebSearchProvider{"brave": {}, "searxng": {}}}, ""},
		{"keyless provider without an entry", WebSearch{Provider: "duckduckgo"}, ""},
		{"self-hosted with a base URL", WebSearch{Provider: "searxng", Providers: map[string]WebSearchProvider{"searxng": {BaseURL: "http://localhost:8888"}}}, ""},
		{"unknown selection", WebSearch{Provider: "bogus"}, `web_search.provider: unknown web search provider "bogus"`},
		{"unknown entry", WebSearch{Providers: map[string]WebSearchProvider{"bogus": {}}}, "web_search.providers: unknown"},
		{"selected without a key", WebSearch{Provider: "brave"}, "web_search.providers.brave requires api_key"},
		{"self-hosted without a base URL", WebSearch{Provider: "searxng"}, "web_search.providers.searxng requires base_url"},
	} {
		cfg := testConfig()
		cfg.WebSearch = test.search
		err := cfg.Validate()
		if test.want == "" && err != nil {
			t.Errorf("%s: %v", test.name, err)
		}
		if test.want != "" && (err == nil || !strings.Contains(err.Error(), test.want)) {
			t.Errorf("%s: error = %v, want it to contain %q", test.name, err, test.want)
		}
	}
}

func TestWebSearchConnectionIsTheSelectedProviders(t *testing.T) {
	search := WebSearch{Provider: "exa", Providers: map[string]WebSearchProvider{"brave": {APIKey: "b"}, "exa": {APIKey: "e", BaseURL: "http://proxy"}}}
	if conn := search.Connection(); conn.APIKey != "e" || conn.BaseURL != "http://proxy" {
		t.Fatalf("connection = %+v, want exa's", conn)
	}
}
