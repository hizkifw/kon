package app

import (
	"context"
	"encoding/json"
	"errors"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"sync/atomic"
	"testing"

	"github.com/hizkifw/kon/internal/agent"
	"github.com/hizkifw/kon/internal/config"
	"github.com/hizkifw/kon/internal/provider"
	"github.com/hizkifw/kon/internal/session"
	"github.com/hizkifw/kon/internal/tools"
	"github.com/hizkifw/kon/internal/typedid"
)

type blockingProvider struct{ started chan struct{} }

func (p *blockingProvider) Stream(ctx context.Context, _ []session.Message, _ []provider.Tool, _ func(provider.Event)) (session.Message, error) {
	close(p.started)
	<-ctx.Done()
	return session.Message{}, ctx.Err()
}

func (*blockingProvider) Complete(context.Context, []session.Message, []provider.Tool, int) (session.Message, error) {
	return session.Message{}, errors.New("unexpected completion")
}

func TestNewSessionFailureKeepsCurrentSession(t *testing.T) {
	store := testStore(t)
	runner := new(agent.Runner)
	runtime := &Runtime{
		active: config.Model{Name: "default", Type: "openai", ModelID: "model"},
		store:  store, runner: runner,
		createSession: func() (*session.Store, error) { return nil, errors.New("disk full") },
	}
	if err := runtime.NewSession(); err == nil {
		t.Fatal("expected replacement failure")
	}
	if runtime.store != store || runtime.runner != runner {
		t.Fatal("live session changed after replacement failure")
	}
	if _, err := store.AppendMessage(session.Message{Role: session.RoleUser, Parts: []session.Part{{Type: session.PartText, Text: "still usable"}}}); err != nil {
		t.Fatalf("old session is unusable: %v", err)
	}
}

func TestNewSessionSwapsThenClosesPreviousStore(t *testing.T) {
	previous := testStore(t)
	replacement := testStore(t)
	runtime := &Runtime{
		active: config.Model{Name: "default", Type: "openai", ModelID: "model"},
		store:  previous, runner: new(agent.Runner),
		createSession: func() (*session.Store, error) { return replacement, nil },
		createRunner:  func(config.Model, *session.Store) (*agent.Runner, error) { return new(agent.Runner), nil },
	}
	if err := runtime.NewSession(); err != nil {
		t.Fatal(err)
	}
	if runtime.store != replacement {
		t.Fatal("replacement store was not installed")
	}
	if _, err := previous.AppendMessage(session.Message{Role: session.RoleUser, Parts: []session.Part{{Type: session.PartText, Text: "closed"}}}); err == nil {
		t.Fatal("previous store remained open")
	}
}

func TestUnconfiguredDefaultIsExplicitState(t *testing.T) {
	cfg := config.Default()
	paths := config.Paths{Sessions: t.TempDir(), ConfigFile: t.TempDir() + "/config.json"}
	runtime, err := New(cfg, paths, t.TempDir(), "test")
	if err != nil {
		t.Fatal(err)
	}
	defer runtime.Close()
	state := runtime.State()
	if state.Ready() || state.Phase != PhaseNeedsConfiguration || state.Problem == nil || state.Active.Name != "default" {
		t.Fatalf("unexpected state: %#v", state)
	}
}

func TestLoginAddsDerivedModelWithoutMaterializingIt(t *testing.T) {
	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if r.Header.Get("Authorization") != "Bearer secret" {
			w.WriteHeader(http.StatusUnauthorized)
			return
		}
		w.Write([]byte(`{"data":[{"id":"private/model"}]}`))
	}))
	defer server.Close()
	root := t.TempDir()
	paths := config.Paths{
		ConfigFile:     filepath.Join(root, "config.json"),
		Sessions:       filepath.Join(root, "sessions"),
		ProviderModels: filepath.Join(root, "provider-models.json"),
	}
	if err := config.Default().Save(paths.ConfigFile); err != nil {
		t.Fatal(err)
	}
	runtime, err := New(config.Default(), paths, t.TempDir(), "test")
	if err != nil {
		t.Fatal(err)
	}
	defer runtime.Close()
	count, verified, err := runtime.Login(context.Background(), config.Provider{ID: "openai", Type: "openai", BaseURL: server.URL, APIKey: "secret"})
	if err != nil || !verified || count != 1 {
		t.Fatalf("login: count=%d verified=%v error=%v", count, verified, err)
	}
	const derived = "openai/private/model"
	found := false
	for _, model := range runtime.Models() {
		if model.Name == derived {
			if model.Source != "provider list" {
				t.Fatalf("source = %q", model.Source)
			}
			found = true
		}
	}
	if !found {
		t.Fatal("discovered model missing from picker")
	}
	if err := runtime.SwitchModel(derived); err != nil {
		t.Fatal(err)
	}
	saved, err := config.Load(paths.ConfigFile)
	if err != nil {
		t.Fatal(err)
	}
	if saved.DefaultModel != derived || len(saved.Models) != 1 || saved.Models[0].Name != "default" {
		t.Fatalf("derived selection was materialized: %#v", saved)
	}
	if len(loadProviderModels(paths.ProviderModels)["openai"]) != 1 {
		t.Fatal("provider discovery was not cached")
	}
	reopened, err := New(saved, paths, t.TempDir(), "test")
	if err != nil {
		t.Fatal(err)
	}
	defer reopened.Close()
	found = false
	for _, model := range reopened.Models() {
		if model.Name == derived {
			found = true
		}
	}
	if !found {
		t.Fatal("derived model was not available offline after restart")
	}
}

func TestLoginProvidersComeFromCatalog(t *testing.T) {
	root := t.TempDir()
	paths := config.Paths{ConfigFile: filepath.Join(root, "config.json"), Sessions: filepath.Join(root, "sessions")}
	runtime, err := New(config.Default(), paths, t.TempDir(), "test")
	if err != nil {
		t.Fatal(err)
	}
	defer runtime.Close()
	if runtime.catalog.Load() != nil {
		t.Fatal("catalog loaded at startup")
	}
	ids := runtime.LoginProviders()
	for _, id := range []string{"fireworks-ai", "deepinfra", "ai21", "openai", "ollama"} {
		if !slices.Contains(ids, id) {
			t.Fatalf("%q missing from login options", id)
		}
	}
	connection, ok := runtime.LoginConnection("fireworks-ai")
	if !ok || connection.BaseURL != "https://api.fireworks.ai/inference/v1" {
		t.Fatalf("catalog connection = %#v, %v", connection, ok)
	}
	if _, ok := runtime.LoginConnection("does-not-exist"); ok {
		t.Fatal("unknown catalog provider was accepted")
	}
}

func TestCatalogModelUsesDisplayNameWithConnectionID(t *testing.T) {
	root := t.TempDir()
	paths := config.Paths{ConfigFile: filepath.Join(root, "config.json"), Sessions: filepath.Join(root, "sessions")}
	cfg := config.Default()
	cfg.Providers = []config.Provider{{ID: "fireworks-2", CatalogProvider: "fireworks-ai", Type: "openai-compatible", BaseURL: "https://api.fireworks.ai/inference/v1"}}
	runtime, err := New(cfg, paths, t.TempDir(), "test")
	if err != nil {
		t.Fatal(err)
	}
	defer runtime.Close()
	const name = "fireworks-2/accounts/fireworks/models/deepseek-v4-pro"
	for _, model := range runtime.Models() {
		if model.Name == name {
			if model.ConnectionID != "fireworks-2" || model.DisplayName != "DeepSeek V4 Pro" || model.ExternalID != "accounts/fireworks/models/deepseek-v4-pro" {
				t.Fatalf("model = %#v", model)
			}
			return
		}
	}
	t.Fatalf("%s missing from model list", name)
}

func TestLoadCatalogPopulatesActiveDerivedModel(t *testing.T) {
	root := t.TempDir()
	paths := config.Paths{ConfigFile: filepath.Join(root, "config.json"), Sessions: filepath.Join(root, "sessions")}
	cfg := config.Default()
	cfg.Providers = []config.Provider{{ID: "fireworks-2", CatalogProvider: "fireworks-ai", Type: "openai-compatible", BaseURL: "https://api.fireworks.ai/inference/v1"}}
	cfg.DefaultModel = "fireworks-2/accounts/fireworks/models/deepseek-v4-pro"
	runtime, err := New(cfg, paths, t.TempDir(), "test")
	if err != nil {
		t.Fatal(err)
	}
	defer runtime.Close()
	before := runtime.State().Active
	if runtime.catalog.Load() != nil {
		t.Fatal("State loaded the catalog")
	}
	if before.ContextWindow != 0 || before.DisplayName != "accounts/fireworks/models/deepseek-v4-pro" || before.ConnectionID != "fireworks-2" {
		t.Fatalf("before catalog = %#v", before)
	}
	runtime.LoadCatalog()
	after := runtime.State().Active
	if after.ContextWindow <= 0 || after.DisplayName != "DeepSeek V4 Pro" || after.Name != cfg.DefaultModel {
		t.Fatalf("after catalog = %#v", after)
	}
	if !runtime.State().Ready() {
		t.Fatal("runtime is not ready after catalog load")
	}
}

func TestFailedLoginDoesNotSaveProvider(t *testing.T) {
	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusUnauthorized)
	}))
	defer server.Close()
	root := t.TempDir()
	paths := config.Paths{ConfigFile: filepath.Join(root, "config.json"), Sessions: filepath.Join(root, "sessions")}
	if err := config.Default().Save(paths.ConfigFile); err != nil {
		t.Fatal(err)
	}
	runtime, err := New(config.Default(), paths, t.TempDir(), "test")
	if err != nil {
		t.Fatal(err)
	}
	defer runtime.Close()
	if _, _, err := runtime.Login(context.Background(), config.Provider{ID: "openai", Type: "openai", BaseURL: server.URL, APIKey: "bad"}); err == nil {
		t.Fatal("invalid key accepted")
	}
	saved, err := config.Load(paths.ConfigFile)
	if err != nil {
		t.Fatal(err)
	}
	if len(saved.Providers) != 0 {
		t.Fatalf("failed login changed config: %#v", saved.Providers)
	}
}

func TestConfiguredProviderDoesNotTriggerStartupNetwork(t *testing.T) {
	var requests atomic.Int32
	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		requests.Add(1)
		w.Write([]byte(`{"data":[]}`))
	}))
	defer server.Close()
	cfg := config.Default()
	cfg.Providers = []config.Provider{{ID: "openai", Type: "openai", BaseURL: server.URL, APIKey: "secret"}}
	cfg.DefaultModel = "openai/private/model"
	root := t.TempDir()
	paths := config.Paths{ConfigFile: filepath.Join(root, "config.json"), Sessions: filepath.Join(root, "sessions")}
	runtime, err := New(cfg, paths, t.TempDir(), "test")
	if err != nil {
		t.Fatal(err)
	}
	defer runtime.Close()
	_ = runtime.Models()
	if got := requests.Load(); got != 0 {
		t.Fatalf("startup or model listing made %d provider requests", got)
	}
}

func TestNewPersistsDiscoveredContextFiles(t *testing.T) {
	workspace := t.TempDir()
	if err := os.WriteFile(filepath.Join(workspace, "AGENTS.md"), []byte("workspace rules"), 0o644); err != nil {
		t.Fatal(err)
	}
	sub := filepath.Join(workspace, "service")
	if err := os.MkdirAll(sub, 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(sub, "AGENTS.md"), []byte("service rules"), 0o644); err != nil {
		t.Fatal(err)
	}

	paths := config.Paths{Sessions: t.TempDir(), ConfigFile: filepath.Join(t.TempDir(), "config.json")}
	runtime, err := New(config.Default(), paths, sub, "test")
	if err != nil {
		t.Fatal(err)
	}
	defer runtime.Close()

	entries := runtime.SessionHistory()
	if len(entries) == 0 || entries[0].Message == nil || entries[0].Message.Role != session.RoleSystem {
		t.Fatalf("session has no system prompt: %#v", entries)
	}
	prompt := entries[0].Message.Text()
	executable, err := os.Executable()
	if err != nil {
		t.Fatal(err)
	}
	executable, err = filepath.Abs(executable)
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(prompt, "Current kon executable: "+executable) {
		t.Fatalf("current executable missing from persisted prompt:\n%s", prompt)
	}
	if !strings.Contains(prompt, "workspace rules") || !strings.Contains(prompt, "service rules") {
		t.Fatalf("context files missing from persisted prompt:\n%s", prompt)
	}
	if strings.Index(prompt, "workspace rules") > strings.Index(prompt, "service rules") {
		t.Fatalf("context files not ordered outermost first:\n%s", prompt)
	}
}

func TestNewSkipsContextFilesWhenDisabled(t *testing.T) {
	workspace := t.TempDir()
	if err := os.WriteFile(filepath.Join(workspace, "AGENTS.md"), []byte("workspace rules"), 0o644); err != nil {
		t.Fatal(err)
	}
	disabled := false
	cfg := config.Default()
	cfg.ContextFiles = &disabled

	paths := config.Paths{Sessions: t.TempDir(), ConfigFile: filepath.Join(t.TempDir(), "config.json")}
	runtime, err := New(cfg, paths, workspace, "test")
	if err != nil {
		t.Fatal(err)
	}
	defer runtime.Close()

	entries := runtime.SessionHistory()
	if len(entries) == 0 || entries[0].Message == nil {
		t.Fatalf("session has no system prompt: %#v", entries)
	}
	if strings.Contains(entries[0].Message.Text(), "workspace rules") {
		t.Fatalf("disabled context files were still loaded:\n%s", entries[0].Message.Text())
	}
}

func TestCloseCancelsAndWaitsForActiveRun(t *testing.T) {
	store := testStore(t)
	profile := config.Model{Name: "default", Type: "openai", ModelID: "model"}
	provider := &blockingProvider{started: make(chan struct{})}
	runtime := &Runtime{
		active: profile, store: store, phase: PhaseReady,
		runner: agent.New(profile, config.Default().Compaction, provider, store, tools.New(t.TempDir(), false)),
	}
	runDone := make(chan error, 1)
	go func() { runDone <- runtime.Run(context.Background(), "work", func(agent.Event) {}) }()
	<-provider.started
	if err := runtime.Close(); err != nil {
		t.Fatal(err)
	}
	if err := <-runDone; !errors.Is(err, context.Canceled) {
		t.Fatalf("run error = %v", err)
	}
	if runtime.State().Phase != PhaseClosed {
		t.Fatalf("phase = %s", runtime.State().Phase)
	}
}

func TestSwitchModelPersistsDefaultModel(t *testing.T) {
	cfg := config.Default()
	cfg.Models = append(cfg.Models, config.Model{Name: "review", Type: "anthropic", ModelID: "claude", ContextWindowTokens: 100_000})
	dir := t.TempDir()
	paths := config.Paths{ConfigFile: filepath.Join(dir, "config.json"), Sessions: filepath.Join(dir, "sessions")}
	store := testStore(t)
	profile := config.Model{Name: "default", Type: "openai", ModelID: "model"}
	runtime := &Runtime{
		config: cfg, paths: paths,
		active: profile, store: store, phase: PhaseReady,
		createRunner: func(config.Model, *session.Store) (*agent.Runner, error) { return new(agent.Runner), nil },
	}
	if err := runtime.SwitchModel("review"); err != nil {
		t.Fatal(err)
	}
	b, err := os.ReadFile(paths.ConfigFile)
	if err != nil {
		t.Fatal(err)
	}
	var saved config.Config
	if err := json.Unmarshal(b, &saved); err != nil {
		t.Fatal(err)
	}
	if saved.DefaultModel != "review" {
		t.Fatalf("saved default_model = %q, want review", saved.DefaultModel)
	}
}

func TestResumeSwitchesToPersistedSession(t *testing.T) {
	dir := t.TempDir()
	paths := config.Paths{Sessions: filepath.Join(dir, "sessions"), ConfigFile: filepath.Join(dir, "config.json")}
	cwd := t.TempDir()
	runtime, err := New(config.Default(), paths, cwd, "test")
	if err != nil {
		t.Fatal(err)
	}
	defer runtime.Close()
	// A fresh session is empty and therefore not yet a resume target. Add a
	// message directly to make it persist, as a real turn would.
	if _, err := runtime.store.AppendMessage(session.Message{Role: session.RoleUser, Parts: []session.Part{{Type: session.PartText, Text: "hello"}}}); err != nil {
		t.Fatal(err)
	}
	original := runtime.SessionID()
	if original.IsZero() {
		t.Fatal("session with content has no session ID")
	}

	other, err := session.New(paths.Sessions, cwd, "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	target := other.ID()
	if _, err := other.AppendMessage(session.Message{Role: session.RoleUser, Parts: []session.Part{{Type: session.PartText, Text: "resume me"}}}); err != nil {
		t.Fatal(err)
	}
	other.Close()

	if err := runtime.Resume(target); err != nil {
		t.Fatal(err)
	}
	if got := runtime.SessionID(); got != target {
		t.Fatalf("session ID = %s, want %s", got, target)
	}
	history := runtime.SessionHistory()
	if len(history) < 2 || history[len(history)-1].Message.Text() != "resume me" {
		t.Fatalf("resumed history = %#v", history)
	}
}

// TestSessionPreviewReadsTailWithoutSwitching guards the read-only preview path:
// it returns a persisted session's trailing turns without changing the live
// session.
func TestSessionPreviewReadsTailWithoutSwitching(t *testing.T) {
	dir := t.TempDir()
	paths := config.Paths{Sessions: filepath.Join(dir, "sessions"), ConfigFile: filepath.Join(dir, "config.json")}
	cwd := t.TempDir()
	cfg := config.Default()
	cfg.Models[0].ModelID = "gpt-4o"
	runtime, err := New(cfg, paths, cwd, "test")
	if err != nil {
		t.Fatal(err)
	}
	defer runtime.Close()
	if _, err := runtime.store.AppendMessage(session.Message{Role: session.RoleUser, Parts: []session.Part{{Type: session.PartText, Text: "live"}}}); err != nil {
		t.Fatal(err)
	}
	live := runtime.SessionID()

	other, err := session.New(paths.Sessions, cwd, "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	if _, err := other.AppendMessage(session.Message{Role: session.RoleUser, Parts: []session.Part{{Type: session.PartText, Text: "preview me"}}}); err != nil {
		t.Fatal(err)
	}
	targetPath := other.Path()
	other.Close()

	entries, err := runtime.SessionPreview(targetPath, 2)
	if err != nil {
		t.Fatal(err)
	}
	if len(entries) == 0 || entries[len(entries)-1].Message.Text() != "preview me" {
		t.Fatalf("preview entries = %#v", entries)
	}
	if got := runtime.SessionID(); got != live {
		t.Fatalf("preview switched the live session from %s to %s", live, got)
	}
}

func TestResumeReportsPersistedContextUsage(t *testing.T) {
	dir := t.TempDir()
	paths := config.Paths{Sessions: filepath.Join(dir, "sessions"), ConfigFile: filepath.Join(dir, "config.json")}
	cwd := t.TempDir()
	cfg := config.Default()
	cfg.Models[0].ModelID = "gpt-4o"
	runtime, err := New(cfg, paths, cwd, "test")
	if err != nil {
		t.Fatal(err)
	}
	defer runtime.Close()

	other, err := session.New(paths.Sessions, cwd, "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	target := other.ID()
	if _, err := other.AppendMessage(session.Message{Role: session.RoleUser, Parts: []session.Part{{Type: session.PartText, Text: "resume me"}}}); err != nil {
		t.Fatal(err)
	}
	if _, err := other.AppendMessage(session.Message{
		Role: session.RoleAssistant, Parts: []session.Part{{Type: session.PartText, Text: "done"}},
		Usage: &session.Usage{PromptTokens: 1200, CompletionTokens: 30, TotalTokens: 1230},
	}); err != nil {
		t.Fatal(err)
	}
	other.Close()

	if err := runtime.Resume(target); err != nil {
		t.Fatal(err)
	}
	tokens, ok := runtime.ContextUsage()
	if !ok || tokens != 1230 {
		t.Fatalf("ContextUsage = (%d, %v), want (1230, true)", tokens, ok)
	}
}

func TestResumeUnknownSessionKeepsCurrent(t *testing.T) {
	dir := t.TempDir()
	paths := config.Paths{Sessions: filepath.Join(dir, "sessions"), ConfigFile: filepath.Join(dir, "config.json")}
	runtime, err := New(config.Default(), paths, t.TempDir(), "test")
	if err != nil {
		t.Fatal(err)
	}
	defer runtime.Close()
	if _, err := runtime.store.AppendMessage(session.Message{Role: session.RoleUser, Parts: []session.Part{{Type: session.PartText, Text: "current"}}}); err != nil {
		t.Fatal(err)
	}
	original := runtime.SessionID()
	if original.IsZero() {
		t.Fatal("session with content has no session ID")
	}

	missing, err := typedid.ParseSessionID("ses_00000000000000000000")
	if err != nil {
		t.Fatal(err)
	}
	if err := runtime.Resume(missing); err == nil {
		t.Fatal("resuming a missing session succeeded")
	}
	if got := runtime.SessionID(); got != original {
		t.Fatalf("session changed after a failed resume: %s", got)
	}
}

func TestNewResumedIDOpensSpecificSession(t *testing.T) {
	dir := t.TempDir()
	paths := config.Paths{Sessions: filepath.Join(dir, "sessions"), ConfigFile: filepath.Join(dir, "config.json")}
	cwd := t.TempDir()
	store, err := session.New(paths.Sessions, cwd, "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	id := store.ID()
	if _, err := store.AppendMessage(session.Message{Role: session.RoleUser, Parts: []session.Part{{Type: session.PartText, Text: "already here"}}}); err != nil {
		t.Fatal(err)
	}
	store.Close()

	runtime, err := NewResumedID(config.Default(), paths, cwd, "test", id)
	if err != nil {
		t.Fatal(err)
	}
	defer runtime.Close()
	if runtime.SessionID() != id {
		t.Fatalf("opened session %s, want %s", runtime.SessionID(), id)
	}
	if history := runtime.SessionHistory(); len(history) != 2 {
		t.Fatalf("resumed history length = %d, want 2", len(history))
	}
}

func TestNewResumedWithoutSessionsStartsFresh(t *testing.T) {
	dir := t.TempDir()
	paths := config.Paths{Sessions: filepath.Join(dir, "sessions"), ConfigFile: filepath.Join(dir, "config.json")}
	runtime, err := NewResumed(config.Default(), paths, t.TempDir(), "test")
	if err != nil {
		t.Fatal(err)
	}
	defer runtime.Close()
	// The fallback session is empty, so it is not yet persisted or resumable.
	if !runtime.SessionID().IsZero() {
		t.Fatal("empty fallback session was reported as resumable")
	}
}

func TestCompactRefusesWhileRunning(t *testing.T) {
	store := testStore(t)
	runtime := &Runtime{
		active: config.Model{Name: "default", Type: "openai", ModelID: "model"},
		store:  store, phase: PhaseRunning,
	}
	if err := runtime.Compact(context.Background(), func(agent.Event) {}); !errors.Is(err, ErrBusy) {
		t.Fatalf("Compact error = %v, want ErrBusy", err)
	}
}

func TestCompactAfterCloseReportsClosed(t *testing.T) {
	runtime := &Runtime{phase: PhaseClosed}
	if err := runtime.Compact(context.Background(), func(agent.Event) {}); !errors.Is(err, ErrClosed) {
		t.Fatalf("Compact error = %v, want ErrClosed", err)
	}
}

func testStore(t *testing.T) *session.Store {
	t.Helper()
	store, err := session.New(t.TempDir(), t.TempDir(), "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = store.Close() })
	return store
}
