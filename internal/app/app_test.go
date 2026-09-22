package app

import (
	"context"
	"encoding/json"
	"errors"
	"os"
	"path/filepath"
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

func (*blockingProvider) Complete(context.Context, []session.Message, int) (session.Message, error) {
	return session.Message{}, errors.New("unexpected completion")
}

func TestNewSessionFailureKeepsCurrentSession(t *testing.T) {
	store := testStore(t)
	runner := new(agent.Runner)
	runtime := &Runtime{
		active: config.Model{Name: "default", Provider: "openai", ModelID: "model"},
		store:  store, runner: runner,
		createSession: func() (*session.Store, error) { return nil, errors.New("disk full") },
	}
	if err := runtime.NewSession(); err == nil {
		t.Fatal("expected replacement failure")
	}
	if runtime.store != store || runtime.runner != runner {
		t.Fatal("live session changed after replacement failure")
	}
	if _, err := store.AppendMessage(session.Message{Role: session.RoleUser, Content: "still usable"}); err != nil {
		t.Fatalf("old session is unusable: %v", err)
	}
}

func TestNewSessionSwapsThenClosesPreviousStore(t *testing.T) {
	previous := testStore(t)
	replacement := testStore(t)
	runtime := &Runtime{
		active: config.Model{Name: "default", Provider: "openai", ModelID: "model"},
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
	if _, err := previous.AppendMessage(session.Message{Role: session.RoleUser, Content: "closed"}); err == nil {
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

func TestCloseCancelsAndWaitsForActiveRun(t *testing.T) {
	store := testStore(t)
	profile := config.Model{Name: "default", Provider: "openai", ModelID: "model"}
	provider := &blockingProvider{started: make(chan struct{})}
	runtime := &Runtime{
		active: profile, store: store, phase: PhaseReady,
		runner: agent.New(profile, config.Default().Compaction, provider, store, tools.New(t.TempDir())),
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
	cfg.Models = append(cfg.Models, config.Model{Name: "review", Provider: "anthropic", ModelID: "claude", ContextWindowTokens: 100_000})
	dir := t.TempDir()
	paths := config.Paths{ConfigFile: filepath.Join(dir, "config.json"), Sessions: filepath.Join(dir, "sessions")}
	store := testStore(t)
	profile := config.Model{Name: "default", Provider: "openai", ModelID: "model"}
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
	original := runtime.SessionID()
	if original.IsZero() {
		t.Fatal("new runtime has no session ID")
	}

	other, err := session.New(paths.Sessions, cwd, "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	target := other.ID()
	if _, err := other.AppendMessage(session.Message{Role: session.RoleUser, Content: "resume me"}); err != nil {
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
	if len(history) < 2 || history[len(history)-1].Message.Content != "resume me" {
		t.Fatalf("resumed history = %#v", history)
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
	original := runtime.SessionID()

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
	if _, err := store.AppendMessage(session.Message{Role: session.RoleUser, Content: "already here"}); err != nil {
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
	if runtime.SessionID().IsZero() {
		t.Fatal("--resume with no sessions did not start a session")
	}
}

func TestCompactRefusesWhileRunning(t *testing.T) {
	store := testStore(t)
	runtime := &Runtime{
		active: config.Model{Name: "default", Provider: "openai", ModelID: "model"},
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
