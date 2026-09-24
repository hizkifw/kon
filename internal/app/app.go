// Package app owns the live session, selected model, and agent runner.
package app

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"sync/atomic"

	"github.com/hizkifw/kon/internal/agent"
	"github.com/hizkifw/kon/internal/catalog"
	"github.com/hizkifw/kon/internal/config"
	"github.com/hizkifw/kon/internal/contextfiles"
	"github.com/hizkifw/kon/internal/provider"
	"github.com/hizkifw/kon/internal/session"
	"github.com/hizkifw/kon/internal/tokens"
	"github.com/hizkifw/kon/internal/tools"
	"github.com/hizkifw/kon/internal/typedid"
)

var (
	ErrBusy     = errors.New("agent is busy")
	ErrClosed   = errors.New("app is closed")
	ErrNotReady = errors.New("model is not configured")
)

type Model struct {
	Name          string
	ConnectionID  string
	DisplayName   string
	Type          string
	ExternalID    string
	ContextWindow tokens.Count
	Source        string
}

type Phase string

const (
	PhaseNeedsConfiguration Phase = "needs_configuration"
	PhaseReady              Phase = "ready"
	PhaseRunning            Phase = "running"
	PhaseClosed             Phase = "closed"
)

type State struct {
	Active  Model
	Phase   Phase
	Problem error
}

func (s State) Ready() bool { return s.Phase == PhaseReady }

type Runtime struct {
	mu sync.Mutex

	config         config.Config
	paths          config.Paths
	cwd            string
	catalogOnce    sync.Once
	catalog        atomic.Pointer[catalog.Service]
	providerModels map[string][]string

	active         config.Model
	activeResolved bool
	store          *session.Store
	runner         *agent.Runner
	problem        error
	cleanupErr     error
	phase          Phase
	runCancel      context.CancelFunc
	runDone        chan struct{}

	createSession func() (*session.Store, error)
	openSession   func(string) (*session.Store, error)
	createRunner  func(config.Model, *session.Store) (*agent.Runner, error)
}

func New(cfg config.Config, paths config.Paths, cwd, version string) (*Runtime, error) {
	return start(cfg, paths, cwd, version, false, typedid.SessionID{})
}

// NewResumed starts a runtime already attached to an existing session for cwd,
// if one exists. It is the entry point for the --resume flag.
func NewResumed(cfg config.Config, paths config.Paths, cwd, version string) (*Runtime, error) {
	return start(cfg, paths, cwd, version, true, typedid.SessionID{})
}

// NewResumedID starts a runtime attached to a specific session ID. The named
// session must already exist for cwd.
func NewResumedID(cfg config.Config, paths config.Paths, cwd, version string, id typedid.SessionID) (*Runtime, error) {
	return start(cfg, paths, cwd, version, true, id)
}

func start(cfg config.Config, paths config.Paths, cwd, version string, resume bool, id typedid.SessionID) (*Runtime, error) {
	r := &Runtime{config: cfg, paths: paths, cwd: cwd, providerModels: loadProviderModels(paths.ProviderModels)}
	r.createSession = func() (*session.Store, error) {
		prompt, err := r.systemPrompt()
		if err != nil {
			return nil, err
		}
		return session.New(paths.Sessions, cwd, version, prompt)
	}
	r.openSession = session.Open
	r.createRunner = func(profile config.Model, store *session.Store) (*agent.Runner, error) {
		client, err := provider.New(profile, store.ReadImage)
		if err != nil {
			return nil, err
		}
		selection := session.ModelSelection{Name: profile.Name, WireFormat: profile.WireType(), ExternalID: client.ModelID()}
		if _, explicit := r.config.Model(profile.Name); !explicit {
			if id, _, ok := strings.Cut(profile.Name, "/"); ok {
				if connection, found := r.config.Provider(id); found {
					selection.ConnectionID = connection.ID
				}
			}
		}
		if _, err := store.AppendModelChange(selection); err != nil {
			return nil, err
		}
		return agent.New(profile, cfg.Compaction, client, store, tools.New(cwd, profile.Vision)), nil
	}

	profile, _ := cfg.ResolveModel(cfg.DefaultModel)
	r.active = profile
	_, r.activeResolved = cfg.Model(cfg.DefaultModel)

	var (
		store   *session.Store
		runner  *agent.Runner
		problem error
		err     error
	)
	if resume {
		store, runner, problem, err = r.openTarget(id)
		if err != nil {
			return nil, err
		}
	} else {
		store, runner, problem, err = r.prepareSession(profile)
		if err != nil {
			return nil, err
		}
	}
	r.store, r.runner, r.problem = store, runner, problem
	r.phase = PhaseReady
	if problem != nil {
		r.phase = PhaseNeedsConfiguration
	}
	return r, nil
}

// openTarget opens the session to resume. With a zero id it resumes the newest
// session for cwd; with no sessions at all it starts a fresh one.
func (r *Runtime) openTarget(id typedid.SessionID) (*session.Store, *agent.Runner, error, error) {
	var path string
	if id.IsZero() {
		summary, ok, err := session.Latest(r.paths.Sessions, r.cwd)
		if err != nil {
			return nil, nil, nil, err
		}
		if !ok {
			// Nothing to resume: fall back to a new session so --resume still
			// launches rather than failing on an empty workspace.
			return r.prepareSession(r.active)
		}
		path = summary.Path
	} else {
		summary, err := session.Find(r.paths.Sessions, r.cwd, id)
		if err != nil {
			return nil, nil, nil, err
		}
		path = summary.Path
	}
	store, runner, problem, err := r.openStore(path)
	if err != nil {
		return nil, nil, nil, err
	}
	return store, runner, problem, nil
}

func (r *Runtime) State() State {
	r.mu.Lock()
	defer r.mu.Unlock()
	return State{Active: r.describeActive(), Phase: r.phase, Problem: r.problem}
}

func (r *Runtime) Run(ctx context.Context, prompt string, emit func(agent.Event)) error {
	r.mu.Lock()
	if r.phase == PhaseClosed {
		r.mu.Unlock()
		return ErrClosed
	}
	if r.phase == PhaseRunning {
		r.mu.Unlock()
		return ErrBusy
	}
	if r.phase == PhaseNeedsConfiguration {
		problem := r.problem
		r.mu.Unlock()
		return fmt.Errorf("%w: %v in %s", ErrNotReady, problem, r.paths.ConfigFile)
	}
	// Startup leaves catalog metadata to LoadCatalog; make sure the model's
	// capabilities are resolved before its first provider request.
	if err := r.resolveActive(); err != nil {
		r.mu.Unlock()
		return err
	}
	runner := r.runner
	runCtx, cancel := context.WithCancel(ctx)
	r.phase = PhaseRunning
	r.runCancel = cancel
	r.runDone = make(chan struct{})
	done := r.runDone
	r.mu.Unlock()

	defer func() {
		cancel()
		r.mu.Lock()
		r.phase = PhaseReady
		r.runCancel = nil
		r.runDone = nil
		close(done)
		r.mu.Unlock()
	}()
	return runner.Run(runCtx, prompt, emit)
}

// Compact forces a manual compaction of the live session. Like Run it occupies
// the busy phase so it cannot race an active request, and it can be cancelled
// with Esc through the same context.
func (r *Runtime) Compact(ctx context.Context, emit func(agent.Event)) error {
	r.mu.Lock()
	if r.phase == PhaseClosed {
		r.mu.Unlock()
		return ErrClosed
	}
	if r.phase == PhaseRunning {
		r.mu.Unlock()
		return ErrBusy
	}
	if r.phase == PhaseNeedsConfiguration {
		problem := r.problem
		r.mu.Unlock()
		return fmt.Errorf("%w: %v in %s", ErrNotReady, problem, r.paths.ConfigFile)
	}
	runner := r.runner
	if runner == nil {
		r.mu.Unlock()
		return ErrNotReady
	}
	runCtx, cancel := context.WithCancel(ctx)
	r.phase = PhaseRunning
	r.runCancel = cancel
	r.runDone = make(chan struct{})
	done := r.runDone
	r.mu.Unlock()

	defer func() {
		cancel()
		r.mu.Lock()
		r.phase = PhaseReady
		r.runCancel = nil
		r.runDone = nil
		close(done)
		r.mu.Unlock()
	}()
	return runner.Compact(runCtx, emit)
}

// Interrupt escalates cancellation of the tool call the agent is currently
// running. attempt is the number of consecutive interrupt presses; the runner
// forwards it to every registered tool. A shell command is interrupted on the
// first press and force-killed on the second if it ignored the interrupt.
func (r *Runtime) Interrupt(attempt int) bool {
	r.mu.Lock()
	runner, running := r.runner, r.phase == PhaseRunning
	r.mu.Unlock()
	return running && runner != nil && runner.Interrupt(attempt)
}

// SwitchModel changes the runner in the current session only after the new
// model has been constructed and its durable model-change entry has synced.
// It also persists the selection by updating default_model in the config file.
func (r *Runtime) SwitchModel(name string) error {
	r.mu.Lock()
	defer r.mu.Unlock()
	if err := r.mutable(); err != nil {
		return err
	}
	if name == r.active.Name {
		return nil
	}
	profile, ok := r.resolveModel(name)
	if !ok {
		return fmt.Errorf("unknown model %q", name)
	}
	if err := profile.Ready(); err != nil {
		return err
	}
	runner, err := r.createRunner(profile, r.store)
	if err != nil {
		return err
	}
	// Persist the selection so the next kon launch starts on this model.
	r.config.DefaultModel = profile.Name
	if err := r.config.Save(r.paths.ConfigFile); err != nil {
		return fmt.Errorf("model switched to %s, but saving config: %w", name, err)
	}
	r.active, r.runner, r.problem, r.phase = profile, runner, nil, PhaseReady
	r.activeResolved = true
	return nil
}

// Sessions returns the persisted sessions for this workspace, newest first.
// The UI uses it to list candidates for "/resume".
func (r *Runtime) Sessions() ([]session.Summary, error) {
	return session.Discover(r.paths.Sessions, r.cwd)
}

// SessionID is the identifier of the live session, or the zero ID when none is
// open. A session that is still empty is excluded: it is discarded on close
// rather than kept as a resume target, so it is never reported.
func (r *Runtime) SessionID() typedid.SessionID {
	r.mu.Lock()
	defer r.mu.Unlock()
	if r.store == nil || r.store.Empty() {
		return typedid.SessionID{}
	}
	return r.store.ID()
}

// SessionHistory returns the live session's active conversation path, in
// conversation order, for read-only replay in the transcript.
func (r *Runtime) SessionHistory() []session.Entry {
	r.mu.Lock()
	defer r.mu.Unlock()
	if r.store == nil {
		return nil
	}
	return r.store.ActivePath()
}

// SessionPreview returns the last maxTurns user turns of a persisted session
// for a read-only preview. It reads the tail of the file directly, so it does
// not scan every session in the workspace or parse the whole transcript, and it
// never opens or modifies the file.
func (r *Runtime) SessionPreview(path string, maxTurns int) ([]session.Entry, error) {
	return session.TailEntries(path, maxTurns)
}

// DescribeTool resolves the transcript display for a persisted tool call
// through the tool that owns it, so a replayed session shows the same
// presentation the call had live.
func (r *Runtime) DescribeTool(name string, args json.RawMessage, result string, failed bool, details json.RawMessage) tools.Display {
	return tools.Describe(name, args, result, failed, details, r.cwd)
}

// ContextUsage reports the current context size in tokens and whether that value
// is known. A resumed session carries the last provider-reported count so the
// status line shows it instead of an unknown placeholder.
func (r *Runtime) ContextUsage() (tokens.Count, bool) {
	r.mu.Lock()
	defer r.mu.Unlock()
	if r.runner == nil {
		return 0, false
	}
	return r.runner.ContextUsage()
}

// Resume replaces the live session with the persisted session named by id. The
// replacement is fully opened and validated before the current session is
// closed, so a failure leaves the current session usable.
func (r *Runtime) Resume(id typedid.SessionID) error {
	r.mu.Lock()
	defer r.mu.Unlock()
	if err := r.mutable(); err != nil {
		return err
	}
	summary, err := session.Find(r.paths.Sessions, r.cwd, id)
	if err != nil {
		return err
	}
	store, runner, problem, err := r.openStore(summary.Path)
	if err != nil {
		return err
	}
	previous := r.store
	r.store, r.runner, r.problem = store, runner, problem
	r.phase = PhaseReady
	if problem != nil {
		r.phase = PhaseNeedsConfiguration
	}
	if previous != nil {
		r.cleanupErr = errors.Join(r.cleanupErr, previous.Close())
	}
	return nil
}

// NewSession prepares the complete replacement before closing the current
// store. A failure therefore leaves the existing session usable.
func (r *Runtime) NewSession() error {
	r.mu.Lock()
	defer r.mu.Unlock()
	if err := r.mutable(); err != nil {
		return err
	}
	store, runner, problem, err := r.prepareSession(r.active)
	if err != nil {
		return err
	}
	previous := r.store
	r.store, r.runner, r.problem = store, runner, problem
	r.phase = PhaseReady
	if problem != nil {
		r.phase = PhaseNeedsConfiguration
	}
	if previous != nil {
		r.cleanupErr = errors.Join(r.cleanupErr, previous.Close())
	}
	return nil
}

func (r *Runtime) Close() error {
	r.mu.Lock()
	if r.phase == PhaseClosed {
		r.mu.Unlock()
		return nil
	}
	if r.phase == PhaseRunning {
		cancel, done := r.runCancel, r.runDone
		r.mu.Unlock()
		cancel()
		<-done
		r.mu.Lock()
		if r.phase == PhaseClosed {
			r.mu.Unlock()
			return nil
		}
	}
	r.phase = PhaseClosed
	if r.store == nil {
		r.mu.Unlock()
		return nil
	}
	err := errors.Join(r.cleanupErr, r.store.Close())
	r.store, r.runner = nil, nil
	r.mu.Unlock()
	return err
}

// systemPrompt resolves the context files that apply to the working directory,
// when enabled, and builds the durable system prompt for a new session.
func (r *Runtime) systemPrompt() (string, error) {
	executable, err := os.Executable()
	if err != nil {
		return "", fmt.Errorf("find kon executable: %w", err)
	}
	executable, err = filepath.Abs(executable)
	if err != nil {
		return "", fmt.Errorf("resolve kon executable path: %w", err)
	}
	var files []contextfiles.File
	if r.config.ContextFilesEnabled() {
		discovered, err := contextfiles.Load(r.cwd)
		if err != nil {
			return "", err
		}
		files = discovered
	}
	return agent.SystemPrompt(r.cwd, executable, files, r.config.Instructions), nil
}

func (r *Runtime) prepareSession(profile config.Model) (*session.Store, *agent.Runner, error, error) {
	store, err := r.createSession()
	if err != nil {
		return nil, nil, nil, err
	}
	if problem := profile.Ready(); problem != nil {
		return store, nil, problem, nil
	}
	runner, err := r.createRunner(profile, store)
	if err != nil {
		_ = store.Close()
		return nil, nil, nil, err
	}
	return store, runner, nil, nil
}

// openStore opens a persisted session and, when the active model is configured,
// builds a runner for it. No model-change entry is appended: the resumed
// session already records the model that applies to it.
func (r *Runtime) openStore(path string) (*session.Store, *agent.Runner, error, error) {
	store, err := r.openSession(path)
	if err != nil {
		return nil, nil, nil, err
	}
	if problem := r.active.Ready(); problem != nil {
		return store, nil, problem, nil
	}
	client, err := provider.New(r.active, store.ReadImage)
	if err != nil {
		_ = store.Close()
		return nil, nil, nil, err
	}
	runner := agent.New(r.active, r.config.Compaction, client, store, tools.New(r.cwd, r.active.Vision))
	return store, runner, nil, nil
}

func (r *Runtime) mutable() error {
	if r.phase == PhaseClosed {
		return ErrClosed
	}
	if r.phase == PhaseRunning {
		return ErrBusy
	}
	return nil
}

func describe(profile config.Model) Model {
	return Model{Name: profile.Name, Type: profile.WireType(), ExternalID: profile.ModelID, ContextWindow: profile.ContextWindowTokens}
}
