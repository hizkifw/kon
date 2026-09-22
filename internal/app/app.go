// Package app owns the live session, selected model, and agent runner.
package app

import (
	"context"
	"errors"
	"fmt"
	"sync"

	"github.com/hizkifw/kon/internal/agent"
	"github.com/hizkifw/kon/internal/config"
	"github.com/hizkifw/kon/internal/provider"
	"github.com/hizkifw/kon/internal/session"
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
	Provider      string
	ExternalID    string
	ContextWindow int
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

	config config.Config
	paths  config.Paths
	cwd    string

	active     config.Model
	store      *session.Store
	runner     *agent.Runner
	problem    error
	cleanupErr error
	phase      Phase
	runCancel  context.CancelFunc
	runDone    chan struct{}

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
	r := &Runtime{config: cfg, paths: paths, cwd: cwd}
	r.createSession = func() (*session.Store, error) {
		return session.New(paths.Sessions, cwd, version, agent.SystemPrompt(cwd, cfg.Instructions))
	}
	r.openSession = session.Open
	r.createRunner = func(profile config.Model, store *session.Store) (*agent.Runner, error) {
		client, err := provider.New(profile)
		if err != nil {
			return nil, err
		}
		selection := session.ModelSelection{Name: profile.Name, Provider: profile.Provider, ExternalID: client.ModelID()}
		if _, err := store.AppendModelChange(selection); err != nil {
			return nil, err
		}
		return agent.New(profile, cfg.Compaction, client, store, tools.New(cwd)), nil
	}

	profile, _ := cfg.Model(cfg.DefaultModel)
	r.active = profile

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

func (r *Runtime) Models() []Model {
	r.mu.Lock()
	defer r.mu.Unlock()
	models := make([]Model, 0, len(r.config.Models))
	for _, profile := range r.config.Models {
		models = append(models, describe(profile))
	}
	return models
}

func (r *Runtime) State() State {
	r.mu.Lock()
	defer r.mu.Unlock()
	return State{Active: describe(r.active), Phase: r.phase, Problem: r.problem}
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

// KillShell force-kills the shell command the agent is currently running, if
// any, and reports whether a command was killed. The shell tool interrupts a
// cancelled command first; this is the escalation for commands that ignore
// the interrupt.
func (r *Runtime) KillShell() bool {
	r.mu.Lock()
	runner, running := r.runner, r.phase == PhaseRunning
	r.mu.Unlock()
	return running && runner != nil && runner.KillShell()
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
	profile, ok := r.config.Model(name)
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
	return nil
}

// Sessions returns the persisted sessions for this workspace, newest first.
// The UI uses it to list candidates for "/resume".
func (r *Runtime) Sessions() ([]session.Summary, error) {
	return session.Discover(r.paths.Sessions, r.cwd)
}

// SessionID is the identifier of the live session, or the zero ID when none is
// open. It is printed on exit so the user can resume later.
func (r *Runtime) SessionID() typedid.SessionID {
	r.mu.Lock()
	defer r.mu.Unlock()
	if r.store == nil {
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
	client, err := provider.New(r.active)
	if err != nil {
		_ = store.Close()
		return nil, nil, nil, err
	}
	runner := agent.New(r.active, r.config.Compaction, client, store, tools.New(r.cwd))
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
	return Model{Name: profile.Name, Provider: profile.Provider, ExternalID: profile.ModelID, ContextWindow: profile.ContextWindowTokens}
}
