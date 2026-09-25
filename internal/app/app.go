// Package app owns the live session, selected model, and agent runner.
package app

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"slices"
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
	ErrNoEffort = errors.New("model has no reasoning effort levels")
)

type Model struct {
	Name          string
	ConnectionID  string
	DisplayName   string
	WireFormat    string
	ExternalID    string
	ContextWindow tokens.Count
	// ReasoningEfforts lists the levels Shift+Tab cycles through; empty means
	// the model has no reasoning control.
	ReasoningEfforts []string
	// ReasoningEffort is the selected effort; empty means the provider default.
	ReasoningEffort string
	Source          string
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

	active         modelSpec
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
	createRunner  func(modelSpec, *session.Store) (*agent.Runner, error)
}

// modelSpec is a model ready to run: its profile as configured, completed with
// catalog capabilities for a derived model, and the reasoning effort chosen at
// runtime. The effort stays out of config.Model because it is never written
// per model.
type modelSpec struct {
	config.Model
	effort string
}

// providerSpec is what the backend needs to reach and shape requests for the model.
func (m modelSpec) providerSpec() provider.Spec {
	return provider.Spec{
		Name: m.Name, Format: m.WireFormat(), ModelID: m.ModelID,
		BaseURL: m.BaseURL, APIKey: m.APIKey, Headers: m.Headers,
		Vision: m.Vision, Reasoning: m.Reasoning, ReasoningEffort: m.effort,
	}
}

// limits sizes the runner's context for the model under compaction's budgets.
func (m modelSpec) limits(compaction config.Compaction) agent.Limits {
	return agent.Limits{
		ContextWindow:    m.ContextWindowTokens,
		ReserveTokens:    compaction.ReserveTokens,
		KeepRecentTokens: compaction.KeepRecentTokens,
	}
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
	r.createRunner = func(profile modelSpec, store *session.Store) (*agent.Runner, error) {
		client, err := provider.New(profile.providerSpec(), store.ReadImage)
		if err != nil {
			return nil, err
		}
		selection := session.ModelSelection{Name: profile.Name, WireFormat: string(profile.WireFormat()), ExternalID: client.ModelID()}
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
		return agent.New(profile.limits(cfg.Compaction), client, store, tools.New(cwd, profile.Vision)), nil
	}

	configured, _ := cfg.ResolveModel(cfg.DefaultModel)
	// A derived model has no levels until its catalog entry resolves, so its
	// saved effort is applied again by resolveActive.
	profile := r.withEffort(configured)
	r.active = profile
	_, r.activeResolved = cfg.Model(cfg.DefaultModel)

	if resume {
		target, err := r.openTarget(id)
		if err != nil {
			return nil, err
		}
		r.install(target)
		return r, nil
	}
	store, runner, problem, err := r.prepareSession(profile)
	if err != nil {
		return nil, err
	}
	r.install(opened{store: store, runner: runner, profile: profile, problem: problem})
	return r, nil
}

// opened is a session ready to install: its store, the model that will answer
// in it, and that model's runner, or the problem that keeps it from running.
type opened struct {
	store   *session.Store
	runner  *agent.Runner
	profile modelSpec
	problem error
}

// install makes an opened session and its model the live ones. The caller
// closes any session it replaces.
func (r *Runtime) install(o opened) {
	r.store, r.runner, r.problem = o.store, o.runner, o.problem
	r.active = o.profile
	_, r.activeResolved = r.config.Model(o.profile.Name)
	r.phase = PhaseReady
	if o.problem != nil {
		r.phase = PhaseNeedsConfiguration
	}
}

// openTarget opens the session to resume. With a zero id it resumes the newest
// session for cwd; with no sessions at all it starts a fresh one.
func (r *Runtime) openTarget(id typedid.SessionID) (opened, error) {
	var path string
	if id.IsZero() {
		summary, ok, err := session.Latest(r.paths.Sessions, r.cwd)
		if err != nil {
			return opened{}, err
		}
		if !ok {
			// Nothing to resume: fall back to a new session so --resume still
			// launches rather than failing on an empty workspace.
			store, runner, problem, err := r.prepareSession(r.active)
			if err != nil {
				return opened{}, err
			}
			return opened{store: store, runner: runner, profile: r.active, problem: problem}, nil
		}
		path = summary.Path
	} else {
		summary, err := session.Find(r.paths.Sessions, r.cwd, id)
		if err != nil {
			return opened{}, err
		}
		path = summary.Path
	}
	return r.openStore(path)
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
	// A derived model learns its context window from the catalog, which
	// compaction needs as much as a run does.
	if err := r.resolveActive(); err != nil {
		r.mu.Unlock()
		return err
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
	// Effort levels differ between models, so a switch starts on the provider
	// default.
	runner, err := r.createRunner(modelSpec{Model: profile}, r.store)
	if err != nil {
		return err
	}
	// Persist the selection so the next kon launch starts on this model.
	r.config.DefaultModel = profile.Name
	r.config.ReasoningEffort = ""
	if err := r.config.Save(r.paths.ConfigFile); err != nil {
		return fmt.Errorf("model switched to %s, but saving config: %w", name, err)
	}
	r.active, r.runner, r.problem, r.phase = modelSpec{Model: profile}, runner, nil, PhaseReady
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
	target, err := r.openStore(summary.Path)
	if err != nil {
		return err
	}
	previous := r.store
	r.install(target)
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

func (r *Runtime) prepareSession(profile modelSpec) (*session.Store, *agent.Runner, error, error) {
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

// openStore opens a persisted session on the model it last recorded, so a
// resumed conversation continues with the model that was answering it. When
// that model no longer resolves, or the session never recorded one, it
// continues on the current model instead.
//
// A model change is appended only when the model that will answer differs
// from the session's last record, so the log always says which model wrote
// each reply. Restoring the recorded model adds nothing.
func (r *Runtime) openStore(path string) (opened, error) {
	store, err := r.openSession(path)
	if err != nil {
		return opened{}, err
	}
	last := lastModelChange(store.ActivePath())
	profile := r.active
	if last != nil {
		if recorded, ok := r.config.ResolveModel(last.Name); ok {
			profile = r.withEffort(recorded)
		}
	}
	if problem := profile.Ready(); problem != nil {
		return opened{store: store, profile: profile, problem: problem}, nil
	}
	var runner *agent.Runner
	if last != nil && last.Name == profile.Name && last.ExternalID.String() == profile.ModelID {
		client, err := provider.New(profile.providerSpec(), store.ReadImage)
		if err != nil {
			_ = store.Close()
			return opened{}, err
		}
		runner = agent.New(profile.limits(r.config.Compaction), client, store, tools.New(r.cwd, profile.Vision))
	} else if runner, err = r.createRunner(profile, store); err != nil {
		_ = store.Close()
		return opened{}, err
	}
	return opened{store: store, runner: runner, profile: profile}, nil
}

// lastModelChange is the newest model selection recorded on a path, or nil.
func lastModelChange(path []session.Entry) *session.ModelSelection {
	for i := len(path) - 1; i >= 0; i-- {
		if path[i].Type == session.EntryTypeModelChange && path[i].Model != nil {
			return path[i].Model
		}
	}
	return nil
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

func describe(profile modelSpec) Model {
	return Model{
		Name: profile.Name, WireFormat: string(profile.WireFormat()), ExternalID: profile.ModelID,
		ContextWindow:    profile.ContextWindowTokens,
		ReasoningEfforts: slices.Clone(profile.ReasoningEfforts), ReasoningEffort: profile.effort,
	}
}
