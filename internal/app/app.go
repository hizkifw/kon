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
	// ErrReadOnly refuses a change to a session followed while another kon
	// has it open; see TakeOver.
	ErrReadOnly = errors.New("session is read-only while it is open in another session")
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
	// PhaseFollowing shows a session another kon has open, read-only, until
	// TakeOver makes this runtime its writer.
	PhaseFollowing Phase = "following"
	PhaseClosed    Phase = "closed"
)

type State struct {
	Active  Model
	Phase   Phase
	Problem error
}

func (s State) Ready() bool     { return s.Phase == PhaseReady }
func (s State) Following() bool { return s.Phase == PhaseFollowing }

type Runtime struct {
	mu sync.Mutex

	config         config.Config
	paths          config.Paths
	cwd            string
	catalogOnce    sync.Once
	catalog        atomic.Pointer[catalog.Service]
	providerModels map[string][]string

	active modelSpec
	store  *session.Store
	// view replaces store while following a session another kon has open.
	view       *session.View
	runner     *agent.Runner
	problem    error
	cleanupErr error
	phase      Phase
	runCancel  context.CancelFunc
	runDone    chan struct{}

	// pinned keeps the active model across a resume instead of restoring the
	// session's recorded one, and effort, when set, replaces the saved
	// effort. Both come from Options and are never written to the config.
	pinned bool
	effort string

	createSession func() (*session.Store, error)
	openSession   func(string) (*session.Store, error)
	// createRunner puts a model to work in a store: it builds the runner and
	// records the model change, so the log names the model behind each reply.
	// Rebuilding a runner for the model already recorded uses buildRunner.
	createRunner func(modelSpec, *session.Store) (*agent.Runner, error)
}

// modelSpec is a model ready to run: its profile as configured, completed with
// catalog capabilities for a derived model, and the reasoning effort chosen at
// runtime. The effort stays out of config.Model because it is never written
// per model.
type modelSpec struct {
	config.Model
	effort string
	// resolved reports whether catalog capabilities have been applied. An
	// explicit profile is complete as written; a derived one waits for the
	// catalog, which startup never loads.
	resolved bool
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
	return Start(cfg, paths, cwd, version, Options{})
}

// NewResumed starts a runtime already attached to an existing session for cwd,
// if one exists. It is the entry point for the --resume flag.
func NewResumed(cfg config.Config, paths config.Paths, cwd, version string) (*Runtime, error) {
	return Start(cfg, paths, cwd, version, Options{Resume: true})
}

// NewResumedID starts a runtime attached to a specific session ID. The named
// session must already exist for cwd.
func NewResumedID(cfg config.Config, paths config.Paths, cwd, version string, id typedid.SessionID) (*Runtime, error) {
	return Start(cfg, paths, cwd, version, Options{Resume: true, SessionID: id})
}

// Options choose how a runtime starts.
type Options struct {
	// Resume opens the session named by SessionID, or the newest one for cwd
	// when it is zero. With no sessions at all a new one starts.
	Resume    bool
	SessionID typedid.SessionID
	// Model and Effort replace the configured default model and saved
	// reasoning effort for this runtime only, and are never written to the
	// config. A resumed session switches to Model instead of restoring the one
	// it recorded.
	Model  string
	Effort string
}

// Start builds a runtime on a new or resumed session.
func Start(cfg config.Config, paths config.Paths, cwd, version string, opts Options) (*Runtime, error) {
	name := cfg.DefaultModel
	if opts.Model != "" {
		if _, ok := cfg.ResolveModel(opts.Model); !ok {
			return nil, fmt.Errorf("unknown model %q", opts.Model)
		}
		name = opts.Model
	}
	r := &Runtime{config: cfg, paths: paths, cwd: cwd, providerModels: loadProviderModels(paths.ProviderModels), pinned: opts.Model != "", effort: opts.Effort}
	r.createSession = func() (*session.Store, error) {
		prompt, err := r.systemPrompt()
		if err != nil {
			return nil, err
		}
		return session.New(paths.Sessions, cwd, version, prompt)
	}
	r.openSession = session.Open
	r.createRunner = func(profile modelSpec, store *session.Store) (*agent.Runner, error) {
		runner, err := r.buildRunner(profile, store)
		if err != nil {
			return nil, err
		}
		if err := r.recordModel(profile, store); err != nil {
			return nil, err
		}
		return runner, nil
	}

	// A derived model has no levels until its catalog entry resolves, so its
	// saved effort is applied again by resolveActive.
	r.active, _ = r.configuredSpec(name)

	var target opened
	var err error
	if opts.Resume {
		target, err = r.openTarget(opts.SessionID)
	} else {
		target, err = r.prepareSession(r.active)
	}
	if err != nil {
		return nil, err
	}
	r.install(target)
	if opts.Effort != "" {
		if err := r.checkEffort(opts.Effort); err != nil {
			_ = r.Close()
			return nil, err
		}
	}
	return r, nil
}

// checkEffort refuses a requested effort the active model does not list,
// rather than quietly running at the provider default. A derived model learns
// its levels from the catalog, so asking for an effort waits for it to load.
func (r *Runtime) checkEffort(effort string) error {
	if r.phase != PhaseReady {
		return nil
	}
	if err := r.resolveActive(); err != nil {
		return err
	}
	if r.active.effort == effort {
		return nil
	}
	levels := "none"
	if len(r.active.ReasoningEfforts) > 0 {
		levels = strings.Join(r.active.ReasoningEfforts, ", ")
	}
	return fmt.Errorf("model %q has no reasoning effort %q (levels: %s)", r.active.Name, effort, levels)
}

// buildRunner builds the runner for a model in a store without recording
// anything, for a model the store already names as its latest.
func (r *Runtime) buildRunner(profile modelSpec, store *session.Store) (*agent.Runner, error) {
	client, err := provider.New(profile.providerSpec(), store.ReadImage)
	if err != nil {
		return nil, err
	}
	return agent.New(profile.limits(r.config.Compaction), client, store, tools.New(r.cwd, profile.Vision)), nil
}

// recordModel appends the model change that names profile as the model now
// answering in store.
func (r *Runtime) recordModel(profile modelSpec, store *session.Store) error {
	selection := session.ModelSelection{Name: profile.Name, WireFormat: string(profile.WireFormat()), ExternalID: typedid.ExternalModelID(profile.ModelID)}
	if _, explicit := r.config.Model(profile.Name); !explicit {
		if id, _, ok := strings.Cut(profile.Name, "/"); ok {
			if connection, found := r.config.Provider(id); found {
				selection.ConnectionID = connection.ID
			}
		}
	}
	_, err := store.AppendModelChange(selection)
	return err
}

// opened is a session ready to install: its store, the model that will answer
// in it, and that model's runner, or the problem that keeps it from running.
// A session another kon has open is opened as a view instead, with no store or
// runner.
type opened struct {
	store   *session.Store
	view    *session.View
	runner  *agent.Runner
	profile modelSpec
	problem error
}

// install makes an opened session and its model the live ones. The caller
// closes any session it replaces; see swap.
func (r *Runtime) install(o opened) {
	r.store, r.view, r.runner, r.problem = o.store, o.view, o.runner, o.problem
	r.active = o.profile
	switch {
	case o.view != nil:
		r.phase = PhaseFollowing
	case o.problem != nil:
		r.phase = PhaseNeedsConfiguration
	default:
		r.phase = PhaseReady
	}
}

// swap installs a fully opened replacement and only then closes the session it
// replaces, so a failure to open leaves the current session usable.
func (r *Runtime) swap(o opened) {
	previous := r.store
	r.install(o)
	if previous != nil {
		r.cleanupErr = errors.Join(r.cleanupErr, previous.Close())
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
			return r.prepareSession(r.active)
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

// Run sends prompt and drives the agent to a final response. inbox carries
// steering the user sends while it runs; it may be nil.
func (r *Runtime) Run(ctx context.Context, prompt string, inbox *agent.Inbox, emit func(agent.Event)) error {
	return r.operate(ctx, func(ctx context.Context, runner *agent.Runner) error {
		return runner.Run(ctx, prompt, inbox, emit)
	})
}

// Compact forces a manual compaction of the live session. Like Run it occupies
// the busy phase so it cannot race an active request, and it can be cancelled
// with Esc through the same context.
func (r *Runtime) Compact(ctx context.Context, emit func(agent.Event)) error {
	return r.operate(ctx, func(ctx context.Context, runner *agent.Runner) error {
		return runner.Compact(ctx, emit)
	})
}

// operate runs op on the live runner in the busy phase, outside the lock, so
// every other operation is refused until it returns. Close cancels op's context
// and waits for it.
func (r *Runtime) operate(ctx context.Context, op func(context.Context, *agent.Runner) error) error {
	r.mu.Lock()
	switch r.phase {
	case PhaseClosed:
		r.mu.Unlock()
		return ErrClosed
	case PhaseRunning:
		r.mu.Unlock()
		return ErrBusy
	case PhaseNeedsConfiguration:
		problem := r.problem
		r.mu.Unlock()
		return fmt.Errorf("%w: %v in %s", ErrNotReady, problem, r.paths.ConfigFile)
	case PhaseFollowing:
		r.mu.Unlock()
		return ErrReadOnly
	}
	// Startup leaves catalog metadata to LoadCatalog. A request needs the
	// model's capabilities and compaction its context window, so resolve them
	// before either.
	if err := r.resolveActive(); err != nil {
		r.mu.Unlock()
		return err
	}
	runner := r.runner
	if runner == nil {
		r.mu.Unlock()
		return ErrNotReady
	}
	opCtx, cancel := context.WithCancel(ctx)
	done := make(chan struct{})
	r.phase, r.runCancel, r.runDone = PhaseRunning, cancel, done
	r.mu.Unlock()

	defer func() {
		cancel()
		r.mu.Lock()
		// Close claims the phase before waiting; only a live runtime returns
		// to ready.
		if r.phase == PhaseRunning {
			r.phase = PhaseReady
		}
		r.runCancel, r.runDone = nil, nil
		close(done)
		r.mu.Unlock()
	}()
	return op(opCtx, runner)
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
	if r.phase == PhaseFollowing {
		return ErrReadOnly
	}
	if name == r.active.Name {
		return nil
	}
	profile, ok := r.resolvedSpec(name)
	if !ok {
		return fmt.Errorf("unknown model %q", name)
	}
	if err := profile.Ready(); err != nil {
		return err
	}
	// Effort levels differ between models, so a switch starts on the provider
	// default.
	profile.effort = ""
	runner, err := r.createRunner(profile, r.store)
	if err != nil {
		return err
	}
	// Persist the selection so the next kon launch starts on this model.
	r.config.DefaultModel = profile.Name
	r.config.ReasoningEffort = ""
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
// open. A session that is still empty is excluded: it is discarded on close
// rather than kept as a resume target, so it is never reported.
func (r *Runtime) SessionID() typedid.SessionID {
	r.mu.Lock()
	defer r.mu.Unlock()
	if r.view != nil {
		return r.view.ID()
	}
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
	if r.view != nil {
		return r.view.ActivePath()
	}
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
	r.swap(target)
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
	target, err := r.prepareSession(r.active)
	if err != nil {
		return err
	}
	r.swap(target)
	return nil
}

// Close cancels any running operation, waits for it, and closes the session.
// It claims the closed phase before waiting, so nothing can start while the
// operation winds down and have its store closed underneath it.
func (r *Runtime) Close() error {
	r.mu.Lock()
	if r.phase == PhaseClosed {
		r.mu.Unlock()
		return nil
	}
	r.phase = PhaseClosed
	cancel, done := r.runCancel, r.runDone
	r.mu.Unlock()
	if done != nil {
		cancel()
		<-done
	}

	r.mu.Lock()
	defer r.mu.Unlock()
	if r.store == nil {
		return nil
	}
	err := errors.Join(r.cleanupErr, r.store.Close())
	r.store, r.runner = nil, nil
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

// prepareSession creates a new session for profile. A model that is not ready
// still gets its session, opened with the problem that keeps it from running.
func (r *Runtime) prepareSession(profile modelSpec) (opened, error) {
	store, err := r.createSession()
	if err != nil {
		return opened{}, err
	}
	if problem := profile.Ready(); problem != nil {
		return opened{store: store, profile: profile, problem: problem}, nil
	}
	runner, err := r.createRunner(profile, store)
	if err != nil {
		_ = store.Close()
		return opened{}, err
	}
	return opened{store: store, runner: runner, profile: profile}, nil
}

// openStore opens a persisted session on the model it last recorded, so a
// resumed conversation continues with the model that was answering it. When
// that model no longer resolves, or the session never recorded one, it
// continues on the current model instead.
//
// A model change is appended only when the model that will answer differs
// from the session's last record, so the log always says which model wrote
// each reply. Restoring the recorded model adds nothing.
//
// A session another kon has open is opened as a view to follow instead.
func (r *Runtime) openStore(path string) (opened, error) {
	store, err := r.openSession(path)
	if errors.Is(err, session.ErrInUse) {
		return r.openView(path)
	}
	if err != nil {
		return opened{}, err
	}
	last := lastModelChange(store.ActivePath())
	profile := r.recordedProfile(last)
	if problem := profile.Ready(); problem != nil {
		return opened{store: store, profile: profile, problem: problem}, nil
	}
	build := r.createRunner
	if last != nil && last.Name == profile.Name && last.ExternalID.String() == profile.ModelID {
		build = r.buildRunner
	}
	runner, err := build(profile, store)
	if err != nil {
		_ = store.Close()
		return opened{}, err
	}
	return opened{store: store, runner: runner, profile: profile}, nil
}

// recordedProfile is the model a session last recorded, or the current model
// when the session never recorded one, it no longer resolves, or Options
// pinned the model.
func (r *Runtime) recordedProfile(last *session.ModelSelection) modelSpec {
	if last != nil && !r.pinned {
		if recorded, ok := r.configuredSpec(last.Name); ok {
			return recorded
		}
	}
	return r.active
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
