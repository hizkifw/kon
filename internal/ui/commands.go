package ui

import (
	"fmt"
	"sort"
	"strings"

	tea "charm.land/bubbletea/v2"
	"github.com/hizkifw/kon/internal/app"
	"github.com/hizkifw/kon/internal/session"
	"github.com/hizkifw/kon/internal/typedid"
)

// argument describes one positional argument accepted by a slash command.
// complete is the argument's autocomplete handler: it receives the partial
// text typed for the argument and returns the matching candidates. It takes the
// full Model so a handler can read the runtime it needs (e.g. the model list).
type argument struct {
	name     string
	optional bool
	complete func(m Model, prefix string) []menuItem
}

// commandFunc runs a parsed slash command. args contains one entry per
// positional argument, already validated against the command's arguments.
type commandFunc func(m Model, args []string) (tea.Model, tea.Cmd)

// slashCommand is a command registered with a registry. aliases are alternate
// spellings that resolve to the same command; the canonical name is the one
// shown in usage and the popup's primary row.
type slashCommand struct {
	name      string
	aliases   []string
	summary   string
	arguments []argument
	run       commandFunc
}

// usage renders the command's usage line, e.g. "/model [name]".
func (c slashCommand) usage() string {
	parts := []string{"/" + c.name}
	for _, arg := range c.arguments {
		token := "<" + arg.name + ">"
		if arg.optional {
			token = "[" + arg.name + "]"
		}
		parts = append(parts, token)
	}
	return strings.Join(parts, " ")
}

// parsedCommand is a command resolved from user input together with its
// validated positional arguments.
type parsedCommand struct {
	command *slashCommand
	args    []string
}

// registry is the central catalog of slash commands. Commands are registered
// once at startup; the registry owns name lookup, argument validation, and
// autocomplete dispatch. It is package-internal: the popup types it returns
// (menuItem) are unexported too.
type registry struct {
	ordered []*slashCommand
	byName  map[string]*slashCommand
}

// newRegistry returns an empty command registry.
func newRegistry() *registry {
	return &registry{byName: map[string]*slashCommand{}}
}

// register adds a command. It panics on a duplicate or empty name because the
// registry is assembled once at startup and duplicates are a programming error.
// Aliases are indexed to the same command; a name or alias that collides with
// any existing spelling is rejected.
func (r *registry) register(command slashCommand) {
	if command.name == "" {
		panic("ui: slash command with empty name")
	}
	if _, exists := r.byName[command.name]; exists {
		panic("ui: duplicate slash command: " + command.name)
	}
	stored := command
	r.byName[stored.name] = &stored
	for _, alias := range command.aliases {
		if alias == "" {
			panic("ui: slash command alias with empty name")
		}
		if _, exists := r.byName[alias]; exists {
			panic("ui: duplicate slash command: " + alias)
		}
		r.byName[alias] = &stored
	}
	r.ordered = append(r.ordered, &stored)
}

// parse resolves raw input such as "/model review" into a command and its
// arguments, rejecting unknown commands, too many arguments, and missing
// required arguments.
func (r *registry) parse(text string) (parsedCommand, error) {
	fields := strings.Fields(text)
	if len(fields) == 0 {
		return parsedCommand{}, nil
	}
	name := strings.TrimPrefix(fields[0], "/")
	command, ok := r.byName[name]
	if !ok {
		return parsedCommand{}, fmt.Errorf("unknown command: %s", fields[0])
	}
	args := fields[1:]
	if len(args) > len(command.arguments) {
		return parsedCommand{}, usageError(command.usage())
	}
	for i, arg := range command.arguments {
		if !arg.optional && i >= len(args) {
			return parsedCommand{}, usageError(command.usage())
		}
	}
	return parsedCommand{command: command, args: args}, nil
}

// run executes a parsed command.
func (p parsedCommand) run(m Model) (tea.Model, tea.Cmd) {
	return p.command.run(m, p.args)
}

// Candidates implements menuSource, so the command registry plugs into the
// generic popup widget directly.
func (r *registry) Candidates(m Model, input string) []menuItem {
	return r.completion(m, input)
}

var _ menuSource = (*registry)(nil)

// Accept implements menuSource: it fills the prompt with value, replacing the
// trailing token. The appended space ends the current segment: completing a
// command with no more arguments (e.g. "/new") leaves no matching candidate and
// closes the popup, while "/model" advances to its argument list.
func (r *registry) Accept(m *Model, value string) {
	m.input.SetValue(replaceToken(m.input.Value(), value) + " ")
	m.input.CursorEnd()
}

// completion returns the autocomplete candidates for input. Only input
// beginning with "/" at the very start of the message is treated as a slash
// command; once any other text precedes it the command is plain prompt text
// and no completion is offered.
func (r *registry) completion(m Model, input string) []menuItem {
	if !strings.HasPrefix(input, "/") {
		return nil
	}
	body := input[1:]
	if !strings.ContainsAny(body, " \t") {
		return r.completeNames(body)
	}
	fields := strings.Fields(body)
	if len(fields) == 0 {
		return nil
	}
	command, ok := r.byName[fields[0]]
	if !ok {
		return nil
	}
	typed, prefix := splitArguments(strings.TrimPrefix(body, fields[0]))
	index := len(typed)
	if index >= len(command.arguments) {
		return nil
	}
	argument := command.arguments[index]
	if argument.complete == nil {
		return nil
	}
	return argument.complete(m, prefix)
}

// completeNames suggests commands whose canonical name or an alias matches
// prefix, or every command when prefix is empty. Aliases match for completion
// but never get their own row: selecting one fills in the canonical name, so the
// alias is a typing shortcut rather than a second entry.
func (r *registry) completeNames(prefix string) []menuItem {
	candidates := make([]menuItem, 0, len(r.ordered))
	for _, command := range r.ordered {
		if !command.matches(prefix) {
			continue
		}
		candidates = append(candidates, menuItem{Value: "/" + command.name, Description: command.summary})
	}
	return candidates
}

// matches reports whether prefix matches the command's canonical name or any
// alias.
func (c slashCommand) matches(prefix string) bool {
	if strings.HasPrefix(c.name, prefix) {
		return true
	}
	for _, alias := range c.aliases {
		if strings.HasPrefix(alias, prefix) {
			return true
		}
	}
	return false
}

// splitArguments separates completed argument tokens from the token currently
// being typed. rest is the input after the command name, including the leading
// separator when present.
func splitArguments(rest string) (typed []string, current string) {
	rest = strings.TrimLeft(rest, " \t")
	if rest == "" {
		return nil, ""
	}
	fields := strings.Fields(rest)
	if strings.HasSuffix(rest, " ") || strings.HasSuffix(rest, "\t") {
		return fields, ""
	}
	return fields[:len(fields)-1], fields[len(fields)-1]
}

func usageError(usage string) error { return fmt.Errorf("usage: %s", usage) }

// defaultRegistry registers kon's built-in slash commands.
func defaultRegistry() *registry {
	registry := newRegistry()
	registry.register(slashCommand{
		name:    "new",
		aliases: []string{"clear"},
		summary: "start a new session",
		run: func(m Model, _ []string) (tea.Model, tea.Cmd) {
			return m.newSession()
		},
	})
	registry.register(slashCommand{
		name:    "model",
		summary: "list or switch models",
		arguments: []argument{{
			name:     "name",
			optional: true,
			complete: completeModelNames,
		}},
		run: func(m Model, args []string) (tea.Model, tea.Cmd) {
			if len(args) == 0 {
				return m.listModels()
			}
			return m.switchModel(args[0])
		},
	})
	registry.register(slashCommand{
		name:    "login",
		summary: "connect a provider",
		arguments: []argument{{
			name: "provider",
			complete: func(m Model, prefix string) []menuItem {
				var options []menuItem
				for _, provider := range m.runtime.LoginProviders() {
					if strings.HasPrefix(provider, prefix) {
						options = append(options, menuItem{Value: provider})
					}
				}
				return options
			},
		}},
		run: func(m Model, args []string) (tea.Model, tea.Cmd) {
			return m.startLogin(args[0])
		},
	})
	registry.register(slashCommand{
		name:    "resume",
		summary: "resume a previous session",
		arguments: []argument{{
			name:     "id",
			optional: true,
			complete: completeSessionIDs,
		}},
		run: func(m Model, args []string) (tea.Model, tea.Cmd) {
			return m.resume(args)
		},
	})
	registry.register(slashCommand{
		name:    "compact",
		summary: "summarize older context now",
		run: func(m Model, _ []string) (tea.Model, tea.Cmd) {
			return m.compact()
		},
	})
	return registry
}

// completeModelNames keeps stable model names as values while showing names
// from the catalog in the picker.
func completeModelNames(m Model, prefix string) []menuItem {
	var candidates []menuItem
	for _, option := range m.runtime.Models() {
		label := modelLabel(option)
		if strings.HasPrefix(option.Name, prefix) || strings.Contains(strings.ToLower(option.DisplayName), strings.ToLower(prefix)) {
			candidates = append(candidates, menuItem{
				Value:       option.Name,
				Label:       label,
				Description: option.ExternalID,
			})
		}
	}
	sort.Slice(candidates, func(i, j int) bool { return candidates[i].Value < candidates[j].Value })
	return candidates
}

func modelLabel(option app.Model) string {
	name := option.DisplayName
	if name == "" {
		name = option.Name
	}
	connectionID := option.ConnectionID
	if connectionID == "" {
		connectionID = option.Type
	}
	if connectionID == "" {
		return name
	}
	return connectionID + " · " + name
}

func (m Model) newSession() (tea.Model, tea.Cmd) {
	if err := m.runtime.NewSession(); err != nil {
		m.status = "error: " + err.Error()
		return m, nil
	}
	m.transcript.reset()
	m.contextTokens = -1
	m.input.Reset()
	m.history.resetPosition()
	m.syncRuntimeState()
	m.status = "new session"
	m.refreshTranscript(true)
	return m, nil
}

func (m Model) listModels() (tea.Model, tea.Cmd) {
	var lines []string
	for _, option := range m.runtime.Models() {
		marker := "  "
		if option.Name == m.active.Name {
			marker = "* "
		}
		line := marker + modelLabel(option) + "  " + option.ExternalID
		if option.Source != "" {
			line += "  [" + option.Source + "]"
		}
		lines = append(lines, line)
	}
	m.transcript.add(block{kind: blockModels, text: strings.Join(lines, "\n")})
	m.input.Reset()
	m.status = "switch with /model [name]"
	m.refreshTranscript(true)
	return m, nil
}

func (m Model) switchModel(name string) (tea.Model, tea.Cmd) {
	if name == m.active.Name {
		m.input.Reset()
		m.status = "already using " + name
		return m, nil
	}
	if err := m.runtime.SwitchModel(name); err != nil {
		m.status = "error: " + err.Error()
		return m, nil
	}
	m.syncRuntimeState()
	m.contextTokens = -1
	m.input.Reset()
	m.status = "model: " + name + " (saved to config)"
	m.transcript.add(block{kind: blockModel, text: m.active.Name + "  " + m.active.Type + "/" + m.active.ExternalID})
	m.refreshTranscript(true)
	return m, nil
}

// compact forces a manual context compaction. It refuses to run while another
// operation is in flight and reports when there is nothing safe to compact.
func (m Model) compact() (tea.Model, tea.Cmd) {
	if m.busy {
		m.status = "agent is busy; Esc interrupts"
		return m, nil
	}
	state := m.runtime.State()
	if !state.Ready() {
		m.status = state.Problem.Error() + " in " + m.configPath
		return m, nil
	}
	m.input.Reset()
	return m.startRun("compacting…", m.runtime.Compact)
}

// completeSessionIDs suggests resumable sessions for this workspace, newest
// first, with a short creation time as the description. Each row can preview
// the session in the transcript without opening it.
func completeSessionIDs(m Model, prefix string) []menuItem {
	summaries, err := m.runtime.Sessions()
	if err != nil {
		return nil
	}
	var candidates []menuItem
	for _, summary := range summaries {
		id := summary.ID.String()
		if !strings.HasPrefix(id, prefix) {
			continue
		}
		description := summary.CreatedAt.Local().Format("2006-01-02 15:04")
		if summary.Title != "" {
			description = summary.Title + " · " + description
		}
		candidates = append(candidates, menuItem{
			Value:       id,
			Description: description,
			Preview:     previewSession(m, summary),
		})
	}
	return candidates
}

// previewTurns is how many of a session's most recent user turns a resume
// preview replays. The full transcript is available after switching; the
// preview only has to identify the session, so it stays cheap on large files.
const previewTurns = 2

// previewSession returns a lazy builder for a session's read-only transcript.
// Building it up front would read and render every candidate even when the
// popup is never opened; the closure runs only when its row is highlighted. It
// reads just the session's trailing turns and leads with a marker so the
// preview is never mistaken for the live conversation.
func previewSession(m Model, summary session.Summary) func() *transcript {
	return func() *transcript {
		entries, err := m.runtime.SessionPreview(summary.Path, previewTurns)
		if err != nil || len(entries) == 0 {
			return nil
		}
		preview := &transcript{}
		preview.add(block{kind: blockContext, text: "preview " + summary.ID.String() + " · last " + pluralTurns(previewTurns) + " · Esc to cancel"})
		m.applyHistoryTo(preview, entries)
		return preview
	}
}

// pluralTurns renders "N turns" for the preview marker.
func pluralTurns(n int) string {
	if n == 1 {
		return "1 turn"
	}
	return fmt.Sprintf("%d turns", n)
}

// resume switches to a persisted session. With no argument it lists the
// candidates; with an ID it resumes that session and replays it into the
// transcript.
func (m Model) resume(args []string) (tea.Model, tea.Cmd) {
	if len(args) == 0 {
		return m.listSessions()
	}
	id, err := typedid.ParseSessionID(args[0])
	if err != nil {
		m.input.Reset()
		m.status = "error: " + err.Error()
		return m, nil
	}
	if current := m.runtime.SessionID(); current == id {
		m.input.Reset()
		m.status = "already on " + id.String()
		return m, nil
	}
	if err := m.runtime.Resume(id); err != nil {
		m.input.Reset()
		m.status = "error: " + err.Error()
		return m, nil
	}
	m.transcript.reset()
	m.contextTokens = -1
	m.applyHistory(m.runtime.SessionHistory())
	m.seedContextUsage()
	m.input.Reset()
	m.history.resetPosition()
	m.syncRuntimeState()
	m.status = "resumed " + id.String()
	m.refreshTranscript(true)
	m.viewport.GotoBottom()
	return m, nil
}

// listSessions renders the resumable sessions for this workspace.
func (m Model) listSessions() (tea.Model, tea.Cmd) {
	summaries, err := m.runtime.Sessions()
	if err != nil {
		m.input.Reset()
		m.status = "error: " + err.Error()
		return m, nil
	}
	m.input.Reset()
	current := m.runtime.SessionID().String()
	if len(summaries) == 0 {
		m.status = "no sessions to resume"
		return m, nil
	}
	lines := make([]string, 0, len(summaries))
	for _, summary := range summaries {
		marker := "  "
		if summary.ID.String() == current {
			marker = "* "
		}
		line := marker + summary.ID.String() + "  " + summary.CreatedAt.Local().Format("2006-01-02 15:04")
		if summary.Title != "" {
			line += "  " + summary.Title
		}
		lines = append(lines, line)
	}
	m.transcript.add(block{kind: blockModels, text: strings.Join(lines, "\n")})
	m.status = "resume with /resume [id]"
	m.refreshTranscript(true)
	return m, nil
}
