package ui

import (
	"fmt"
	"sort"
	"strings"

	tea "charm.land/bubbletea/v2"
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

// slashCommand is a command registered with a registry.
type slashCommand struct {
	name      string
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
func (r *registry) register(command slashCommand) {
	if command.name == "" {
		panic("ui: slash command with empty name")
	}
	if _, exists := r.byName[command.name]; exists {
		panic("ui: duplicate slash command: " + command.name)
	}
	stored := command
	r.byName[stored.name] = &stored
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

// completeNames suggests registered command names matching prefix, or every
// command when prefix is empty.
func (r *registry) completeNames(prefix string) []menuItem {
	candidates := make([]menuItem, 0, len(r.ordered))
	for _, command := range r.ordered {
		if strings.HasPrefix(command.name, prefix) {
			candidates = append(candidates, menuItem{Value: "/" + command.name, Description: command.summary})
		}
	}
	return candidates
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
	return registry
}

// completeModelNames suggests configured models whose name matches prefix.
func completeModelNames(m Model, prefix string) []menuItem {
	var candidates []menuItem
	for _, option := range m.runtime.Models() {
		if strings.HasPrefix(option.Name, prefix) {
			candidates = append(candidates, menuItem{
				Value:       option.Name,
				Description: option.Provider + "/" + option.ExternalID,
			})
		}
	}
	sort.Slice(candidates, func(i, j int) bool { return candidates[i].Value < candidates[j].Value })
	return candidates
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
		lines = append(lines, marker+option.Name+"  "+option.Provider+"/"+option.ExternalID)
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
	m.transcript.add(block{kind: blockModel, text: m.active.Name + "  " + m.active.Provider + "/" + m.active.ExternalID})
	m.refreshTranscript(true)
	return m, nil
}
