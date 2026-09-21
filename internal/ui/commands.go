package ui

import (
	"fmt"
	"strings"

	tea "charm.land/bubbletea/v2"
)

type slashCommand struct{ name, argument string }

func parseSlashCommand(text string) (slashCommand, error) {
	fields := strings.Fields(text)
	if len(fields) == 0 {
		return slashCommand{}, nil
	}
	switch fields[0] {
	case "/new":
		if len(fields) != 1 {
			return slashCommand{}, errorsUsage("/new")
		}
		return slashCommand{name: "new"}, nil
	case "/model":
		if len(fields) > 2 {
			return slashCommand{}, errorsUsage("/model [name]")
		}
		command := slashCommand{name: "model"}
		if len(fields) == 2 {
			command.argument = fields[1]
		}
		return command, nil
	default:
		return slashCommand{}, fmt.Errorf("unknown command: %s", fields[0])
	}
}

func errorsUsage(usage string) error { return fmt.Errorf("usage: %s", usage) }

func (m Model) executeCommand(command slashCommand) (tea.Model, tea.Cmd) {
	if m.busy {
		m.status = "cancel the active run first"
		return m, nil
	}
	switch command.name {
	case "new":
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
	case "model":
		if command.argument == "" {
			return m.listModels()
		}
		return m.switchModel(command.argument)
	default:
		m.status = "unknown command"
		return m, nil
	}
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
	m.status = "switch with /model <name>"
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
