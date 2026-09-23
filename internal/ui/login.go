package ui

import (
	"context"
	"fmt"
	"strings"

	"charm.land/bubbles/v2/textinput"
	tea "charm.land/bubbletea/v2"
	"github.com/hizkifw/kon/internal/config"
)

type loginFlow struct {
	connection config.Provider
	input      textinput.Model
	step       int
	pending    bool
	cancel     context.CancelFunc
}

type loginDoneMsg struct {
	flow     *loginFlow
	count    int
	verified bool
	err      error
}

func (m Model) startLogin(providerID string) (tea.Model, tea.Cmd) {
	if m.busy {
		m.status = "agent is busy; Ctrl+C cancels"
		return m, nil
	}
	input := textinput.New()
	input.CharLimit = 0
	input.SetWidth(max(1, m.width-2))
	input.Focus()
	connection, ok := m.runtime.LoginConnection(providerID)
	if !ok {
		m.status = "unsupported provider: " + providerID
		return m, nil
	}
	m.login = &loginFlow{
		connection: connection,
		input:      input,
	}
	m.input.Reset()
	m.resetMenu()
	m.login.setStep()
	m.status = m.login.question()
	m.resize()
	return m, nil
}

func (f *loginFlow) wantsURL() bool {
	return f.connection.Type == "ollama" || f.connection.BaseURL == "" && f.connection.Type == "openai-compatible"
}

func (f *loginFlow) setStep() {
	f.input.SetValue("")
	f.input.EchoMode = textinput.EchoNormal
	if f.wantsURL() && f.step == 0 {
		if f.connection.Type == "ollama" {
			f.input.Placeholder = "http://localhost:11434 (Enter for default)"
		} else {
			f.input.Placeholder = "https://example.com/v1"
		}
		return
	}
	f.input.Placeholder = "API key (hidden)"
	f.input.EchoMode = textinput.EchoPassword
}

func (f *loginFlow) question() string {
	if f.pending {
		return "checking " + f.connection.ID + "… Esc cancels"
	}
	if f.wantsURL() && f.step == 0 {
		return "provider URL for " + f.connection.ID + " · Esc cancels"
	}
	return "API key for " + f.connection.ID + " · Esc cancels"
}

func (m Model) updateLogin(msg tea.Msg) (tea.Model, tea.Cmd) {
	f := m.login
	if f == nil {
		return m, nil
	}
	if key, ok := msg.(tea.KeyPressMsg); ok {
		switch key.String() {
		case "esc", "ctrl+c":
			if f.cancel != nil {
				f.cancel()
			}
			m.login = nil
			m.status = "login cancelled"
			m.resize()
			return m, nil
		case "enter":
			if f.pending {
				return m, nil
			}
			value := strings.TrimSpace(f.input.Value())
			if f.wantsURL() && f.step == 0 {
				if value == "" && f.connection.Type == "ollama" {
					value = "http://localhost:11434"
				}
				if value == "" {
					m.status = "provider URL is required"
					return m, nil
				}
				f.connection.BaseURL = value
				if f.connection.Type == "ollama" {
					return m.beginLogin()
				}
				f.step++
				f.setStep()
				m.status = f.question()
				return m, nil
			}
			if value == "" && f.connection.ID != "openai-compatible" {
				m.status = "API key is required"
				return m, nil
			}
			f.connection.APIKey = value
			return m.beginLogin()
		}
	}
	if f.pending {
		return m, nil
	}
	var cmd tea.Cmd
	f.input, cmd = f.input.Update(msg)
	return m, cmd
}

func (m Model) beginLogin() (tea.Model, tea.Cmd) {
	m.login.pending = true
	m.status = m.login.question()
	ctx, cancel := context.WithCancel(context.Background())
	m.login.cancel = cancel
	connection := m.login.connection
	flow := m.login
	return m, func() tea.Msg {
		count, verified, err := m.runtime.Login(ctx, connection)
		return loginDoneMsg{flow: flow, count: count, verified: verified, err: err}
	}
}

func (m Model) finishLogin(done loginDoneMsg) (tea.Model, tea.Cmd) {
	if m.login == nil || m.login != done.flow {
		return m, nil
	}
	m.login.pending = false
	m.login.cancel = nil
	if done.err != nil {
		m.status = "login failed: " + done.err.Error()
		return m, nil
	}
	providerID := m.login.connection.ID
	m.login = nil
	m.syncRuntimeState()
	if done.verified {
		m.status = fmt.Sprintf("%s connected · %d models found · choose with /model", providerID, done.count)
	} else {
		m.status = providerID + " saved · model listing unavailable · add an explicit model"
	}
	m.resize()
	return m, nil
}
