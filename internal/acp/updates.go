package acp

import (
	"encoding/json"
	"path/filepath"
	"strings"

	"kon.kitsu.red/core/acp"
	"kon.kitsu.red/core/agent"
	"kon.kitsu.red/core/session"
	"kon.kitsu.red/core/typedid"
	"kon.kitsu.red/internal/codetools"
)

// translator turns one turn's agent events into session updates.
type translator struct {
	s *liveSession
	// shown is the live output last sent for each running call. The shell
	// reports its tail ten times a second whether or not it changed, and
	// only a change is worth a message.
	shown map[typedid.ToolCallID]string
}

func (s *liveSession) newTranslator() *translator {
	return &translator{s: s, shown: map[typedid.ToolCallID]string{}}
}

func (tr *translator) event(e agent.Event) {
	s := tr.s
	switch e.Kind {
	case agent.EventText:
		s.update(acp.ContentChunk{SessionUpdate: acp.UpdateAgentMessage, Content: acp.TextBlock(e.Text)})
	case agent.EventThinking:
		s.update(acp.ContentChunk{SessionUpdate: acp.UpdateAgentThought, Content: acp.TextBlock(e.Text)})
	case agent.EventSteered:
		s.update(acp.ContentChunk{SessionUpdate: acp.UpdateUserMessage, Content: acp.TextBlock(e.Text)})
	case agent.EventToolStart:
		s.update(s.startedCall(e.CallID, e.Tool, json.RawMessage(e.Arguments)))
	case agent.EventToolOutput:
		d, ok := e.Progress.(codetools.Display)
		if !ok {
			return
		}
		text := strings.Join(d.Lines, "\n")
		if text == "" || text == tr.shown[e.CallID] {
			return
		}
		tr.shown[e.CallID] = text
		s.update(acp.ToolCall{SessionUpdate: acp.UpdateToolProgress, ToolCallID: e.CallID.String(), Content: textContent(text)})
	case agent.EventToolDone:
		delete(tr.shown, e.CallID)
		s.update(s.finishedCall(e.CallID, e.Tool, json.RawMessage(e.Arguments), e.Text, e.IsError, e.Details))
	case agent.EventUsage:
		// A negative count only resets the estimate after a compaction.
		if e.Tokens >= 0 {
			s.reportUsage(int64(e.Tokens))
		}
	}
}

// toolArgs is what kon's tools take that ACP has a place for.
type toolArgs struct {
	Path    string  `json:"path"`
	OldText *string `json:"old_text"`
	NewText *string `json:"new_text"`
	Content *string `json:"content"`
}

func toolKind(name string) string {
	switch name {
	case "read":
		return "read"
	case "write", "edit":
		return "edit"
	case "shell":
		return "execute"
	}
	return "other"
}

// startedCall is the tool_call announcing a call. A write or an edit carries
// its change as a diff from the start, so a client can show what it does
// before it is done.
func (s *liveSession) startedCall(id typedid.ToolCallID, name string, raw json.RawMessage) acp.ToolCall {
	title := name
	if summary := codetools.Summarize(name, raw, s.cwd); summary != "" {
		title += " " + summary
	}
	call := acp.ToolCall{SessionUpdate: acp.UpdateToolCall, ToolCallID: id.String(), Title: title, Name: name, Kind: toolKind(name), Status: acp.StatusInProgress, RawInput: rawInput(raw)}
	var args toolArgs
	if toolKind(name) == "other" || name == "shell" || json.Unmarshal(raw, &args) != nil || args.Path == "" {
		return call
	}
	path := args.Path
	if !filepath.IsAbs(path) {
		path = filepath.Join(s.cwd, path)
	}
	call.Locations = []acp.Location{{Path: path}}
	switch {
	case name == "edit" && args.NewText != nil:
		call.Content = []acp.ToolCallContent{{Type: "diff", Path: path, OldText: args.OldText, NewText: args.NewText}}
	case name == "write" && args.Content != nil:
		call.Content = []acp.ToolCallContent{{Type: "diff", Path: path, NewText: args.Content}}
	}
	return call
}

// finishedCall is the tool_call_update ending a call. A write's or an edit's
// diff stands as its content unless it failed; anything else shows the output
// the model sees.
func (s *liveSession) finishedCall(id typedid.ToolCallID, name string, raw json.RawMessage, output string, failed bool, details json.RawMessage) acp.ToolCall {
	call := acp.ToolCall{SessionUpdate: acp.UpdateToolProgress, ToolCallID: id.String(), Status: acp.StatusCompleted, RawOutput: details}
	if failed {
		call.Status = acp.StatusFailed
	}
	if toolKind(name) != "edit" || failed {
		call.Content = textContent(output)
	}
	return call
}

func textContent(text string) []acp.ToolCallContent {
	if text == "" {
		return nil
	}
	block := acp.TextBlock(text)
	return []acp.ToolCallContent{{Type: "content", Content: &block}}
}

// rawInput passes arguments through as JSON when they are JSON, and as a
// string when a model sent something else.
func rawInput(raw json.RawMessage) json.RawMessage {
	if json.Valid(raw) {
		return raw
	}
	quoted, _ := json.Marshal(string(raw))
	return quoted
}

// replay sends the conversation on the active path as the updates its turns
// sent when they ran, for session/load.
func (s *liveSession) replay() {
	for _, entry := range s.runtime.SessionHistory() {
		m := entry.Message
		if m == nil {
			continue
		}
		switch m.Role {
		case session.RoleUser:
			if text := m.Text(); text != "" {
				s.update(acp.ContentChunk{SessionUpdate: acp.UpdateUserMessage, Content: acp.TextBlock(text)})
			}
		case session.RoleAssistant:
			for _, part := range m.Parts {
				switch part.Type {
				case session.PartReasoning:
					if part.Text != "" {
						s.update(acp.ContentChunk{SessionUpdate: acp.UpdateAgentThought, Content: acp.TextBlock(part.Text)})
					}
				case session.PartText:
					if part.Text != "" {
						s.update(acp.ContentChunk{SessionUpdate: acp.UpdateAgentMessage, Content: acp.TextBlock(part.Text)})
					}
				case session.PartToolCall:
					s.update(s.startedCall(part.ToolCallID, part.ToolName, part.ToolInput))
				}
			}
		case session.RoleTool:
			for _, part := range m.Parts {
				if part.Type == session.PartToolResult {
					s.update(s.finishedCall(part.ToolCallID, part.ToolName, nil, part.ToolOutput, m.IsError, m.Details))
				}
			}
		}
	}
}

// configOptions describes the session's model and, when the model has any,
// its reasoning efforts. The catalog is loaded first: it names the models
// and gives a derived model its levels.
func (s *liveSession) configOptions() []acp.ConfigOption {
	s.runtime.LoadCatalog()
	active := s.runtime.State().Active
	models := acp.ConfigOption{ID: acp.ConfigModel, Name: "Model", Category: "model", Type: "select", CurrentValue: active.Name}
	listed := false
	for _, m := range s.runtime.Models() {
		listed = listed || m.Name == active.Name
		models.Options = append(models.Options, acp.ConfigChoice{Value: m.Name, Name: modelLabel(m.Name, m.DisplayName)})
	}
	// The active model may be one /model would not list, such as a
	// configured one whose provider is gone; a select must offer its value.
	if !listed && active.Name != "" {
		models.Options = append([]acp.ConfigChoice{{Value: active.Name, Name: modelLabel(active.Name, active.DisplayName)}}, models.Options...)
	}
	// With no model to choose from there is nothing to offer: a select
	// needs options.
	var options []acp.ConfigOption
	if len(models.Options) > 0 {
		options = append(options, models)
	}
	if len(active.ReasoningEfforts) > 0 {
		effort := acp.ConfigOption{ID: acp.ConfigEffort, Name: "Reasoning effort", Category: "thought_level", Type: "select", CurrentValue: acp.EffortDefault,
			Options: []acp.ConfigChoice{{Value: acp.EffortDefault, Name: "Provider default"}}}
		if active.ReasoningEffort != "" {
			effort.CurrentValue = active.ReasoningEffort
		}
		for _, level := range active.ReasoningEfforts {
			effort.Options = append(effort.Options, acp.ConfigChoice{Value: level, Name: level})
		}
		options = append(options, effort)
	}
	return options
}

func modelLabel(name, display string) string {
	if display == "" || display == name {
		return name
	}
	return display + " (" + name + ")"
}

// spend is what the session has cost so far, its subagents included.
func (s *liveSession) spend() float64 {
	return session.TotalUsage(s.runtime.SessionHistory()).Cost + s.runtime.SubagentUsage().Cost
}
