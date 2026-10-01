package migrations

import (
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"strings"
	"time"

	"github.com/hizkifw/kon/core/typedid"
)

// This file freezes session format v4 as the steps that produce it knew it.
// Steps must not read core/session: a later format bump would change what
// a historical step writes and accepts, and the step that converts v4 onward
// expects exactly what these steps produced.

// sessionV4 is the session format version steps 1 and 2 produce and accept.
const sessionV4 = 4

const (
	v4RoleSystem    = "system"
	v4RoleUser      = "user"
	v4RoleAssistant = "assistant"
	v4RoleTool      = "tool"
)

const (
	v4PartReasoning  = "reasoning"
	v4PartText       = "text"
	v4PartToolCall   = "tool_call"
	v4PartImage      = "image"
	v4PartToolResult = "tool_result"
)

// v4Part is one ordered content part of a v4 message.
type v4Part struct {
	Type            string          `json:"type"`
	Text            string          `json:"text,omitempty"`
	ToolCallID      string          `json:"tool_call_id,omitempty"`
	ToolName        string          `json:"tool_name,omitempty"`
	ToolInput       json.RawMessage `json:"tool_input,omitempty"`
	ToolOutput      string          `json:"tool_output,omitempty"`
	ImageHash       string          `json:"image_hash,omitempty"`
	ImageMIME       string          `json:"image_mime,omitempty"`
	ProviderOptions json.RawMessage `json:"provider_options,omitempty"`
}

type v4Message struct {
	Role  string   `json:"role"`
	Parts []v4Part `json:"parts"`
}

type v4Header struct {
	Type    string            `json:"type"`
	Version int               `json:"version"`
	ID      typedid.SessionID `json:"id"`
}

type v4Entry struct {
	Type             string           `json:"type"`
	ID               typedid.EntryID  `json:"id"`
	ParentID         *typedid.EntryID `json:"parent_id"`
	Timestamp        time.Time        `json:"timestamp"`
	Message          *v4Message       `json:"message"`
	Summary          string           `json:"summary"`
	FirstKeptEntryID *typedid.EntryID `json:"first_kept_entry_id"`
	Model            *struct {
		Name       string `json:"name"`
		WireFormat string `json:"wire_format"`
		ExternalID string `json:"external_id"`
	} `json:"model"`
	DurationMS int64 `json:"duration_ms"`
}

// validateV4File checks a session against the v4 rules the session reader
// enforced when v4 was current. As that reader did, it tolerates one torn
// final record, which the reader trims when it opens the file.
func validateV4File(path string) error {
	b, err := os.ReadFile(path)
	if err != nil {
		return fmt.Errorf("read session: %w", err)
	}
	lines := strings.Split(string(b), "\n")
	last := len(lines) - 1
	for last >= 0 && strings.TrimSpace(lines[last]) == "" {
		last--
	}
	if last < 0 {
		return errors.New("empty session")
	}
	var header v4Header
	if err := json.Unmarshal([]byte(lines[0]), &header); err != nil {
		return fmt.Errorf("parse session header: %w", err)
	}
	if header.Type != "session" || header.ID.IsZero() {
		return errors.New("invalid session header")
	}
	if header.Version != sessionV4 {
		return fmt.Errorf("unsupported session version %d", header.Version)
	}
	seen := make(map[typedid.EntryID]bool)
	for i := 1; i <= last; i++ {
		if strings.TrimSpace(lines[i]) == "" {
			continue
		}
		var entry v4Entry
		if err := json.Unmarshal([]byte(lines[i]), &entry); err != nil {
			if i == last {
				break
			}
			return fmt.Errorf("parse session line %d: %w", i+1, err)
		}
		if err := entry.validate(seen); err != nil {
			return fmt.Errorf("session line %d: invalid session entry: %w", i+1, err)
		}
		seen[entry.ID] = true
	}
	return nil
}

func (entry v4Entry) validate(seen map[typedid.EntryID]bool) error {
	if entry.ID.IsZero() || entry.Type == "" {
		return errors.New("missing ID or type")
	}
	if seen[entry.ID] {
		return fmt.Errorf("duplicate session entry id %q", entry.ID)
	}
	if entry.ParentID != nil && !seen[*entry.ParentID] {
		return fmt.Errorf("entry %q has missing parent %q", entry.ID, *entry.ParentID)
	}
	switch entry.Type {
	case "message":
		if entry.Message == nil {
			return errors.New("message entry has no message")
		}
		return entry.Message.validate()
	case "compaction":
		if entry.Summary == "" || entry.FirstKeptEntryID == nil || entry.FirstKeptEntryID.IsZero() {
			return errors.New("compaction entry requires a summary and retained entry ID")
		}
	case "model_change":
		if entry.Model == nil || entry.Model.Name == "" || entry.Model.WireFormat == "" || entry.Model.ExternalID == "" {
			return errors.New("model change requires name, wire format, and external ID")
		}
	case "turn_end":
		if entry.DurationMS < 0 {
			return errors.New("turn end requires a non-negative duration")
		}
	}
	return nil
}

func (m v4Message) validate() error {
	switch m.Role {
	case v4RoleSystem, v4RoleUser:
		text := ""
		for _, part := range m.Parts {
			if part.Type == v4PartText {
				text += part.Text
			}
		}
		if text == "" {
			return fmt.Errorf("%s message content must not be empty", m.Role)
		}
	case v4RoleAssistant:
		hasContent := false
		calls := make(map[string]bool)
		for _, part := range m.Parts {
			if ((part.Type == v4PartText || part.Type == v4PartReasoning) && part.Text != "") || part.Type == v4PartToolCall {
				hasContent = true
			}
			if part.Type != v4PartToolCall {
				continue
			}
			if part.ToolCallID == "" || part.ToolName == "" {
				return errors.New("assistant tool call requires an external ID and function name")
			}
			if calls[part.ToolCallID] {
				return fmt.Errorf("duplicate assistant tool call ID %q", part.ToolCallID)
			}
			calls[part.ToolCallID] = true
		}
		if !hasContent {
			return errors.New("assistant message must contain text, reasoning, or tool calls")
		}
	case v4RoleTool:
		var results []v4Part
		for _, part := range m.Parts {
			if part.Type == v4PartToolResult {
				results = append(results, part)
			}
		}
		if len(results) != 1 || results[0].ToolCallID == "" || results[0].ToolName == "" {
			return errors.New("tool result requires an external tool call ID and name")
		}
	default:
		return fmt.Errorf("unknown message role %q", m.Role)
	}
	for _, part := range m.Parts {
		if part.Type == v4PartImage && (part.Text != "" || !validV4ImageHash(part.ImageHash) || part.ImageMIME == "") {
			return errors.New("image part requires a blob hash and MIME type")
		}
	}
	return nil
}

// validV4ImageHash reports whether hash is a lowercase hex SHA-256 digest.
func validV4ImageHash(hash string) bool {
	if len(hash) != 64 {
		return false
	}
	for _, c := range hash {
		if (c < '0' || c > '9') && (c < 'a' || c > 'f') {
			return false
		}
	}
	return true
}
