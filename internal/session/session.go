// Package session persists an append-only, parent-linked conversation tree.
package session

import (
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"time"

	"github.com/hizkifw/kon/internal/typedid"
)

const SchemaVersion = 1

type Usage struct {
	PromptTokens     int `json:"prompt_tokens"`
	CompletionTokens int `json:"completion_tokens"`
	TotalTokens      int `json:"total_tokens"`
}

type Role string

const (
	RoleSystem    Role = "system"
	RoleUser      Role = "user"
	RoleAssistant Role = "assistant"
	RoleTool      Role = "tool"
)

type EntryType string

const (
	EntryTypeMessage     EntryType = "message"
	EntryTypeCompaction  EntryType = "compaction"
	EntryTypeModelChange EntryType = "model_change"
)

type FinishReason string

type ToolFunction struct {
	Name      string          `json:"name"`
	Arguments json.RawMessage `json:"arguments"`
}

type ToolCall struct {
	ID       typedid.ToolCallID `json:"id"`
	Type     string             `json:"type"`
	Function ToolFunction       `json:"function"`
	Metadata map[string]any     `json:"metadata,omitempty"`
}

// Part preserves ordered, provider-neutral content needed for exact replay.
// ProviderOptions is opaque data owned by the upstream provider.
type Part struct {
	Type            string             `json:"type"`
	Text            string             `json:"text,omitempty"`
	ToolCallID      typedid.ToolCallID `json:"tool_call_id,omitempty"`
	ToolName        string             `json:"tool_name,omitempty"`
	ToolInput       json.RawMessage    `json:"tool_input,omitempty"`
	ToolOutput      string             `json:"tool_output,omitempty"`
	ProviderOptions map[string]any     `json:"provider_options,omitempty"`
}

// Message is provider-neutral while preserving opaque data needed for replay.
type Message struct {
	Role            Role               `json:"role"`
	Content         string             `json:"content,omitempty"`
	ToolCalls       []ToolCall         `json:"tool_calls,omitempty"`
	ToolCallID      typedid.ToolCallID `json:"tool_call_id,omitempty"`
	Name            string             `json:"name,omitempty"`
	Model           typedid.ModelID    `json:"model,omitempty"`
	Finish          FinishReason       `json:"finish_reason,omitempty"`
	Usage           *Usage             `json:"usage,omitempty"`
	Parts           []Part             `json:"parts,omitempty"`
	ProviderOptions map[string]any     `json:"provider_options,omitempty"`
}

func (m Message) Validate() error {
	switch m.Role {
	case RoleSystem, RoleUser:
		if m.Content == "" {
			return fmt.Errorf("%s message content must not be empty", m.Role)
		}
	case RoleAssistant:
		if m.Content == "" && len(m.ToolCalls) == 0 {
			return errors.New("assistant message must contain text or tool calls")
		}
		seen := make(map[typedid.ToolCallID]bool, len(m.ToolCalls))
		for _, call := range m.ToolCalls {
			if call.ID.String() == "" || call.Function.Name == "" {
				return errors.New("assistant tool call requires an external ID and function name")
			}
			if seen[call.ID] {
				return fmt.Errorf("duplicate assistant tool call ID %q", call.ID)
			}
			seen[call.ID] = true
		}
	case RoleTool:
		if m.ToolCallID.String() == "" || m.Name == "" {
			return errors.New("tool result requires an external tool call ID and name")
		}
	default:
		return fmt.Errorf("unknown message role %q", m.Role)
	}
	return nil
}

type Header struct {
	Type       string            `json:"type"`
	Version    int               `json:"version"`
	ID         typedid.SessionID `json:"id"`
	AppVersion string            `json:"app_version"`
	Timestamp  time.Time         `json:"timestamp"`
	CWD        string            `json:"cwd"`
}

type Entry struct {
	Type                  EntryType        `json:"type"`
	ID                    typedid.EntryID  `json:"id"`
	ParentID              *typedid.EntryID `json:"parent_id"`
	Timestamp             time.Time        `json:"timestamp"`
	Message               *Message         `json:"message,omitempty"`
	Summary               string           `json:"summary,omitempty"`
	FirstKeptEntryID      *typedid.EntryID `json:"first_kept_entry_id,omitempty"`
	TokensBefore          int              `json:"tokens_before,omitempty"`
	TokensBeforeEstimated bool             `json:"tokens_before_estimated,omitempty"`
	Usage                 *Usage           `json:"usage,omitempty"`
	Model                 *ModelSelection  `json:"model,omitempty"`
	Raw                   json.RawMessage  `json:"-"`
}

type ModelSelection struct {
	Name       string          `json:"name"`
	Provider   string          `json:"provider"`
	ExternalID typedid.ModelID `json:"external_id"`
}

type ContextMessage struct {
	EntryID typedid.EntryID
	Message Message
}

type Store struct {
	mu      sync.Mutex
	header  Header
	path    string
	file    *os.File
	entries []Entry
	byID    map[typedid.EntryID]int
	leafID  *typedid.EntryID
}

func New(root, cwd, appVersion, systemPrompt string) (*Store, error) {
	absCWD, err := filepath.Abs(cwd)
	if err != nil {
		return nil, fmt.Errorf("resolve working directory: %w", err)
	}
	absCWD = filepath.Clean(absCWD)
	digest := sha256.Sum256([]byte(absCWD))
	dir := filepath.Join(root, hex.EncodeToString(digest[:12]))
	if err := os.MkdirAll(dir, 0o700); err != nil {
		return nil, fmt.Errorf("create session directory: %w", err)
	}

	sessionID, err := typedid.NewSessionID()
	if err != nil {
		return nil, err
	}
	now := time.Now().UTC()
	name := now.Format("20060102T150405.000Z") + "_" + sessionID.String() + ".jsonl"
	path := filepath.Join(dir, name)
	f, err := os.OpenFile(path, os.O_RDWR|os.O_CREATE|os.O_EXCL, 0o600)
	if err != nil {
		return nil, fmt.Errorf("create session: %w", err)
	}
	s := &Store{
		header: Header{Type: "session", Version: SchemaVersion, ID: sessionID, AppVersion: appVersion, Timestamp: now, CWD: absCWD},
		path:   path,
		file:   f,
		byID:   make(map[typedid.EntryID]int),
	}
	if err := s.writeLine(s.header); err != nil {
		f.Close()
		return nil, err
	}
	if _, err := s.AppendMessage(Message{Role: RoleSystem, Content: systemPrompt}); err != nil {
		f.Close()
		return nil, err
	}
	return s, nil
}

func Open(path string) (*Store, error) {
	b, err := os.ReadFile(path)
	if err != nil {
		return nil, fmt.Errorf("read session: %w", err)
	}
	lines := strings.Split(string(b), "\n")
	last := len(lines) - 1
	for last >= 0 && strings.TrimSpace(lines[last]) == "" {
		last--
	}
	if last < 0 {
		return nil, errors.New("empty session")
	}
	var header Header
	if err := json.Unmarshal([]byte(lines[0]), &header); err != nil {
		return nil, fmt.Errorf("parse session header: %w", err)
	}
	if header.Type != "session" || header.ID.IsZero() {
		return nil, errors.New("invalid session header")
	}
	if header.Version != SchemaVersion {
		return nil, fmt.Errorf("unsupported session version %d", header.Version)
	}

	entries := make([]Entry, 0, last)
	byID := make(map[typedid.EntryID]int)
	repairAt := -1
	for i := 1; i <= last; i++ {
		if strings.TrimSpace(lines[i]) == "" {
			continue
		}
		var entry Entry
		if err := json.Unmarshal([]byte(lines[i]), &entry); err != nil {
			if i == last {
				repairAt = i
				break
			}
			return nil, fmt.Errorf("parse session line %d: %w", i+1, err)
		}
		entry.Raw = append(json.RawMessage(nil), lines[i]...)
		if entry.ID.IsZero() || entry.Type == "" {
			return nil, fmt.Errorf("invalid session entry on line %d", i+1)
		}
		if err := entry.validate(); err != nil {
			return nil, fmt.Errorf("invalid session entry on line %d: %w", i+1, err)
		}
		if _, exists := byID[entry.ID]; exists {
			return nil, fmt.Errorf("duplicate session entry id %q", entry.ID)
		}
		if entry.ParentID != nil {
			if _, exists := byID[*entry.ParentID]; !exists {
				return nil, fmt.Errorf("entry %q has missing parent %q", entry.ID, *entry.ParentID)
			}
		}
		byID[entry.ID] = len(entries)
		entries = append(entries, entry)
	}
	if repairAt >= 0 {
		validPrefix := strings.Join(lines[:repairAt], "\n") + "\n"
		if err := os.Truncate(path, int64(len(validPrefix))); err != nil {
			return nil, fmt.Errorf("repair incomplete session tail: %w", err)
		}
	}
	f, err := os.OpenFile(path, os.O_RDWR|os.O_APPEND, 0o600)
	if err != nil {
		return nil, fmt.Errorf("open session for append: %w", err)
	}
	s := &Store{header: header, path: path, file: f, entries: entries, byID: byID}
	if len(entries) > 0 {
		leaf := entries[len(entries)-1].ID
		s.leafID = &leaf
	}
	return s, nil
}

func (s *Store) Path() string { return s.path }
func (s *Store) CWD() string  { return s.header.CWD }

func (s *Store) Close() error {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.file == nil {
		return nil
	}
	err := s.file.Sync()
	closeErr := s.file.Close()
	s.file = nil
	return errors.Join(err, closeErr)
}

func (s *Store) AppendMessage(message Message) (typedid.EntryID, error) {
	if err := message.Validate(); err != nil {
		return typedid.EntryID{}, fmt.Errorf("append message: %w", err)
	}
	return s.append(Entry{Type: EntryTypeMessage, Message: &message})
}

func (s *Store) AppendCompaction(summary string, firstKeptID typedid.EntryID, tokensBefore int, estimated bool, usage *Usage) (typedid.EntryID, error) {
	if firstKeptID.IsZero() {
		return typedid.EntryID{}, errors.New("compaction requires a retained entry")
	}
	return s.append(Entry{
		Type:                  EntryTypeCompaction,
		Summary:               summary,
		FirstKeptEntryID:      &firstKeptID,
		TokensBefore:          tokensBefore,
		TokensBeforeEstimated: estimated,
		Usage:                 usage,
	})
}

func (s *Store) AppendModelChange(selection ModelSelection) (typedid.EntryID, error) {
	if selection.Name == "" || selection.Provider == "" || selection.ExternalID.String() == "" {
		return typedid.EntryID{}, errors.New("model change requires name, provider, and external ID")
	}
	return s.append(Entry{Type: EntryTypeModelChange, Model: &selection})
}

func (s *Store) append(entry Entry) (typedid.EntryID, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.file == nil {
		return typedid.EntryID{}, errors.New("session is closed")
	}
	id, err := typedid.NewEntryID()
	if err != nil {
		return typedid.EntryID{}, err
	}
	entry.ID = id
	entry.Timestamp = time.Now().UTC()
	if s.leafID != nil {
		parent := *s.leafID
		entry.ParentID = &parent
	}
	if err := s.writeLine(entry); err != nil {
		return typedid.EntryID{}, err
	}
	s.byID[id] = len(s.entries)
	s.entries = append(s.entries, entry)
	s.leafID = &id
	return id, nil
}

func (s *Store) writeLine(value any) error {
	b, err := json.Marshal(value)
	if err != nil {
		return fmt.Errorf("encode session entry: %w", err)
	}
	if _, err := s.file.Write(append(b, '\n')); err != nil {
		return fmt.Errorf("append session entry: %w", err)
	}
	if err := s.file.Sync(); err != nil {
		return fmt.Errorf("sync session entry: %w", err)
	}
	return nil
}

// Context walks parent links and applies the newest compaction on that path.
func (s *Store) Context() ([]ContextMessage, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	path, err := s.activePathLocked()
	if err != nil {
		return nil, err
	}
	if len(path) == 0 {
		return nil, nil
	}
	latestCompaction := -1
	for i := range path {
		if path[i].Type == EntryTypeCompaction {
			latestCompaction = i
		}
	}

	var out []ContextMessage
	if latestCompaction < 0 {
		return messagesFromEntries(path), nil
	}
	comp := path[latestCompaction]
	if path[0].Type != EntryTypeMessage || path[0].Message == nil || path[0].Message.Role != RoleSystem {
		return nil, errors.New("session has no root system message")
	}
	system := *path[0].Message
	system.Content += "\n\n<conversation-summary>\n" + comp.Summary + "\n</conversation-summary>"
	out = append(out, ContextMessage{EntryID: path[0].ID, Message: system})

	kept := -1
	for i := 1; i < latestCompaction; i++ {
		if comp.FirstKeptEntryID != nil && path[i].ID == *comp.FirstKeptEntryID {
			kept = i
			break
		}
	}
	if kept < 0 {
		return nil, fmt.Errorf("compaction %q refers to missing entry %v", comp.ID, comp.FirstKeptEntryID)
	}
	out = append(out, messagesFromEntries(path[kept:latestCompaction])...)
	out = append(out, messagesFromEntries(path[latestCompaction+1:])...)
	return out, nil
}

func messagesFromEntries(entries []Entry) []ContextMessage {
	out := make([]ContextMessage, 0, len(entries))
	for _, entry := range entries {
		if entry.Type == EntryTypeMessage && entry.Message != nil {
			out = append(out, ContextMessage{EntryID: entry.ID, Message: *entry.Message})
		}
	}
	return out
}

func (entry Entry) validate() error {
	switch entry.Type {
	case EntryTypeMessage:
		if entry.Message == nil {
			return errors.New("message entry has no message")
		}
		return entry.Message.Validate()
	case EntryTypeCompaction:
		if entry.Summary == "" || entry.FirstKeptEntryID == nil || entry.FirstKeptEntryID.IsZero() {
			return errors.New("compaction entry requires a summary and retained entry ID")
		}
	case EntryTypeModelChange:
		if entry.Model == nil || entry.Model.Name == "" || entry.Model.Provider == "" || entry.Model.ExternalID.String() == "" {
			return errors.New("model change requires name, provider, and external ID")
		}
	}
	// Unknown types retain their envelope for forward-compatible readers.
	return nil
}

func (s *Store) activePathLocked() ([]Entry, error) {
	if s.leafID == nil {
		return nil, nil
	}
	var reverse []Entry
	current := *s.leafID
	seen := make(map[typedid.EntryID]bool)
	for {
		if seen[current] {
			return nil, errors.New("cycle in session parent links")
		}
		seen[current] = true
		idx, ok := s.byID[current]
		if !ok {
			return nil, fmt.Errorf("missing session entry %q", current)
		}
		entry := s.entries[idx]
		reverse = append(reverse, entry)
		if entry.ParentID == nil {
			break
		}
		current = *entry.ParentID
	}
	path := make([]Entry, len(reverse))
	for i := range reverse {
		path[len(reverse)-1-i] = reverse[i]
	}
	return path, nil
}
