// Package session persists an append-only, parent-linked conversation tree.
package session

import (
	"bufio"
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"sync"
	"time"

	"github.com/hizkifw/kon/internal/typedid"
)

const SchemaVersion = 1

// fileSuffix ends every persisted session file. Names begin with a fixed-width
// UTC timestamp, so lexical order is creation order.
const fileSuffix = ".jsonl"

// A projected compaction summary is delivered as a user message wrapped in
// these markers rather than folded into the system prompt. Keeping the system
// prompt byte-identical across compactions preserves the stable prefix that
// provider prompt caches key on.
const (
	CompactionSummaryPrefix = "The conversation history before this point was compacted into the following summary:\n\n<summary>\n"
	CompactionSummarySuffix = "\n</summary>"
)

// InterruptedToolResult is the model-facing result synthesized for a tool call
// that never ran because its turn was cancelled or the process exited. Both the
// agent, which writes it when a turn is cancelled, and context projection, which
// recreates it for a crash, use this exact string.
const InterruptedToolResult = "not executed: interrupted"

type Usage struct {
	PromptTokens     int `json:"prompt_tokens"`
	CompletionTokens int `json:"completion_tokens"`
	TotalTokens      int `json:"total_tokens"`
	// CachedTokens is the provider-reported share of PromptTokens that hit a
	// prompt cache. PromptTokens always covers every input token.
	CachedTokens int `json:"cached_tokens,omitempty"`
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

// Content part types. Parts carry provider-native blocks that the plain string
// fields cannot express; an image part holds base64-encoded bytes in Text.
const (
	PartReasoning  = "reasoning"
	PartText       = "text"
	PartToolCall   = "tool_call"
	PartImage      = "image"
	PartToolResult = "tool_result"
)

// Message is provider-neutral while preserving opaque data needed for replay.
type Message struct {
	Role            Role               `json:"role"`
	Content         string             `json:"content,omitempty"`
	ToolCalls       []ToolCall         `json:"tool_calls,omitempty"`
	ToolCallID      typedid.ToolCallID `json:"tool_call_id,omitempty"`
	Name            string             `json:"name,omitempty"`
	IsError         bool               `json:"is_error,omitempty"`
	Details         json.RawMessage    `json:"details,omitempty"`
	Model           typedid.ModelID    `json:"model,omitempty"`
	Finish          FinishReason       `json:"finish_reason,omitempty"`
	Usage           *Usage             `json:"usage,omitempty"`
	Parts           []Part             `json:"parts,omitempty"`
	ProviderOptions map[string]any     `json:"provider_options,omitempty"`
	// Interrupted marks an assistant message persisted from a stream that ended
	// early (user cancellation or a dropped connection) rather than a provider
	// finish reason. The partial text and reasoning are kept so the turn can be
	// replayed and continued; a completed turn leaves this false.
	Interrupted bool `json:"interrupted,omitempty"`
}

// hasTextPart reports whether any ordered part carries text. An assistant turn
// that was interrupted mid-stream may hold only reasoning, with no answer text
// and no tool calls, and must still persist so the partial turn is retained.
func hasTextPart(m Message) bool {
	for _, part := range m.Parts {
		if part.Text != "" {
			return true
		}
	}
	return false
}

func (m Message) Validate() error {
	switch m.Role {
	case RoleSystem, RoleUser:
		if m.Content == "" {
			return fmt.Errorf("%s message content must not be empty", m.Role)
		}
	case RoleAssistant:
		if m.Content == "" && len(m.ToolCalls) == 0 && !hasTextPart(m) {
			return errors.New("assistant message must contain text, reasoning, or tool calls")
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
	// Summary marks a synthetic message projected from a compaction entry. It
	// carries no entry of its own and is never a valid compaction cut point.
	Summary bool
}

type Store struct {
	mu      sync.Mutex
	header  Header
	path    string
	file    *os.File
	entries []Entry
	byID    map[typedid.EntryID]int
	leafID  *typedid.EntryID
	// empty is true while the session holds nothing beyond its root system
	// message. Such a session is discarded on close so an accidental launch does
	// not leave a resumable file behind, which would otherwise shadow an earlier
	// session that actually has content.
	empty bool
}

func New(root, cwd, appVersion, systemPrompt string) (*Store, error) {
	dir, err := directoryFor(root, cwd)
	if err != nil {
		return nil, err
	}
	absCWD, err := filepath.Abs(cwd)
	if err != nil {
		return nil, fmt.Errorf("resolve working directory: %w", err)
	}
	absCWD = filepath.Clean(absCWD)
	if err := os.MkdirAll(dir, 0o700); err != nil {
		return nil, fmt.Errorf("create session directory: %w", err)
	}

	sessionID, err := typedid.NewSessionID()
	if err != nil {
		return nil, err
	}
	now := time.Now().UTC()
	name := now.Format("20060102T150405.000Z") + "_" + sessionID.String() + fileSuffix
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
		empty:  true,
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
	parsed, err := parseSession(path)
	if err != nil {
		return nil, err
	}
	if parsed.repairOffset >= 0 {
		if err := os.Truncate(path, parsed.repairOffset); err != nil {
			return nil, fmt.Errorf("repair incomplete session tail: %w", err)
		}
	}
	f, err := os.OpenFile(path, os.O_RDWR|os.O_APPEND, 0o600)
	if err != nil {
		return nil, fmt.Errorf("open session for append: %w", err)
	}
	s := &Store{header: parsed.header, path: path, file: f, entries: parsed.entries, byID: parsed.byID}
	// A session holding only its root system message and structural entries has
	// no conversation to keep.
	s.empty = true
	for _, entry := range parsed.entries {
		if entry.Type == EntryTypeCompaction || (entry.Message != nil && entry.Message.Role != RoleSystem) {
			s.empty = false
			break
		}
	}
	s.leafID = parsed.leafID()
	return s, nil
}

// tailBlock bounds how much of a file a tail read pulls in per step.
const tailBlock = 256 << 10

// TailEntries reads at most the last maxTurns user turns of a session for
// read-only display, without parsing the whole file. Sessions grow without
// bound, so a preview that parsed every line would scale with the transcript
// rather than the glance it is. It never repairs or opens the file for append,
// so it is safe to call against a session a live runtime is still writing.
func TailEntries(path string, maxTurns int) ([]Entry, error) {
	if maxTurns < 1 {
		maxTurns = 1
	}
	f, err := os.Open(path)
	if err != nil {
		return nil, fmt.Errorf("read session: %w", err)
	}
	defer f.Close()
	info, err := f.Stat()
	if err != nil {
		return nil, fmt.Errorf("read session: %w", err)
	}

	// Read backwards a block at a time until the buffer holds enough user
	// messages to name the window, stopping at the start of the smaller of the
	// whole file or the trailing turns.
	var data []byte
	for start := info.Size(); ; {
		readSize := min(int64(tailBlock), start)
		if readSize > 0 {
			start -= readSize
			chunk := make([]byte, readSize)
			if _, err := f.ReadAt(chunk, start); err != nil {
				return nil, fmt.Errorf("read session: %w", err)
			}
			data = append(chunk, data...)
		}
		lines := strings.Split(string(data), "\n")
		first := 0
		if start > 0 {
			// The first buffered line may begin mid-record.
			first = 1
		}
		index, found := windowStart(lines, first, maxTurns)
		if found || start == 0 {
			return parseTail(lines, index)
		}
	}
}

// windowStart returns the index of the maxTurns-th user message counting back
// from the end, scanning no earlier than first. found is false when fewer than
// maxTurns user messages are present in lines[first:].
func windowStart(lines []string, first, maxTurns int) (int, bool) {
	count := 0
	for i := len(lines) - 1; i >= first; i-- {
		var probe struct {
			Message *struct {
				Role Role `json:"role"`
			} `json:"message"`
		}
		if json.Unmarshal([]byte(strings.TrimSpace(lines[i])), &probe) != nil || probe.Message == nil {
			continue
		}
		if probe.Message.Role != RoleUser {
			continue
		}
		count++
		if count == maxTurns {
			return i, true
		}
	}
	return first, false
}

// parseTail decodes entries from lines[start:], skipping the header and any
// blank lines, then returns the active path tail: the leaf's ancestor chain
// within the window, so a branch that skips entries is still followed
// correctly. A single incomplete trailing record (a crash mid-append) is
// dropped rather than failing the whole preview.
func parseTail(lines []string, start int) ([]Entry, error) {
	if start < 1 {
		start = 1
	}
	entries := make([]Entry, 0, len(lines)-start)
	byID := make(map[typedid.EntryID]int, len(lines)-start)
	for i := start; i < len(lines); i++ {
		line := strings.TrimSpace(lines[i])
		if line == "" {
			continue
		}
		var entry Entry
		if err := json.Unmarshal([]byte(line), &entry); err != nil {
			if i == len(lines)-1 {
				break
			}
			return nil, fmt.Errorf("parse session line %d: %w", i+1, err)
		}
		if entry.ID.IsZero() || entry.Type == "" {
			continue
		}
		entry.Raw = append(json.RawMessage(nil), line...)
		byID[entry.ID] = len(entries)
		entries = append(entries, entry)
	}
	if len(entries) == 0 {
		return nil, nil
	}
	// Walk parent links from the last entry. A parent that is absent marks the
	// window's leading edge, which is where the preview starts.
	reverse := make([]Entry, 0, len(entries))
	index := len(entries) - 1
	seen := make(map[typedid.EntryID]bool, len(entries))
	for {
		entry := entries[index]
		if seen[entry.ID] {
			return nil, errors.New("cycle in session parent links")
		}
		seen[entry.ID] = true
		reverse = append(reverse, entry)
		if entry.ParentID == nil {
			break
		}
		parent, ok := byID[*entry.ParentID]
		if !ok {
			break
		}
		index = parent
	}
	for i, j := 0, len(reverse)-1; i < j; i, j = i+1, j-1 {
		reverse[i], reverse[j] = reverse[j], reverse[i]
	}
	return reverse, nil
}

// parsedSession is the mutable-free result of reading and validating a session
// file. Open owns the file afterwards for append; Entries only reads it.
type parsedSession struct {
	header       Header
	entries      []Entry
	byID         map[typedid.EntryID]int
	repairOffset int64 // byte length of the valid prefix, or -1 when the file is intact
}

// leafID is the final entry's ID, which is the active leaf in v1.
func (p parsedSession) leafID() *typedid.EntryID {
	if len(p.entries) == 0 {
		return nil
	}
	leaf := p.entries[len(p.entries)-1].ID
	return &leaf
}

// parseSession reads a session file and validates its header and entries. A
// single incomplete trailing record is reported through repairOffset instead of
// an error, so a caller that owns the file can trim it while a read-only caller
// can ignore it.
func parseSession(path string) (parsedSession, error) {
	b, err := os.ReadFile(path)
	if err != nil {
		return parsedSession{}, fmt.Errorf("read session: %w", err)
	}
	lines := strings.Split(string(b), "\n")
	last := len(lines) - 1
	for last >= 0 && strings.TrimSpace(lines[last]) == "" {
		last--
	}
	if last < 0 {
		return parsedSession{}, errors.New("empty session")
	}
	var header Header
	if err := json.Unmarshal([]byte(lines[0]), &header); err != nil {
		return parsedSession{}, fmt.Errorf("parse session header: %w", err)
	}
	if header.Type != "session" || header.ID.IsZero() {
		return parsedSession{}, errors.New("invalid session header")
	}
	if header.Version != SchemaVersion {
		return parsedSession{}, fmt.Errorf("unsupported session version %d", header.Version)
	}

	result := parsedSession{header: header, byID: make(map[typedid.EntryID]int), repairOffset: -1}
	for i := 1; i <= last; i++ {
		if strings.TrimSpace(lines[i]) == "" {
			continue
		}
		var entry Entry
		if err := json.Unmarshal([]byte(lines[i]), &entry); err != nil {
			if i == last {
				result.repairOffset = int64(len(strings.Join(lines[:i], "\n")) + 1)
				break
			}
			return parsedSession{}, fmt.Errorf("parse session line %d: %w", i+1, err)
		}
		entry.Raw = append(json.RawMessage(nil), lines[i]...)
		if entry.ID.IsZero() || entry.Type == "" {
			return parsedSession{}, fmt.Errorf("invalid session entry on line %d", i+1)
		}
		if err := entry.validate(); err != nil {
			return parsedSession{}, fmt.Errorf("invalid session entry on line %d: %w", i+1, err)
		}
		if _, exists := result.byID[entry.ID]; exists {
			return parsedSession{}, fmt.Errorf("duplicate session entry id %q", entry.ID)
		}
		if entry.ParentID != nil {
			if _, exists := result.byID[*entry.ParentID]; !exists {
				return parsedSession{}, fmt.Errorf("entry %q has missing parent %q", entry.ID, *entry.ParentID)
			}
		}
		result.byID[entry.ID] = len(result.entries)
		result.entries = append(result.entries, entry)
	}
	return result, nil
}

// Summary describes a persisted session without opening it for append.
type Summary struct {
	ID        typedid.SessionID
	Path      string
	CWD       string
	CreatedAt time.Time
	// Title is the first line of the session's first user message, trimmed and
	// length-bounded, so a picker can show what a session is about without
	// opening it.
	Title string
}

// Discover lists every readable session recorded for cwd, newest first. The
// cwd-scoped directory is derived the same way New derives its target, so a
// session is visible here exactly when a future New in cwd would be able to
// resume it. Files that are not valid sessions are skipped.
func Discover(root, cwd string) ([]Summary, error) {
	dir, err := directoryFor(root, cwd)
	if err != nil {
		return nil, err
	}
	dirEntries, err := os.ReadDir(dir)
	if errors.Is(err, os.ErrNotExist) {
		return nil, nil
	}
	if err != nil {
		return nil, fmt.Errorf("list sessions: %w", err)
	}
	names := make([]string, 0, len(dirEntries))
	for _, entry := range dirEntries {
		if !entry.IsDir() && strings.HasSuffix(entry.Name(), fileSuffix) {
			names = append(names, entry.Name())
		}
	}
	// Timestamp-prefixed names sort chronologically; newest first matches how
	// "/resume" presents choices.
	sort.Sort(sort.Reverse(sort.StringSlice(names)))
	summaries := make([]Summary, 0, len(names))
	for _, name := range names {
		summary, err := readSummary(filepath.Join(dir, name))
		if err != nil {
			// A single unreadable file must not hide the rest of the sessions.
			continue
		}
		summaries = append(summaries, summary)
	}
	return summaries, nil
}

// Latest returns the most recently created session for cwd.
func Latest(root, cwd string) (Summary, bool, error) {
	summaries, err := Discover(root, cwd)
	if err != nil {
		return Summary{}, false, err
	}
	if len(summaries) == 0 {
		return Summary{}, false, nil
	}
	return summaries[0], true, nil
}

// Find returns the session for cwd whose ID matches. Only the current working
// directory is searched because session IDs are meaningful only within it.
func Find(root, cwd string, id typedid.SessionID) (Summary, error) {
	summaries, err := Discover(root, cwd)
	if err != nil {
		return Summary{}, err
	}
	for _, summary := range summaries {
		if summary.ID == id {
			return summary, nil
		}
	}
	return Summary{}, fmt.Errorf("session %s not found for this workspace", id)
}

// readSummary parses just enough of a session file to describe it. It reads the
// header directly and scans only the leading lines, stopping once the session is
// known to hold a conversation and its first user message (the title) has been
// seen. The head is where both live — a session's first user turn follows only
// its system prompt and any model changes — so listing never scales with the
// transcript. This runs on every keystroke while completing "/resume".
func readSummary(path string) (Summary, error) {
	f, err := os.Open(path)
	if err != nil {
		return Summary{}, err
	}
	defer f.Close()
	reader := bufio.NewReaderSize(f, 32<<10)

	var header Header
	first := true
	substantive := false
	title := ""
	for {
		line, readErr := reader.ReadString('\n')
		trimmed := strings.TrimSpace(line)
		switch {
		case first:
			first = false
			if err := json.Unmarshal([]byte(trimmed), &header); err != nil {
				return Summary{}, fmt.Errorf("parse session header: %w", err)
			}
			if header.Type != "session" || header.ID.IsZero() || header.Version != SchemaVersion {
				return Summary{}, errors.New("invalid session header")
			}
		case trimmed != "":
			var entry struct {
				Type    EntryType `json:"type"`
				Message *Message  `json:"message"`
			}
			if json.Unmarshal([]byte(trimmed), &entry) == nil {
				if entry.Type == EntryTypeCompaction || (entry.Message != nil && entry.Message.Role != RoleSystem) {
					substantive = true
				}
				if title == "" && entry.Message != nil && entry.Message.Role == RoleUser {
					title = sessionTitle(entry.Message.Content)
				}
			}
		}
		if readErr != nil {
			break
		}
		if substantive && title != "" {
			break
		}
	}
	if !substantive {
		// A session with no conversation is an accidental launch that should have
		// been discarded; ignore any that survived, e.g. a crash before close.
		return Summary{}, errors.New("empty session")
	}
	return Summary{ID: header.ID, Path: path, CWD: header.CWD, CreatedAt: header.Timestamp, Title: title}, nil
}

// titleMaxRunes bounds a session title so a picker row stays one line.
const titleMaxRunes = 60

// sessionTitle is the first non-empty line of a session's first user message,
// collapsed to a single line and length-bounded. Not every session has one.
func sessionTitle(content string) string {
	for _, line := range strings.Split(content, "\n") {
		line = strings.TrimSpace(line)
		if line == "" {
			continue
		}
		runes := []rune(line)
		if len(runes) > titleMaxRunes {
			return strings.TrimSpace(string(runes[:titleMaxRunes])) + "…"
		}
		return line
	}
	return ""
}

// directoryFor resolves the cwd-scoped session directory used by New.
func directoryFor(root, cwd string) (string, error) {
	absCWD, err := filepath.Abs(cwd)
	if err != nil {
		return "", fmt.Errorf("resolve working directory: %w", err)
	}
	absCWD = filepath.Clean(absCWD)
	digest := sha256.Sum256([]byte(absCWD))
	return filepath.Join(root, hex.EncodeToString(digest[:12])), nil
}

func (s *Store) Path() string { return s.path }
func (s *Store) CWD() string  { return s.header.CWD }

// ID is the stable session identifier persisted in the header.
func (s *Store) ID() typedid.SessionID { return s.header.ID }

// Empty reports whether the session still holds only structural entries (the
// root system message and model changes), with no conversation to keep. Such a
// session is deleted when closed.
func (s *Store) Empty() bool {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.empty
}

// CreatedAt is the session's creation time from the header.
func (s *Store) CreatedAt() time.Time { return s.header.Timestamp }

// ActivePath returns the entries from the root to the active leaf in
// conversation order. It is a snapshot used for read-only display.
func (s *Store) ActivePath() []Entry {
	s.mu.Lock()
	defer s.mu.Unlock()
	path, err := s.activePathLocked()
	if err != nil {
		return nil
	}
	return path
}

func (s *Store) Close() error {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.file == nil {
		return nil
	}
	err := s.file.Sync()
	closeErr := s.file.Close()
	s.file = nil
	// A session that never grew past its root system message is an accidental
	// launch: remove it so it does not become the newest resume target.
	if s.empty {
		if removeErr := os.Remove(s.path); removeErr != nil && !errors.Is(removeErr, os.ErrNotExist) {
			err = errors.Join(err, fmt.Errorf("discard empty session: %w", removeErr))
		}
	}
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
	// The root system message is written through this path on creation, and model
	// changes are structural; neither makes the session worth keeping. The first
	// appended conversation or compaction entry does.
	if len(s.entries) > 0 && entry.Type != EntryTypeModelChange {
		s.empty = false
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
//
// The newest compaction summary is projected as a user message immediately
// after the untouched system message, followed by the retained tail and any
// messages appended after the compaction. Keeping the system prompt verbatim
// across compactions is deliberate: provider prompt caches key on a stable
// leading prefix, and folding the summary into the system message would force a
// full cache miss on every compaction.
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
		return repairUnansweredToolCalls(messagesFromEntries(path)), nil
	}
	comp := path[latestCompaction]
	if path[0].Type != EntryTypeMessage || path[0].Message == nil || path[0].Message.Role != RoleSystem {
		return nil, errors.New("session has no root system message")
	}
	out = append(out, ContextMessage{EntryID: path[0].ID, Message: *path[0].Message})

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
	out = append(out, ContextMessage{
		EntryID: comp.ID,
		Message: Message{Role: RoleUser, Content: CompactionSummaryPrefix + comp.Summary + CompactionSummarySuffix},
		Summary: true,
	})
	out = append(out, messagesFromEntries(path[kept:latestCompaction])...)
	out = append(out, messagesFromEntries(path[latestCompaction+1:])...)
	return repairUnansweredToolCalls(out), nil
}

// repairUnansweredToolCalls keeps projected provider context valid when a
// process exited before it could append every tool result. The durable log
// remains append-only; the synthetic result is recreated on each projection.
//
// Results are grouped with the batch they answer, in call order, because wire
// formats require a result for each call in the order the calls were made. The
// common case needs no repair and returns messages as-is.
func repairUnansweredToolCalls(messages []ContextMessage) []ContextMessage {
	// A tool result answers the batch of the assistant message that precedes
	// it. Compatible servers reuse call IDs across turns, so matching globally
	// by ID would let an earlier turn's result answer a later turn's call and
	// mask an incomplete batch.
	owner := groupToolBatches(messages)
	if !repairNeeded(messages, owner) {
		return messages
	}
	return fillUnansweredToolCalls(messages, owner)
}

// groupToolBatches maps each message to the index of the assistant tool-call
// batch it belongs to, or -1 when it belongs to none. Tool results after an
// assistant with calls join that batch; any other message closes it.
func groupToolBatches(messages []ContextMessage) []int {
	owner := make([]int, len(messages))
	batch := -1
	for i := range owner {
		owner[i] = -1
	}
	for i, item := range messages {
		switch item.Message.Role {
		case RoleAssistant:
			if len(item.Message.ToolCalls) == 0 {
				batch = -1
				continue
			}
			batch = i
			owner[i] = i
		case RoleTool:
			owner[i] = batch
		default:
			batch = -1
		}
	}
	return owner
}

func repairNeeded(messages []ContextMessage, owner []int) bool {
	for i, item := range messages {
		if item.Message.Role != RoleAssistant || len(item.Message.ToolCalls) == 0 {
			continue
		}
		answered := make(map[typedid.ToolCallID]bool)
		for j := i + 1; j < len(messages) && owner[j] == i; j++ {
			answered[messages[j].Message.ToolCallID] = true
		}
		for _, call := range item.Message.ToolCalls {
			if !answered[call.ID] {
				return true
			}
		}
	}
	return false
}

// fillUnansweredToolCalls rebuilds each batch a cancelled or crashed turn left
// open, placing a result for every call in call order.
func fillUnansweredToolCalls(messages []ContextMessage, owner []int) []ContextMessage {
	results := make(map[int]map[typedid.ToolCallID]ContextMessage)
	for i, item := range messages {
		if item.Message.Role != RoleTool || owner[i] < 0 {
			continue
		}
		batch := owner[i]
		if results[batch] == nil {
			results[batch] = make(map[typedid.ToolCallID]ContextMessage)
		}
		results[batch][item.Message.ToolCallID] = item
	}
	// emitted records the results already placed with their batch, so an orphan
	// result — one whose ID matches no call in its batch — can still be emitted
	// in its original position rather than dropped.
	emitted := make(map[int]map[typedid.ToolCallID]bool)
	var out []ContextMessage
	for i, item := range messages {
		switch item.Message.Role {
		case RoleTool:
			if owner[i] < 0 || !emitted[owner[i]][item.Message.ToolCallID] {
				out = append(out, item)
			}
		case RoleAssistant:
			out = append(out, item)
			for _, call := range item.Message.ToolCalls {
				if emitted[i] == nil {
					emitted[i] = make(map[typedid.ToolCallID]bool)
				}
				emitted[i][call.ID] = true
				if result, ok := results[i][call.ID]; ok {
					out = append(out, result)
					continue
				}
				out = append(out, ContextMessage{Message: Message{
					Role: RoleTool, Content: InterruptedToolResult, IsError: true,
					ToolCallID: call.ID, Name: call.Function.Name,
				}})
			}
		default:
			out = append(out, item)
		}
	}
	return out
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
