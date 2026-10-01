// Package session persists an append-only, parent-linked conversation tree.
package session

import (
	"encoding/json"
	"errors"
	"time"

	"github.com/hizkifw/kon/core/tokens"
	"github.com/hizkifw/kon/core/typedid"
)

const SchemaVersion = 4

type EntryType string

const (
	EntryTypeMessage     EntryType = "message"
	EntryTypeCompaction  EntryType = "compaction"
	EntryTypeModelChange EntryType = "model_change"
	// A turn is bracketed by a start entry, written before its first user
	// message, and an end entry carrying the duration the runner measured. The
	// pair lets a replay show a turn's total without inferring boundaries from
	// timestamps, and a start with no end marks a turn the process never
	// finished. Neither enters model context.
	EntryTypeTurnStart EntryType = "turn_start"
	EntryTypeTurnEnd   EntryType = "turn_end"
)

type Header struct {
	Type       string            `json:"type"`
	Version    int               `json:"version"`
	ID         typedid.SessionID `json:"id"`
	AppVersion string            `json:"app_version"`
	Timestamp  time.Time         `json:"timestamp"`
	CWD        string            `json:"cwd"`
	// Parent is the session whose agent started this one as a subagent, with
	// kon run from its shell. It is optional, so older readers ignore it.
	Parent typedid.SessionID `json:"parent_session_id,omitzero"`
}

type Entry struct {
	Type                  EntryType        `json:"type"`
	ID                    typedid.EntryID  `json:"id"`
	ParentID              *typedid.EntryID `json:"parent_id"`
	Timestamp             time.Time        `json:"timestamp"`
	Message               *Message         `json:"message,omitempty"`
	Summary               string           `json:"summary,omitempty"`
	FirstKeptEntryID      *typedid.EntryID `json:"first_kept_entry_id,omitempty"`
	TokensBefore          tokens.Count     `json:"tokens_before,omitempty"`
	TokensBeforeEstimated bool             `json:"tokens_before_estimated,omitempty"`
	Usage                 *Usage           `json:"usage,omitempty"`
	Model                 *ModelSelection  `json:"model,omitempty"`
	DurationMS            int64            `json:"duration_ms,omitempty"`
	Raw                   json.RawMessage  `json:"-"`
}

type ModelSelection struct {
	Name         string          `json:"name"`
	WireFormat   string          `json:"wire_format"`
	ConnectionID string          `json:"connection_id,omitempty"`
	ExternalID   typedid.ModelID `json:"external_id"`
}

// TurnDuration is a turn end entry's recorded duration.
func (entry Entry) TurnDuration() time.Duration {
	return time.Duration(entry.DurationMS) * time.Millisecond
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
		if entry.Model == nil || entry.Model.Name == "" || entry.Model.WireFormat == "" || entry.Model.ExternalID.String() == "" {
			return errors.New("model change requires name, wire format, and external ID")
		}
	case EntryTypeTurnEnd:
		if entry.DurationMS < 0 {
			return errors.New("turn end requires a non-negative duration")
		}
	}
	// Unknown types retain their envelope for forward-compatible readers.
	return nil
}
