// Package typedid defines identifiers that cannot be accidentally mixed.
//
// IDs this package generates are cryptographically random, prefix-qualified base62 values.
// Provider-owned IDs are lightweight named strings because their formats belong
// to the provider and must round-trip without local validation.
package typedid

import (
	"crypto/rand"
	"encoding/json"
	"fmt"
	"strings"
)

const (
	randomLength = 20
	base62       = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
)

// SessionID has the serialized form ses_<20 base62 characters>.
type SessionID struct{ value string }

// NewSessionID makes a random session ID.
func NewSessionID() (SessionID, error) {
	value, err := sessionKind.generate()
	return SessionID{value: value}, err
}

// ParseSessionID reads a session ID, rejecting any other shape.
func ParseSessionID(value string) (SessionID, error) {
	value, err := sessionKind.parse(value)
	return SessionID{value: value}, err
}

func (id SessionID) String() string                   { return id.value }
func (id SessionID) IsZero() bool                     { return id.value == "" }
func (id SessionID) MarshalJSON() ([]byte, error)     { return sessionKind.marshal(id.value) }
func (id *SessionID) UnmarshalJSON(data []byte) error { return sessionKind.unmarshal(data, &id.value) }

// EntryID has the serialized form ent_<20 base62 characters>.
type EntryID struct{ value string }

// NewEntryID makes a random entry ID.
func NewEntryID() (EntryID, error) {
	value, err := entryKind.generate()
	return EntryID{value: value}, err
}

// ParseEntryID reads an entry ID, rejecting any other shape.
func ParseEntryID(value string) (EntryID, error) {
	value, err := entryKind.parse(value)
	return EntryID{value: value}, err
}

func (id EntryID) String() string                   { return id.value }
func (id EntryID) IsZero() bool                     { return id.value == "" }
func (id EntryID) MarshalJSON() ([]byte, error)     { return entryKind.marshal(id.value) }
func (id *EntryID) UnmarshalJSON(data []byte) error { return entryKind.unmarshal(data, &id.value) }

// ToolCallID is controlled by an external model provider. No local format
// constraints are applied; the type exists to prevent mixing it with generated IDs.
type ToolCallID string

// ExternalToolCallID wraps a provider's tool call ID without checking it.
func ExternalToolCallID(value string) ToolCallID { return ToolCallID(value) }
func (id ToolCallID) String() string             { return string(id) }

// ModelID is controlled by the configured provider and is intentionally opaque.
type ModelID string

// ExternalModelID wraps a provider's model ID without checking it.
func ExternalModelID(value string) ModelID { return ModelID(value) }
func (id ModelID) String() string          { return string(id) }

// kind is what sets one kon-owned ID apart from another: the prefix it is
// serialized with and the noun its errors use. Each ID type delegates to its
// kind, so generation and validation are written once.
type kind struct{ prefix, noun string }

var (
	sessionKind = kind{prefix: "ses", noun: "session"}
	entryKind   = kind{prefix: "ent", noun: "entry"}
)

func (k kind) generate() (string, error) {
	random := make([]byte, randomLength)
	// Reject bytes outside the largest multiple of 62 below 256 to avoid modulo bias.
	const ceiling = byte(248)
	for i := range random {
		for {
			var candidate [1]byte
			if _, err := rand.Read(candidate[:]); err != nil {
				return "", fmt.Errorf("generate %s ID: %w", k.noun, err)
			}
			if candidate[0] < ceiling {
				random[i] = base62[int(candidate[0])%len(base62)]
				break
			}
		}
	}
	return k.prefix + "_" + string(random), nil
}

// parse returns value unchanged if it has this kind's shape, and "" otherwise,
// so a failed parse always yields the zero ID.
func (k kind) parse(value string) (string, error) {
	if err := k.validate(value); err != nil {
		return "", fmt.Errorf("invalid %s ID: %w", k.noun, err)
	}
	return value, nil
}

func (k kind) validate(value string) error {
	wantPrefix := k.prefix + "_"
	if !strings.HasPrefix(value, wantPrefix) {
		return fmt.Errorf("must start with %q", wantPrefix)
	}
	random := strings.TrimPrefix(value, wantPrefix)
	if len(random) != randomLength {
		return fmt.Errorf("must have %d base62 characters after the prefix", randomLength)
	}
	for _, char := range random {
		if !strings.ContainsRune(base62, char) {
			return fmt.Errorf("contains non-base62 character %q", char)
		}
	}
	return nil
}

// marshal refuses the zero ID, which would otherwise be written as "" and fail
// to parse when the session is read back.
func (k kind) marshal(value string) ([]byte, error) {
	if value == "" {
		return nil, fmt.Errorf("marshal zero %s ID", k.noun)
	}
	return json.Marshal(value)
}

// unmarshal sets *dst only when data holds a valid ID, leaving it untouched on
// error.
func (k kind) unmarshal(data []byte, dst *string) error {
	var value string
	if err := json.Unmarshal(data, &value); err != nil {
		return err
	}
	parsed, err := k.parse(value)
	if err != nil {
		return err
	}
	*dst = parsed
	return nil
}
