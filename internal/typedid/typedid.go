// Package typedid defines identifiers that cannot be accidentally mixed.
//
// IDs owned by kon are cryptographically random, prefix-qualified base62 values.
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

func NewSessionID() (SessionID, error) {
	value, err := generate("ses")
	return SessionID{value: value}, err
}

func ParseSessionID(value string) (SessionID, error) {
	if err := validate(value, "ses"); err != nil {
		return SessionID{}, fmt.Errorf("invalid session ID: %w", err)
	}
	return SessionID{value: value}, nil
}

func (id SessionID) String() string { return id.value }
func (id SessionID) IsZero() bool   { return id.value == "" }

func (id SessionID) MarshalJSON() ([]byte, error) {
	if id.IsZero() {
		return nil, fmt.Errorf("marshal zero session ID")
	}
	return json.Marshal(id.value)
}

func (id *SessionID) UnmarshalJSON(data []byte) error {
	var value string
	if err := json.Unmarshal(data, &value); err != nil {
		return err
	}
	parsed, err := ParseSessionID(value)
	if err != nil {
		return err
	}
	*id = parsed
	return nil
}

// EntryID has the serialized form ent_<20 base62 characters>.
type EntryID struct{ value string }

func NewEntryID() (EntryID, error) {
	value, err := generate("ent")
	return EntryID{value: value}, err
}

func ParseEntryID(value string) (EntryID, error) {
	if err := validate(value, "ent"); err != nil {
		return EntryID{}, fmt.Errorf("invalid entry ID: %w", err)
	}
	return EntryID{value: value}, nil
}

func (id EntryID) String() string { return id.value }
func (id EntryID) IsZero() bool   { return id.value == "" }

func (id EntryID) MarshalJSON() ([]byte, error) {
	if id.IsZero() {
		return nil, fmt.Errorf("marshal zero entry ID")
	}
	return json.Marshal(id.value)
}

func (id *EntryID) UnmarshalJSON(data []byte) error {
	var value string
	if err := json.Unmarshal(data, &value); err != nil {
		return err
	}
	parsed, err := ParseEntryID(value)
	if err != nil {
		return err
	}
	*id = parsed
	return nil
}

// ToolCallID is controlled by an external model provider. No local format
// constraints are applied; the type exists to prevent mixing it with kon IDs.
type ToolCallID string

func ExternalToolCallID(value string) ToolCallID { return ToolCallID(value) }
func (id ToolCallID) String() string             { return string(id) }

// ModelID is controlled by the configured provider and is intentionally opaque.
type ModelID string

func ExternalModelID(value string) ModelID { return ModelID(value) }
func (id ModelID) String() string          { return string(id) }

func generate(prefix string) (string, error) {
	random := make([]byte, randomLength)
	// Reject bytes outside the largest multiple of 62 below 256 to avoid modulo bias.
	const ceiling = byte(248)
	for i := range random {
		for {
			var candidate [1]byte
			if _, err := rand.Read(candidate[:]); err != nil {
				return "", fmt.Errorf("generate %s ID: %w", prefix, err)
			}
			if candidate[0] < ceiling {
				random[i] = base62[int(candidate[0])%len(base62)]
				break
			}
		}
	}
	return prefix + "_" + string(random), nil
}

func validate(value, prefix string) error {
	wantPrefix := prefix + "_"
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
