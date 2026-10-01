// Package codetools contains kon's deliberately small coding tools. Each one
// implements tool.Tool, and Registry assembles them for a session.
package codetools

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io"
)

// decodeArgs decodes the one JSON object the model sent into dst, rejecting
// unknown fields and trailing garbage so schema drift fails loudly instead of
// silently ignoring a mistyped argument.
func decodeArgs(raw json.RawMessage, dst any) error {
	dec := json.NewDecoder(bytes.NewReader(raw))
	dec.DisallowUnknownFields()
	if err := dec.Decode(dst); err != nil {
		return fmt.Errorf("invalid arguments: %w", err)
	}
	var extra any
	if err := dec.Decode(&extra); !errors.Is(err, io.EOF) {
		if err == nil {
			return errors.New("invalid arguments: multiple JSON values")
		}
		return fmt.Errorf("invalid arguments: %w", err)
	}
	return nil
}
