package migrations

import (
	"encoding/json"
	"errors"
	"fmt"
	"os"
)

// readConfigObject decodes the config at path as its top-level fields, left
// raw. Steps edit only the fields they own and write the rest back unchanged,
// so they depend on neither the current config.Config nor its defaults. It
// reports false for a missing config, or one that is not a JSON object, so the
// step leaves it for loading to report.
func readConfigObject(path string) (map[string]json.RawMessage, bool, error) {
	b, err := os.ReadFile(path)
	if errors.Is(err, os.ErrNotExist) {
		return nil, false, nil
	}
	if err != nil {
		return nil, false, fmt.Errorf("read config for migration: %w", err)
	}
	var fields map[string]json.RawMessage
	if json.Unmarshal(b, &fields) != nil || fields == nil {
		return nil, false, nil
	}
	return fields, true, nil
}
