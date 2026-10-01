package migrations

import (
	"context"
	"encoding/json"

	"kon.kitsu.red/internal/config"
)

type modelInputsV6 struct{}

func (modelInputsV6) Version() int { return 6 }
func (modelInputsV6) Name() string { return "replace model vision with inputs" }

// Run replaces each model's "vision" flag with the "inputs" list that
// generalizes it: true becomes ["image"], and false is dropped. A model that
// already lists inputs keeps them. A config kon cannot read is left for
// loading to report.
func (modelInputsV6) Run(_ context.Context, paths config.Paths) error {
	cfg, ok, err := readConfigObject(paths.ConfigFile)
	if !ok {
		return err
	}
	var models []map[string]json.RawMessage
	if json.Unmarshal(cfg["models"], &models) != nil {
		return nil
	}
	changed := false
	for _, model := range models {
		raw, present := model["vision"]
		if !present {
			continue
		}
		var vision bool
		if json.Unmarshal(raw, &vision) != nil {
			continue
		}
		if _, listed := model["inputs"]; vision && !listed {
			model["inputs"] = mustJSON([]string{"image"})
		}
		delete(model, "vision")
		changed = true
	}
	if !changed {
		return nil
	}
	cfg["models"] = mustJSON(models)
	return config.WriteJSON(paths.ConfigFile, cfg)
}
