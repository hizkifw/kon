package migrations

import (
	"context"
	"encoding/json"

	"github.com/hizkifw/kon/internal/config"
)

// Every config kon wrote before compaction was sized from the model's window
// held these budgets, which kon had filled in rather than the user chosen.
const (
	writtenReserveTokens    = 16_384
	writtenKeepRecentTokens = 20_000
)

type compactionDefaultsV3 struct{}

func (compactionDefaultsV3) Version() int { return 3 }
func (compactionDefaultsV3) Name() string { return "drop the compaction budgets kon filled in" }

// Run drops each budget that still holds the value kon wrote, so the budget is
// derived from the model's window instead. A budget the user changed stays. A
// config kon cannot read is left for loading to report.
func (compactionDefaultsV3) Run(_ context.Context, paths config.Paths) error {
	cfg, ok, err := readConfigObject(paths.ConfigFile)
	if !ok {
		return err
	}
	var compaction map[string]json.RawMessage
	if json.Unmarshal(cfg["compaction"], &compaction) != nil || compaction == nil {
		return nil
	}
	changed := false
	for field, written := range map[string]int64{
		"reserve_tokens":     writtenReserveTokens,
		"keep_recent_tokens": writtenKeepRecentTokens,
	} {
		var value int64
		if json.Unmarshal(compaction[field], &value) == nil && value == written {
			delete(compaction, field)
			changed = true
		}
	}
	if !changed {
		return nil
	}
	if len(compaction) == 0 {
		delete(cfg, "compaction")
	} else {
		cfg["compaction"] = mustJSON(compaction)
	}
	// The instructions field is kept for the next step to move.
	return config.WriteJSON(paths.ConfigFile, cfg)
}
