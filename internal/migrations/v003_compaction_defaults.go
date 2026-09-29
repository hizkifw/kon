package migrations

import (
	"context"

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
	cfg, ok, err := readInstructionsConfig(paths.ConfigFile)
	if !ok {
		return err
	}
	written := cfg.Compaction
	if cfg.Compaction.ReserveTokens == writtenReserveTokens {
		cfg.Compaction.ReserveTokens = 0
	}
	if cfg.Compaction.KeepRecentTokens == writtenKeepRecentTokens {
		cfg.Compaction.KeepRecentTokens = 0
	}
	if cfg.Compaction == written {
		return nil
	}
	// The instructions field is kept for the next step to move.
	return config.WriteJSON(paths.ConfigFile, cfg)
}
