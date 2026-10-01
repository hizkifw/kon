package agent

import (
	"testing"

	"github.com/hizkifw/kon/core/tokens"
)

func TestCompactionSizesFollowTheWindow(t *testing.T) {
	for _, c := range []struct {
		name                     string
		limits                   Limits
		threshold, keep, summary tokens.Count
	}{
		// A small window scales the room for a reply and a summary down with
		// it, so compaction still starts well before the window is full.
		{"32K local model", Limits{ContextWindow: 32_768}, 24_576, 4_587, 4_096},
		// An output limit caps both, so the summary request is not refused
		// for asking more than the model writes.
		{"128K, 8K output", Limits{ContextWindow: 128_000, OutputLimit: 8_192}, 102_400, 19_169, 8_192},
		{"200K", Limits{ContextWindow: 200_000, OutputLimit: 64_000}, 150_000, 28_000, 25_000},
		// A large window compacts at 80%, where models still work well, and
		// keeps far more verbatim.
		{"1M", Limits{ContextWindow: 1_000_000, OutputLimit: 128_000}, 800_000, 154_880, 32_000},
		// An unknown output limit holds the summary to a size every model
		// accepts; an unknown window keeps a fixed amount.
		{"200K, output unknown", Limits{ContextWindow: 200_000}, 160_000, 28_000, 8_192},
		{"window unknown", Limits{}, 0, 20_000, 8_192},
		// Configured budgets win, and a reserve holds the summary to half of
		// itself.
		{"configured", Limits{ContextWindow: 200_000, OutputLimit: 64_000, ReserveTokens: 40_000, KeepRecentTokens: 30_000}, 160_000, 30_000, 20_000},
	} {
		t.Run(c.name, func(t *testing.T) {
			if c.limits.ContextWindow > 0 {
				if got := c.limits.threshold(); got != c.threshold {
					t.Errorf("threshold %d, want %d", got, c.threshold)
				}
			}
			if got := c.limits.keepRecent(); got != c.keep {
				t.Errorf("keep %d, want %d", got, c.keep)
			}
			if got := c.limits.summaryBudget(); got != c.summary {
				t.Errorf("summary budget %d, want %d", got, c.summary)
			}
		})
	}
}
