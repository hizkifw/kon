// Package migrations contains the concrete, ordered storage upgrades.
package migrations

import "kon.kitsu.red/internal/migrate"

// Ordered is the one place that defines migration order. New steps append here
// and use the next version number; existing steps must not be reordered.
func Ordered() []migrate.Step {
	return []migrate.Step{
		baselineV1{},
		v011SessionsV2{},
		compactionDefaultsV3{},
		instructionsFileV4{},
	}
}

var (
	_ migrate.Step = baselineV1{}
	_ migrate.Step = v011SessionsV2{}
	_ migrate.Step = compactionDefaultsV3{}
	_ migrate.Step = instructionsFileV4{}
)
