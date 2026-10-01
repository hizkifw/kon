// Package core is the reusable half of kon: the agent loop, compaction,
// provider wire backends with retries, the durable session tree, and the tool
// contract. A program supplies its own prompt and tools and drives a
// core/agent Runner; the kon CLI is one such program.
//
// Nothing under core imports kon's internal packages, so core can be used
// from another module.
package core
