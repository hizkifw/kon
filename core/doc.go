// Package core is a library for building coding agents and other tool-using
// programs on large language models. It runs the model/tool loop, talks to
// providers over their native wire formats with retries, compacts long
// conversations, and keeps every conversation as an append-only tree on disk
// or in memory. Your program brings the rest: a system prompt, a model, and
// whatever tools it wants the model to have.
//
// The kon terminal agent is built on core and uses no private hooks into it:
// kon's prompt and coding tools are supplied exactly as yours would be. A
// chat bot, a CI reviewer, or a different agent altogether can drive core the
// same way.
//
// # Packages
//
//   - [kon.kitsu.red/core/agent] runs the loop: it sends the conversation to a
//     model, executes the tool calls that come back, reports retries as
//     events, and compacts the history before it outgrows the context window.
//   - [kon.kitsu.red/core/tool] is the contract a tool implements, and the
//     registry and executor that hand tools to the loop.
//   - [kon.kitsu.red/core/provider] connects to a model and retries transient
//     failures. Each backend maps the neutral conversation onto one wire
//     format; [kon.kitsu.red/core/provider/wire] lists the formats and what
//     each implies.
//   - [kon.kitsu.red/core/session] stores conversations: a file-backed or
//     in-memory, append-only tree of parent-linked entries, so history is
//     added to and never rewritten. The loop needs only the
//     [kon.kitsu.red/core/agent.Store] interface, so a program can keep
//     conversations anywhere.
//   - [kon.kitsu.red/core/typedid] and [kon.kitsu.red/core/tokens] are the
//     identifier and token-count types the others share.
//
// # Getting started
//
// A whole program is a store, a provider, a registry of tools, and a call to
// Run. The package example on [kon.kitsu.red/core/agent] builds a complete bot
// with one tool.
//
// The session root holds the system prompt, written once when the store is
// created and never rewritten, so provider prompt caches stay warm across
// turns and compactions.
//
// Nothing under core imports kon's other packages, and a test enforces it, so
// adding core to a module brings no terminal or kon-specific code with it.
package core
