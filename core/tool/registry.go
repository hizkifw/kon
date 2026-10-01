package tool

import (
	"github.com/hizkifw/kon/core/session"
)

// Registry is a catalog of tools. Tools are registered once at startup; the
// registry owns name lookup and the model-facing definition list.
type Registry struct {
	ordered []Tool
	byName  map[string]Tool
}

// NewRegistry returns a registry holding tools, in that order.
func NewRegistry(tools ...Tool) *Registry {
	r := &Registry{byName: map[string]Tool{}}
	for _, tool := range tools {
		r.Register(tool)
	}
	return r
}

// Register adds a tool. It panics on a nil tool or an empty or duplicate name
// because the registry is assembled once at startup and either is a
// programming error.
func (r *Registry) Register(tool Tool) {
	if tool == nil {
		panic("tool: nil tool")
	}
	definition := tool.Definition()
	if definition.Name == "" {
		panic("tool: tool with empty name")
	}
	if _, exists := r.byName[definition.Name]; exists {
		panic("tool: duplicate tool: " + definition.Name)
	}
	r.byName[definition.Name] = tool
	r.ordered = append(r.ordered, tool)
}

// Lookup returns the tool registered under name.
func (r *Registry) Lookup(name string) (Tool, bool) {
	tool, ok := r.byName[name]
	return tool, ok
}

// Definitions returns the model-facing schema of every registered tool, in
// registration order.
func (r *Registry) Definitions() []session.ToolDefinition {
	definitions := make([]session.ToolDefinition, 0, len(r.ordered))
	for _, tool := range r.ordered {
		definitions = append(definitions, tool.Definition())
	}
	return definitions
}

// InterruptAll escalates cancellation across every registered Interrupter.
// attempt is forwarded as-is; see Interrupter. It reports whether any tool had
// something to escalate against.
func (r *Registry) InterruptAll(attempt int) bool {
	interrupted := false
	for _, tool := range r.ordered {
		if i, ok := tool.(Interrupter); ok && i.Interrupt(attempt) {
			interrupted = true
		}
	}
	return interrupted
}
