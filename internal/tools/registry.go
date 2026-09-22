package tools

import (
	"github.com/hizkifw/kon/internal/provider"
)

// Registry is the central catalog of tools, the counterpart of the
// slash-command registry in internal/ui. Tools are registered once at startup;
// the registry owns name lookup and the model-facing definition list.
type Registry struct {
	ordered []Tool
	byName  map[string]Tool
}

// NewRegistry returns an empty tool registry.
func NewRegistry() *Registry {
	return &Registry{byName: map[string]Tool{}}
}

// Register adds a tool. It panics on a nil tool or an empty or duplicate name
// because the registry is assembled once at startup and either is a
// programming error.
func (r *Registry) Register(tool Tool) {
	if tool == nil {
		panic("tools: nil tool")
	}
	definition := tool.Definition()
	if definition.Name == "" {
		panic("tools: tool with empty name")
	}
	if _, exists := r.byName[definition.Name]; exists {
		panic("tools: duplicate tool: " + definition.Name)
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
func (r *Registry) Definitions() []provider.Tool {
	definitions := make([]provider.Tool, 0, len(r.ordered))
	for _, tool := range r.ordered {
		definitions = append(definitions, tool.Definition())
	}
	return definitions
}

// InterruptAll escalates cancellation across every registered tool. attempt is
// the number of consecutive Ctrl+C presses; it is forwarded as-is so tools can
// distinguish the polite first press from the harder repeat. It reports
// whether any tool had something to escalate against.
func (r *Registry) InterruptAll(attempt int) bool {
	interrupted := false
	for _, tool := range r.ordered {
		if tool.Interrupt(attempt) {
			interrupted = true
		}
	}
	return interrupted
}

// defaultRegistry registers kon's built-in tools. Adding a tool means adding
// a file that implements Tool and one registration line here.
func defaultRegistry() *Registry {
	registry := NewRegistry()
	registry.Register(readTool{})
	registry.Register(writeTool{})
	registry.Register(editTool{})
	registry.Register(&shellTool{})
	return registry
}
