package codetools

import (
	"github.com/hizkifw/kon/core/tool"
)

// Registry returns kon's built-in tools. jobs runs background shell commands
// and may be nil where there is no session to keep them in. Adding a tool
// means adding a file that implements tool.Tool and one line here.
func Registry(jobs *Jobs) *tool.Registry {
	return tool.NewRegistry(readTool{}, writeTool{}, editTool{}, &shellTool{jobs: jobs})
}

// defaultDisplays resolves tool calls to their owned display. Displays are
// pure functions of a call, so a registry without jobs serves every session.
var defaultDisplays = Registry(nil)
