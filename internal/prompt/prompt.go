// Package prompt assembles kon's system prompt. The agent persists it once as
// a session's root message and never rebuilds it.
package prompt

import (
	_ "embed"
	"path/filepath"
	"strings"

	"github.com/hizkifw/kon/internal/contextfiles"
)

//go:embed prompt.txt
var basePrompt string

// System builds the durable system prompt persisted as the session's root
// message. It deliberately does not name the model: the prompt is never
// rebuilt, so a later /model switch would leave the name stale, and telling
// the model through a later message risks one that distrusts a user speaking
// as the system. It names kon's commands as plain `kon`, which the shell tool's
// PATH resolves to this kon. contextFiles are AGENTS.md-style instructions,
// the user's global file first and then the project's ordered outermost to
// innermost; they precede the cwd so a project can describe conventions
// before the model sees where it is working. The result is byte-stable for a
// given input, which is what keeps the provider prompt cache valid across
// compactions.
func System(cwd string, contextFiles []contextfiles.File) string {
	prompt := strings.TrimSuffix(basePrompt, "\n")
	prompt += renderContextFiles(contextFiles)
	prompt += "\nCurrent working directory: " + filepath.Clean(cwd)
	return prompt
}

// renderContextFiles formats discovered instruction files as tagged blocks. The
// path attribute lets the model attribute an instruction to its file when it
// reports or applies it.
func renderContextFiles(files []contextfiles.File) string {
	if len(files) == 0 {
		return ""
	}
	var out strings.Builder
	out.WriteString("\n\nUser and project instructions:")
	for _, file := range files {
		out.WriteString("\n\n<instructions path=\"")
		out.WriteString(file.Path)
		out.WriteString("\">\n")
		out.WriteString(strings.TrimSpace(file.Content))
		out.WriteString("\n</instructions>")
	}
	out.WriteString("\n")
	return out.String()
}
