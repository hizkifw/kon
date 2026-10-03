// Package prompt assembles kon's system prompt. The agent persists it once as
// a session's root message and never rebuilds it.
package prompt

import (
	_ "embed"
	"path/filepath"
	"strings"

	"kon.kitsu.red/internal/contextfiles"
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
// compactions. base replaces kon's built-in instructions when it is not
// empty; the context files and cwd follow either one. webSearch reports
// whether a search provider is configured, so the built-in instructions name
// `kon tool websearch` only when it would work.
func System(base, cwd string, contextFiles []contextfiles.File, webSearch bool) string {
	if base == "" {
		base = strings.TrimSuffix(basePrompt, "\n") + "\n\n" + shellTools(webSearch)
	}
	prompt := strings.TrimSuffix(base, "\n")
	prompt += renderContextFiles(contextFiles)
	prompt += "\nCurrent working directory: " + filepath.Clean(cwd)
	return prompt
}

// shellTools is the paragraph naming the tools the model runs as `kon tool
// <name>`. Each costs a clause here instead of a schema in every request.
func shellTools(webSearch bool) string {
	if !webSearch {
		return "More tools run through kon: `kon tool webfetch <url>` prints a web page as\n" +
			"Markdown. Run `kon tool --help` for their usage."
	}
	return "More tools run through kon: `kon tool websearch <query>` searches the web,\n" +
		"and `kon tool webfetch <url>` prints a web page as Markdown. Run\n" +
		"`kon tool --help` for their usage."
}

// renderContextFiles formats instruction files as tagged blocks. The path
// attribute lets the model attribute an instruction to its file when it
// reports or applies it; instructions given inline have no file, so their
// block has no path.
func renderContextFiles(files []contextfiles.File) string {
	if len(files) == 0 {
		return ""
	}
	var out strings.Builder
	out.WriteString("\n\nUser and project instructions:")
	for _, file := range files {
		if file.Path == "" {
			out.WriteString("\n\n<instructions>\n")
		} else {
			out.WriteString("\n\n<instructions path=\"")
			out.WriteString(file.Path)
			out.WriteString("\">\n")
		}
		out.WriteString(strings.TrimSpace(file.Content))
		out.WriteString("\n</instructions>")
	}
	out.WriteString("\n")
	return out.String()
}
