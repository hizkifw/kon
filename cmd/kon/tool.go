package main

import (
	"context"
	"fmt"
	"os"
	"strings"

	"kon.kitsu.red/internal/web"
)

// toolCommands are the tools the agent runs from its shell as `kon tool
// <name>`. The system prompt names them and points at `kon tool --help`, so a
// new one adds a line there instead of a schema to every model request.
func toolCommands() []command {
	return []command{webfetchCommand()}
}

func toolCommand() command {
	var list strings.Builder
	list.WriteString("tools:\n")
	for _, tool := range toolCommands() {
		fmt.Fprintf(&list, "  %-10s %s\n", tool.name, tool.summary)
	}
	list.WriteString("\nRun \"kon tool <name> --help\" for a tool's usage.")
	return command{
		name:     "tool",
		summary:  "run one of the tools the agent uses from its shell",
		synopsis: "kon tool <name> [args...]",
		detail:   list.String(),
		run:      runTool,
	}
}

// runTool dispatches to the named tool. With no name it prints the tool list,
// which is what a model exploring the command wants to see.
func runTool(args []string) error {
	if len(args) == 0 {
		fmt.Println(toolCommand().help())
		return nil
	}
	tool, ok := lookup(toolCommands(), args[0])
	if !ok {
		return fmt.Errorf("unknown tool %q (try kon tool --help)", args[0])
	}
	if wantsHelp(args[1:]) {
		fmt.Println(tool.help())
		return nil
	}
	return tool.run(args[1:])
}

func webfetchCommand() command {
	return command{
		name:     "webfetch",
		summary:  "print a web page as Markdown",
		synopsis: "kon tool webfetch <url>",
		detail: "Fetch a URL and print it as text: an HTML page as Markdown, without its\n" +
			"scripts, buttons, and decoration, and any other text as it arrived. Links\n" +
			"within the page's site print as paths from its root, and others as full\n" +
			"URLs. When the server redirects, the final URL is noted on stderr; paths\n" +
			"start from its site. A URL without a scheme is fetched over https. At most\n" +
			"the first 5 MiB is read, and a response that is not text is an error.\n\n" +
			"For a long page, redirect the output to a file and read it in parts.",
		run: runWebfetch,
	}
}

func runWebfetch(args []string) error {
	if len(args) != 1 {
		return fmt.Errorf("usage: kon tool webfetch <url>")
	}
	page, err := web.Fetch(context.Background(), args[0])
	if err != nil {
		return err
	}
	if _, err := os.Stdout.WriteString(page.Text); err != nil {
		return err
	}
	if page.RedirectedTo != "" {
		fmt.Fprintf(os.Stderr, "kon: redirected to %s\n", page.RedirectedTo)
	}
	if page.Truncated {
		fmt.Fprintf(os.Stderr, "kon: page truncated to its first %d MiB\n", web.MaxBodyBytes>>20)
	}
	return nil
}
