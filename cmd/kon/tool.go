package main

import (
	"context"
	"errors"
	"flag"
	"fmt"
	"io"
	"io/fs"
	"os"
	"strings"

	"kon.kitsu.red/internal/config"
	"kon.kitsu.red/internal/web"
	"kon.kitsu.red/internal/websearch"
	"kon.kitsu.red/internal/websearch/engines"
)

// toolCommands are the tools the agent runs from its shell as `kon tool
// <name>`. The system prompt names them and points at `kon tool --help`, so a
// new one adds a line there instead of a schema to every model request.
func toolCommands() []command {
	return []command{webfetchCommand(), websearchCommand()}
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

func websearchCommand() command {
	return command{
		name:     "websearch",
		summary:  "search the web",
		synopsis: "kon tool websearch [-n <count>] <query>",
		detail: "Search the web with the provider set as web_search.provider in kon's\n" +
			"config, and print each result's title, URL, and a snippet. The query is\n" +
			"every argument joined by spaces, so it needs no quotes. Without a provider\n" +
			"configured, web search is off and this is an error.\n\n" +
			fmt.Sprintf("  -n <count>   how many results to print, up to %d (default %d)\n\n", websearch.MaxCount, websearch.DefaultCount) +
			"Read a result's page with \"kon tool webfetch <url>\".",
		run: runWebsearch,
	}
}

func runWebsearch(args []string) error {
	flags := flag.NewFlagSet("kon tool websearch", flag.ContinueOnError)
	// Help and errors are rendered by the dispatch layer; keep flag from
	// printing its own usage and duplicating the message.
	flags.SetOutput(io.Discard)
	flags.Usage = func() {}
	count := flags.Int("n", websearch.DefaultCount, "how many results to print")
	if err := flags.Parse(args); err != nil {
		return err
	}
	if flags.NArg() == 0 {
		return fmt.Errorf("usage: kon tool websearch [-n <count>] <query>")
	}
	paths, err := config.ResolvePaths()
	if err != nil {
		return err
	}
	// The config is only read: a kon session has already brought storage up
	// to date, and waiting on the upgrade lock would slow every search.
	cfg, err := config.Load(paths.ConfigFile)
	if err != nil && !errors.Is(err, fs.ErrNotExist) {
		return err
	}
	return searchWeb(cfg.WebSearch, paths.ConfigFile, websearch.Query{Text: strings.Join(flags.Args(), " "), Count: *count}, os.Stdout)
}

// searchWeb runs one query with the configured provider and prints the
// results. With none configured it says where to set one, since the model
// relays the error to the person who can.
func searchWeb(search config.WebSearch, configFile string, query websearch.Query, output io.Writer) error {
	if !search.Enabled() {
		return fmt.Errorf("web search is off: set web_search.provider in %s (one of %s)", configFile, strings.Join(websearch.Names(), ", "))
	}
	results, err := engines.Search(context.Background(), search.Provider, search.Connection(), query)
	if err != nil {
		return err
	}
	if len(results) == 0 {
		_, err := fmt.Fprintln(output, "no results")
		return err
	}
	return websearch.Write(output, results)
}
