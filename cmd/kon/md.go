package main

import (
	"errors"
	"flag"
	"io"
	"os"

	"charm.land/lipgloss/v2"
	"kon.kitsu.red/internal/ui"
)

const mdSynopsis = "kon md [--width <n>] [file]"

func mdCommand() command {
	return command{
		name:     "md",
		summary:  "render Markdown for the terminal",
		synopsis: mdSynopsis,
		detail: "Render Markdown from a file, or from piped stdin when there is none, the\n" +
			"way kon shows a reply. Each block prints as soon as it closes, so a model's\n" +
			"output piped in, as from kon run, renders while it streams. Color and\n" +
			"links are written only to a terminal, and NO_COLOR and CLICOLOR_FORCE\n" +
			"are respected.\n\n" +
			"  --width, -w <n>   wrap lines at n columns (default 80); 0 turns wrapping off",
		run: runMd,
	}
}

func runMd(args []string) error {
	fs := flag.NewFlagSet("kon md", flag.ContinueOnError)
	// Help and errors are rendered by the dispatch layer; keep flag from
	// printing its own usage and duplicating the message.
	fs.SetOutput(io.Discard)
	fs.Usage = func() {}
	width := fs.Int("width", 80, "")
	fs.IntVar(width, "w", 80, "")
	if err := fs.Parse(args); err != nil {
		return usageError(err)
	}
	if fs.NArg() > 1 {
		return usageError(errors.New("usage: " + mdSynopsis))
	}
	if *width < 0 {
		return usageError(errors.New("--width must be 0 or more"))
	}
	input := io.Reader(os.Stdin)
	switch {
	case fs.NArg() == 1:
		file, err := os.Open(fs.Arg(0))
		if err != nil {
			return err
		}
		defer file.Close()
		input = file
	case isTerminal(os.Stdin):
		// Waiting on a terminal would look like a hang to someone trying
		// the command out.
		return usageError(errors.New("no input: pipe Markdown in or name a file"))
	}
	// lipgloss.Writer is stdout with its colors fitted to what stdout is: all
	// escapes are dropped when it is not a terminal.
	return ui.PrintMarkdown(lipgloss.Writer, input, *width)
}
