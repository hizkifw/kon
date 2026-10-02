package main

import (
	"errors"
	"fmt"
	"os"
	"slices"
	"strings"

	"kon.kitsu.red/internal/contextfiles"
)

// command is one kon subcommand. run receives the arguments after the command
// name; a command owns the flag parsing and validation for its own arguments.
type command struct {
	name string
	// summary is the one-line description shown in the root help index.
	summary string
	// synopsis is the usage line shown by the command's own help.
	synopsis string
	// detail is an optional flag list or note appended to the command's help.
	detail string
	run    func(args []string) error
}

// commands returns kon's subcommands in display order. The root help index and
// lookup both read from here, so a new command only registers in one place.
func commands() []command {
	return []command{runCommand(), acpCommand(), mdCommand(), docsCommand(), modelsCommand(), upgradeCommand(), toolCommand()}
}

func lookup(cmds []command, name string) (command, bool) {
	for _, cmd := range cmds {
		if cmd.name == name {
			return cmd, true
		}
	}
	return command{}, false
}

// help renders the command's own help text.
func (c command) help() string {
	var b strings.Builder
	fmt.Fprintf(&b, "usage: %s\n\n%s", c.synopsis, c.summary)
	if c.detail != "" {
		b.WriteString("\n\n" + c.detail)
	}
	return b.String()
}

// rootUsage renders the help shown by "kon --help". The command list is built
// from the registry so it cannot drift from the commands kon actually accepts.
func rootUsage() string {
	var b strings.Builder
	b.WriteString("usage: kon [--resume [<id>] | --incognito] [--system-prompt-override <file>]\n")
	b.WriteString("           [--instructions <text>]... [--instructions-file <file>]...\n")
	b.WriteString("           [--help] [--version]\n")
	b.WriteString("       kon <command> [flags]\n\n")
	b.WriteString("Start a full-screen kon agent session in the current directory.\n\n")
	b.WriteString("  --resume, -r          resume the most recent session in this directory\n")
	b.WriteString("  --resume=<id>         resume a specific session\n")
	b.WriteString("  --incognito           start a session that is never saved\n")
	b.WriteString("  --system-prompt-override <file>\n")
	b.WriteString("                        replace kon's built-in instructions in new sessions\n")
	b.WriteString("  --instructions <text> add instructions as if from an AGENTS.md; repeatable\n")
	b.WriteString("  --instructions-file <file>\n")
	b.WriteString("                        add a file as if it were an AGENTS.md; repeatable\n")
	b.WriteString("  --help, -h            show this help\n")
	b.WriteString("  --version             print the version\n\n")
	b.WriteString("commands:\n")
	for _, cmd := range commands() {
		fmt.Fprintf(&b, "  %-8s %s\n", cmd.name, cmd.summary)
	}
	b.WriteString("\nRun \"kon <command> --help\" for command flags.\n\n")
	b.WriteString("On exit, kon prints the session ID so the session can be resumed later.\n")
	b.WriteString("An incognito session keeps its conversation and prompts in memory only,\n")
	b.WriteString("so there is nothing to resume.")
	return b.String()
}

// run dispatches to the named subcommand, or to the default session command
// when the first argument is a flag. A leading flag never names a subcommand,
// which keeps "kon --resume docs" meaning a resume rather than a docs request.
func run(args []string) error {
	if len(args) > 0 && !strings.HasPrefix(args[0], "-") {
		cmd, ok := lookup(commands(), args[0])
		if !ok {
			return fmt.Errorf("unknown command %q (try --help)", args[0])
		}
		if wantsHelp(args[1:]) {
			fmt.Println(cmd.help())
			return nil
		}
		return cmd.run(args[1:])
	}
	return runSession(args)
}

// wantsHelp reports whether args requests the command's help. Handling help here
// gives every command one help path and keeps it out of the flag parsers, where
// the flag package would otherwise report "help requested" as an error. Only
// flags count: the first plain word, or "--", starts arguments such as a
// kon run message, which may itself mention -h.
func wantsHelp(args []string) bool {
	for _, arg := range args {
		if arg == "--" || !strings.HasPrefix(arg, "-") {
			return false
		}
		if arg == "-h" || arg == "--help" {
			return true
		}
	}
	return false
}

// readSystemPrompt reads the file named by --system-prompt-override. A blank
// file is rejected rather than taken as no override, which would silently fall
// back to kon's built-in prompt.
func readSystemPrompt(path string) (string, error) {
	b, err := os.ReadFile(path)
	if err != nil {
		return "", fmt.Errorf("--system-prompt-override: %w", err)
	}
	if strings.TrimSpace(string(b)) == "" {
		return "", fmt.Errorf("--system-prompt-override: %s is empty", path)
	}
	return string(b), nil
}

// instruction is one --instructions or --instructions-file argument. Both
// flags share one list so the prompt keeps the order they were given in.
type instruction struct {
	value string
	file  bool
}

// readInstructions turns --instructions text and --instructions-file files
// into AGENTS.md-style context files. Inline text has no path. Each file is
// read now so a bad path fails at launch, and again by the runtime for every
// new session, as discovered files are; a file given twice is listed once.
func readInstructions(given []instruction) ([]contextfiles.File, error) {
	var files []contextfiles.File
	for _, arg := range given {
		if !arg.file {
			if strings.TrimSpace(arg.value) == "" {
				return nil, errors.New("--instructions: text is empty")
			}
			files = append(files, contextfiles.File{Content: arg.value})
			continue
		}
		file, err := contextfiles.Read(arg.value)
		if err != nil {
			return nil, fmt.Errorf("--instructions-file: %w", err)
		}
		if !slices.ContainsFunc(files, func(f contextfiles.File) bool { return f.Path == file.Path }) {
			files = append(files, file)
		}
	}
	return files, nil
}
