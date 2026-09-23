package main

import (
	"fmt"
	"strings"
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
	return []command{docsCommand(), modelsCommand()}
}

func lookup(name string) (command, bool) {
	for _, cmd := range commands() {
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
	b.WriteString("usage: kon [--resume [<id>]] [--help] [--version]\n")
	b.WriteString("       kon <command> [flags]\n\n")
	b.WriteString("Start a full-screen kon agent session in the current directory.\n\n")
	b.WriteString("  --resume, -r          resume the most recent session in this directory\n")
	b.WriteString("  --resume=<id>         resume a specific session\n")
	b.WriteString("  --help, -h            show this help\n")
	b.WriteString("  --version             print the version\n\n")
	b.WriteString("commands:\n")
	for _, cmd := range commands() {
		fmt.Fprintf(&b, "  %-8s %s\n", cmd.name, cmd.summary)
	}
	b.WriteString("\nRun \"kon <command> --help\" for command flags.\n\n")
	b.WriteString("On exit, kon prints the session ID so the session can be resumed later.")
	return b.String()
}

// run dispatches to the named subcommand, or to the default session command
// when the first argument is a flag. A leading flag never names a subcommand,
// which keeps "kon --resume docs" meaning a resume rather than a docs request.
func run(args []string) error {
	if len(args) > 0 && !strings.HasPrefix(args[0], "-") {
		cmd, ok := lookup(args[0])
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
// the flag package would otherwise report "help requested" as an error.
func wantsHelp(args []string) bool {
	for _, arg := range args {
		if arg == "-h" || arg == "--help" {
			return true
		}
	}
	return false
}
