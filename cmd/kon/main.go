package main

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	tea "charm.land/bubbletea/v2"
	"github.com/hizkifw/kon/internal/app"
	"github.com/hizkifw/kon/internal/config"
	"github.com/hizkifw/kon/internal/history"
	"github.com/hizkifw/kon/internal/typedid"
	"github.com/hizkifw/kon/internal/ui"
)

var version = "dev"

const usage = `usage: kon [--resume [<id>]] [--help] [--version]
       kon models [--refresh]

Start a full-screen kon agent session in the current directory.

  --resume, -r          resume the most recent session in this directory
  --resume=<id>         resume a specific session
  --help, -h            show this help
  --version             print the version
  models                list bundled or cached models without network access
  models --refresh      fetch the latest models.dev catalog, then list models

On exit, kon prints the session ID so the session can be resumed later.`

func main() {
	if err := run(os.Args[1:]); err != nil {
		fmt.Fprintln(os.Stderr, "kon:", err)
		os.Exit(1)
	}
}

func run(args []string) error {
	if len(args) > 0 && args[0] == "models" {
		paths, err := config.ResolvePaths()
		if err != nil {
			return err
		}
		return runModels(args[1:], paths.Catalog, os.Stdout)
	}
	resume := false
	resumeID := ""
	for i := 0; i < len(args); i++ {
		switch arg := args[i]; {
		case arg == "-h" || arg == "--help":
			fmt.Println(usage)
			return nil
		case arg == "--version":
			fmt.Println("kon " + version)
			return nil
		case arg == "--resume" || arg == "-r":
			resume = true
			// A bare session ID may follow the flag: "kon --resume ses_...".
			if i+1 < len(args) && strings.HasPrefix(args[i+1], "ses_") {
				i++
				resumeID = args[i]
			}
		case strings.HasPrefix(arg, "--resume="):
			resume, resumeID = true, strings.TrimPrefix(arg, "--resume=")
		default:
			return fmt.Errorf("unknown argument %q (try --help)", arg)
		}
	}

	var id typedid.SessionID
	if resumeID != "" {
		parsed, err := typedid.ParseSessionID(resumeID)
		if err != nil {
			return err
		}
		id = parsed
	}

	paths, err := config.ResolvePaths()
	if err != nil {
		return err
	}
	cfg, err := config.Initialize(paths)
	if err != nil {
		return err
	}
	cwd, err := os.Getwd()
	if err != nil {
		return fmt.Errorf("get working directory: %w", err)
	}
	if canonical, canonicalErr := filepath.EvalSymlinks(cwd); canonicalErr == nil {
		cwd = canonical
	}
	historyStore := history.New(paths.History)
	historyEntries, err := historyStore.Load()
	if err != nil {
		return err
	}
	var runtime *app.Runtime
	switch {
	case resumeID != "":
		runtime, err = app.NewResumedID(cfg, paths, cwd, version, id)
	case resume:
		runtime, err = app.NewResumed(cfg, paths, cwd, version)
	default:
		runtime, err = app.New(cfg, paths, cwd, version)
	}
	if err != nil {
		return err
	}
	model := ui.New(cwd, paths.ConfigFile, runtime, historyStore, historyEntries)
	program := tea.NewProgram(model)
	_, runErr := program.Run()
	// Capture the session ID before closing the runtime; Close releases the
	// store that owns the header.
	sessionID := runtime.SessionID()
	closeErr := runtime.Close()
	if !sessionID.IsZero() {
		fmt.Fprintf(os.Stderr, "\nresume with: kon --resume %s\n", sessionID)
	}
	return errors.Join(runErr, closeErr)
}
