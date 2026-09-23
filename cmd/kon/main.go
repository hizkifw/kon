package main

import (
	"context"
	"errors"
	"fmt"
	"os"
	"os/signal"
	"path/filepath"
	"strings"

	tea "charm.land/bubbletea/v2"
	productdocs "github.com/hizkifw/kon/docs/product"
	"github.com/hizkifw/kon/internal/app"
	"github.com/hizkifw/kon/internal/config"
	"github.com/hizkifw/kon/internal/history"
	"github.com/hizkifw/kon/internal/migrate"
	"github.com/hizkifw/kon/internal/migrations"
	"github.com/hizkifw/kon/internal/typedid"
	"github.com/hizkifw/kon/internal/ui"
)

var version = "dev"

const usage = `usage: kon [--resume [<id>]] [--help] [--version]
       kon docs
       kon models [--refresh]

Start a full-screen kon agent session in the current directory.

  --resume, -r          resume the most recent session in this directory
  --resume=<id>         resume a specific session
  --help, -h            show this help
  --version             print the version
  docs                  extract bundled product docs and print their directory
  models                list bundled or cached models without network access
  models --refresh      fetch the latest models.dev catalog, then list models

On exit, kon prints the session ID so the session can be resumed later.`

func main() {
	if err := run(os.Args[1:]); err != nil {
		fmt.Fprintln(os.Stderr, "kon:", err)
		os.Exit(1)
	}
}

func run(args []string) (runErr error) {
	if len(args) > 0 && args[0] == "models" {
		paths, err := config.ResolvePaths()
		if err != nil {
			return err
		}
		return withStorage(paths, func() error { return runModels(args[1:], paths.Catalog, os.Stdout) })
	}
	if len(args) == 1 && args[0] == "docs" {
		paths, err := config.ResolvePaths()
		if err != nil {
			return err
		}
		return withStorage(paths, func() error {
			dir, err := productdocs.Extract(paths.DataDir)
			if err != nil {
				return err
			}
			fmt.Println("Documentation extracted to: " + dir)
			return nil
		})
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
	guard, err := enterStorage(paths)
	if err != nil {
		return err
	}
	defer func() { runErr = errors.Join(runErr, guard.Close()) }()
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
	_, uiErr := program.Run()
	// Capture the session ID before closing the runtime; Close releases the
	// store that owns the header.
	sessionID := runtime.SessionID()
	closeErr := runtime.Close()
	if !sessionID.IsZero() {
		fmt.Fprintf(os.Stderr, "\nresume with: kon --resume %s\n", sessionID)
	}
	return errors.Join(uiErr, closeErr)
}

func enterStorage(paths config.Paths) (*migrate.Guard, error) {
	ctx, stop := signal.NotifyContext(context.Background(), os.Interrupt)
	defer stop()
	return migrate.Enter(ctx, paths, migrations.Ordered(), func(message string) {
		fmt.Fprintln(os.Stderr, "kon:", message)
	})
}

func withStorage(paths config.Paths, work func() error) (err error) {
	guard, err := enterStorage(paths)
	if err != nil {
		return err
	}
	defer func() { err = errors.Join(err, guard.Close()) }()
	return work()
}
