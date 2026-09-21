package main

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"

	tea "charm.land/bubbletea/v2"
	"github.com/hizkifw/kon/internal/app"
	"github.com/hizkifw/kon/internal/config"
	"github.com/hizkifw/kon/internal/history"
	"github.com/hizkifw/kon/internal/ui"
)

var version = "dev"

func main() {
	if err := run(os.Args[1:]); err != nil {
		fmt.Fprintln(os.Stderr, "kon:", err)
		os.Exit(1)
	}
}

func run(args []string) error {
	if len(args) > 0 {
		if len(args) == 1 && (args[0] == "-h" || args[0] == "--help") {
			fmt.Println("usage: kon [--help] [--version]\n\nStart a new full-screen kon agent session in the current directory.")
			return nil
		}
		if len(args) == 1 && args[0] == "--version" {
			fmt.Println("kon " + version)
			return nil
		}
		return fmt.Errorf("unknown argument %q (try --help)", args[0])
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

	runtime, err := app.New(cfg, paths, cwd, version)
	if err != nil {
		return err
	}
	model := ui.New(cwd, paths.ConfigFile, runtime, historyStore, historyEntries)
	program := tea.NewProgram(model)
	_, runErr := program.Run()
	return errors.Join(runErr, runtime.Close())
}
