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
	"charm.land/lipgloss/v2"
	"github.com/hizkifw/kon/internal/app"
	"github.com/hizkifw/kon/internal/buildinfo"
	"github.com/hizkifw/kon/internal/config"
	"github.com/hizkifw/kon/internal/history"
	"github.com/hizkifw/kon/internal/migrate"
	"github.com/hizkifw/kon/internal/migrations"
	"github.com/hizkifw/kon/internal/typedid"
	"github.com/hizkifw/kon/internal/ui"
)

// runSession starts the default command: the full-screen TUI. It parses the
// global flags by hand rather than with the flag package because --resume takes
// an optional value ("kon --resume" or "kon --resume ses_..."), which flag
// cannot express.
func runSession(args []string) (runErr error) {
	resume := false
	resumeID := ""
	for i := 0; i < len(args); i++ {
		switch arg := args[i]; {
		case arg == "-h" || arg == "--help":
			fmt.Println(rootUsage())
			return nil
		case arg == "--version":
			fmt.Println("kon " + buildinfo.Version())
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
		runtime, err = app.NewResumedID(cfg, paths, cwd, buildinfo.Version(), id)
	case resume:
		runtime, err = app.NewResumed(cfg, paths, cwd, buildinfo.Version())
	default:
		runtime, err = app.New(cfg, paths, cwd, buildinfo.Version())
	}
	if err != nil {
		return err
	}
	uiCtx, cancelUI := context.WithCancel(context.Background())
	defer cancelUI()
	model := ui.New(uiCtx, cwd, paths.ConfigFile, runtime, historyStore, historyEntries)
	// Reuse the color profile lipgloss detected at init. Detecting again costs a
	// `tmux info` subprocess inside tmux, which delays the first frame.
	program := tea.NewProgram(model, tea.WithColorProfile(lipgloss.Writer.Profile))
	_, uiErr := program.Run()
	// A signal quits the program without reaching the key handler that cancels
	// an active run. Cancel here so the run's event sends stop waiting on a
	// reader that is gone; otherwise Runtime.Close would wait on it forever.
	cancelUI()
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
