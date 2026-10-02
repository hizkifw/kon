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
	"kon.kitsu.red/core/typedid"
	"kon.kitsu.red/internal/app"
	"kon.kitsu.red/internal/buildinfo"
	"kon.kitsu.red/internal/config"
	"kon.kitsu.red/internal/history"
	"kon.kitsu.red/internal/migrate"
	"kon.kitsu.red/internal/migrations"
	"kon.kitsu.red/internal/ui"
)

// runSession starts the default command: the full-screen TUI. It parses the
// global flags by hand rather than with the flag package because --resume takes
// an optional value ("kon --resume" or "kon --resume ses_..."), which flag
// cannot express.
func runSession(args []string) (runErr error) {
	resume, incognito := false, false
	resumeID, systemPromptFile := "", ""
	var instructions []instruction
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
		case arg == "--incognito":
			incognito = true
		case arg == "--system-prompt-override":
			if i+1 >= len(args) {
				return errors.New("--system-prompt-override needs a value")
			}
			i++
			systemPromptFile = args[i]
		case arg == "--instructions" || arg == "--instructions-file":
			if i+1 >= len(args) {
				return fmt.Errorf("%s needs a value", arg)
			}
			i++
			instructions = append(instructions, instruction{value: args[i], file: arg == "--instructions-file"})
		case strings.HasPrefix(arg, "--instructions="):
			instructions = append(instructions, instruction{value: strings.TrimPrefix(arg, "--instructions=")})
		case strings.HasPrefix(arg, "--instructions-file="):
			instructions = append(instructions, instruction{value: strings.TrimPrefix(arg, "--instructions-file="), file: true})
		case strings.HasPrefix(arg, "--system-prompt-override="):
			systemPromptFile = strings.TrimPrefix(arg, "--system-prompt-override=")
			// An empty value would otherwise mean no override at all.
			if systemPromptFile == "" {
				return errors.New("--system-prompt-override needs a file")
			}
		default:
			return fmt.Errorf("unknown argument %q (try --help)", arg)
		}
	}
	// Continuing a saved session would write to it, which incognito promises
	// not to do.
	if incognito && resume {
		return errors.New("--incognito cannot be combined with --resume")
	}

	var id typedid.SessionID
	if resumeID != "" {
		parsed, err := typedid.ParseSessionID(resumeID)
		if err != nil {
			return err
		}
		id = parsed
	}

	var systemPrompt string
	if systemPromptFile != "" {
		text, err := readSystemPrompt(systemPromptFile)
		if err != nil {
			return err
		}
		systemPrompt = text
	}
	contextFiles, err := readInstructions(instructions)
	if err != nil {
		return err
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
	cwd, err := workingDirectory()
	if err != nil {
		return err
	}
	historyStore := history.New(paths.History)
	historyEntries, err := historyStore.Load()
	if err != nil {
		return err
	}
	if incognito {
		// Earlier prompts can still be recalled; this session's are not
		// recorded.
		historyStore = nil
	}
	runtime, err := app.Start(cfg, paths, cwd, buildinfo.Version(), app.Options{
		Resume: resume, SessionID: id, Incognito: incognito,
		SystemPrompt: systemPrompt, Instructions: contextFiles,
	})
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
	switch {
	case incognito:
		fmt.Fprintln(os.Stderr, "\nincognito session discarded; nothing to resume")
	case !sessionID.IsZero():
		fmt.Fprintf(os.Stderr, "\nresume with: kon --resume %s\n", sessionID)
	}
	return errors.Join(uiErr, closeErr)
}

// workingDirectory is the directory sessions belong to. Symlinks are resolved
// so one project reached by two paths shares its sessions.
func workingDirectory() (string, error) {
	cwd, err := os.Getwd()
	if err != nil {
		return "", fmt.Errorf("get working directory: %w", err)
	}
	if canonical, err := filepath.EvalSymlinks(cwd); err == nil {
		cwd = canonical
	}
	return cwd, nil
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
