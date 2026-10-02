package main

import (
	"context"
	"errors"
	"flag"
	"io"
	"os"
	"os/signal"
	"slices"
	"strings"
	"syscall"

	"kon.kitsu.red/core/typedid"
	"kon.kitsu.red/internal/acp"
	"kon.kitsu.red/internal/app"
	"kon.kitsu.red/internal/buildinfo"
	"kon.kitsu.red/internal/config"
	"kon.kitsu.red/internal/contextfiles"
	"kon.kitsu.red/internal/sessions"
)

func acpCommand() command {
	return command{
		name:     "acp",
		summary:  "serve the Agent Client Protocol on stdin and stdout",
		synopsis: "kon acp [--system-prompt-override <file>] [--instructions <text>]... [--instructions-file <file>]...",
		detail: "Run kon as an agent for an editor that speaks the Agent Client Protocol.\n" +
			"The editor starts kon acp and exchanges JSON-RPC messages with it on\n" +
			"stdin and stdout. Tools run without confirmation, as they do in the\n" +
			"full-screen UI. See the editor integration page of kon docs.\n\n" +
			"  --system-prompt-override <file>\n" +
			"                         replace kon's built-in instructions in new sessions\n" +
			"  --instructions <text>  add instructions as if from an AGENTS.md; repeatable\n" +
			"  --instructions-file <file>\n" +
			"                         add a file as if it were an AGENTS.md; repeatable",
		run: runACP,
	}
}

func runACP(args []string) error {
	fs := flag.NewFlagSet("kon acp", flag.ContinueOnError)
	fs.SetOutput(io.Discard)
	var systemPromptFile string
	fs.Func("system-prompt-override", "", func(path string) error {
		// An empty value would otherwise mean no override at all.
		if path == "" {
			return errors.New("needs a file")
		}
		systemPromptFile = path
		return nil
	})
	var given []instruction
	fs.Func("instructions", "", func(text string) error {
		given = append(given, instruction{value: text})
		return nil
	})
	fs.Func("instructions-file", "", func(path string) error {
		given = append(given, instruction{value: path, file: true})
		return nil
	})
	if err := fs.Parse(args); err != nil {
		return usageError(err)
	}
	if fs.NArg() > 0 {
		return usageError(errors.New("kon acp takes no arguments"))
	}
	var systemPrompt string
	if systemPromptFile != "" {
		text, err := readSystemPrompt(systemPromptFile)
		if err != nil {
			return err
		}
		systemPrompt = text
	}
	instructions, err := readInstructions(given)
	if err != nil {
		return err
	}
	paths, err := config.ResolvePaths()
	if err != nil {
		return err
	}
	return withStorage(paths, func() error {
		if _, err := config.Initialize(paths); err != nil {
			return err
		}
		cwd, err := workingDirectory()
		if err != nil {
			return err
		}
		server := &acp.Server{
			// Each session reads the config afresh, so one started after
			// another changed the model starts on the new default.
			Start: func(cwd string, id typedid.SessionID, extra string) (acp.Runtime, error) {
				cfg, err := config.Load(paths.ConfigFile)
				if err != nil {
					return nil, err
				}
				// A client's instructions come after kon acp's own, on a
				// copy so they never reach another session's prompt.
				files := slices.Clone(instructions)
				if strings.TrimSpace(extra) != "" {
					files = append(files, contextfiles.File{Content: extra})
				}
				return app.Start(cfg, paths, cwd, buildinfo.Version(), app.Options{
					Resume: !id.IsZero(), SessionID: id, SystemPrompt: systemPrompt, Instructions: files,
				})
			},
			List:    func(cwd string) ([]sessions.Summary, error) { return sessions.Discover(paths.Sessions, cwd) },
			CWD:     cwd,
			Version: buildinfo.Version(),
		}
		ctx, stop := signal.NotifyContext(context.Background(), os.Interrupt, syscall.SIGTERM)
		defer stop()
		return server.Serve(ctx, os.Stdin, os.Stdout)
	})
}
