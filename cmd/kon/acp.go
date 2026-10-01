package main

import (
	"context"
	"errors"
	"os"
	"os/signal"
	"syscall"

	"kon.kitsu.red/core/typedid"
	"kon.kitsu.red/internal/acp"
	"kon.kitsu.red/internal/app"
	"kon.kitsu.red/internal/buildinfo"
	"kon.kitsu.red/internal/config"
	"kon.kitsu.red/internal/sessions"
)

func acpCommand() command {
	return command{
		name:     "acp",
		summary:  "serve the Agent Client Protocol on stdin and stdout",
		synopsis: "kon acp",
		detail: "Run kon as an agent for an editor that speaks the Agent Client Protocol.\n" +
			"The editor starts kon acp and exchanges JSON-RPC messages with it on\n" +
			"stdin and stdout. Tools run without confirmation, as they do in the\n" +
			"full-screen UI. See the editor integration page of kon docs.",
		run: runACP,
	}
}

func runACP(args []string) error {
	if len(args) > 0 {
		return usageError(errors.New("kon acp takes no arguments"))
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
			Start: func(cwd string, id typedid.SessionID) (acp.Runtime, error) {
				cfg, err := config.Load(paths.ConfigFile)
				if err != nil {
					return nil, err
				}
				return app.Start(cfg, paths, cwd, buildinfo.Version(), app.Options{Resume: !id.IsZero(), SessionID: id})
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
