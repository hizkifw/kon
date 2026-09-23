package main

import (
	"context"
	"flag"
	"fmt"
	"io"
	"os"

	"github.com/hizkifw/kon/internal/catalog"
	"github.com/hizkifw/kon/internal/config"
)

func modelsCommand() command {
	return command{
		name:     "models",
		summary:  "list bundled or cached model IDs offline",
		synopsis: "kon models [--refresh]",
		detail: "List model IDs from the bundled or cached models.dev catalog without\n" +
			"network access.\n\n" +
			"  --refresh   fetch the latest catalog from models.dev before listing",
		run: runModelsCmd,
	}
}

// runModelsCmd parses its own flags and delegates to runModels, which keeps the
// listing logic independent of the command line.
func runModelsCmd(args []string) error {
	fs := flag.NewFlagSet("kon models", flag.ContinueOnError)
	// Help and errors are rendered by the dispatch layer; keep flag from
	// printing its own usage and duplicating the message.
	fs.SetOutput(io.Discard)
	fs.Usage = func() {}
	refresh := fs.Bool("refresh", false, "fetch the latest catalog first")
	if err := fs.Parse(args); err != nil {
		return err
	}
	if fs.NArg() > 0 {
		return fmt.Errorf("usage: kon models [--refresh]")
	}
	paths, err := config.ResolvePaths()
	if err != nil {
		return err
	}
	return withStorage(paths, func() error {
		return runModels(*refresh, paths.Catalog, os.Stdout)
	})
}

// runModels lists every known provider/model ID, optionally refreshing the
// catalog first. It returns the IDs to output so the command remains testable
// without a terminal.
func runModels(refresh bool, cachePath string, output io.Writer) error {
	service, err := catalog.New(cachePath)
	if err != nil {
		return err
	}
	if refresh {
		if err := service.Refresh(context.Background()); err != nil {
			return fmt.Errorf("refresh model catalog: %w", err)
		}
	}
	for _, provider := range service.Providers() {
		for _, model := range service.Models(provider.ID) {
			if _, err := fmt.Fprintf(output, "%s/%s\n", provider.ID, model.ID); err != nil {
				return err
			}
		}
	}
	return nil
}
