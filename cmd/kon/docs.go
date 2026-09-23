package main

import (
	"fmt"

	productdocs "github.com/hizkifw/kon/docs/product"
	"github.com/hizkifw/kon/internal/config"
)

func docsCommand() command {
	return command{
		name:     "docs",
		summary:  "extract the bundled product guide",
		synopsis: "kon docs",
		detail: "Extract the bundled product guide and print the local directory containing it.\n" +
			"Each release writes its own directory, so renamed or removed pages from an\n" +
			"older release never appear in the path printed by this one.",
		run: runDocs,
	}
}

func runDocs(args []string) error {
	if len(args) > 0 {
		return fmt.Errorf("usage: kon docs")
	}
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
