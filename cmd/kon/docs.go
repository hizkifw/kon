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
			"Each version of the guide gets its own directory, so pages renamed or removed\n" +
			"since an older version never appear in the path printed by this one.",
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
