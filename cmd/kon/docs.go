package main

import (
	"fmt"

	productdocs "kon.kitsu.red/docs/product"
	"kon.kitsu.red/internal/config"
)

func docsCommand() command {
	return command{
		name:     "docs",
		summary:  "extract the bundled product guide",
		synopsis: "kon docs",
		detail: "Extract the bundled product guide and print only the path of the local\n" +
			"directory containing it, so list the pages with: ls \"$(kon docs)\"\n\n" +
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
		// Only the path is printed, so a shell can use it: ls "$(kon docs)".
		fmt.Println(dir)
		return nil
	})
}
