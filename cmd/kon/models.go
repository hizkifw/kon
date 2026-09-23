package main

import (
	"context"
	"fmt"
	"io"

	"github.com/hizkifw/kon/internal/catalog"
)

func runModels(args []string, cachePath string, output io.Writer) error {
	if len(args) > 1 || len(args) == 1 && args[0] != "--refresh" {
		return fmt.Errorf("usage: kon models [--refresh]")
	}
	service, err := catalog.New(cachePath)
	if err != nil {
		return err
	}
	if len(args) == 1 {
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
