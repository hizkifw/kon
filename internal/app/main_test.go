package app

import (
	"compress/gzip"
	"fmt"
	"os"
	"path/filepath"
	"testing"

	"github.com/hizkifw/kon/internal/catalog"
)

// TestMain points every runtime at testdata/catalog.json. The bundled snapshot
// is refreshed from models.dev, which adds and drops models freely, so tests
// that named its entries broke whenever upstream changed. The fixture is
// served as a local cache dated far ahead of any snapshot, which the catalog
// prefers over the bundled one.
func TestMain(m *testing.M) {
	dir, err := os.MkdirTemp("", "kon-app-catalog-")
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	fixture := filepath.Join(dir, "models.json.gz")
	if err := writeFixture(fixture); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	newCatalog = func(string) (*catalog.Service, error) { return catalog.New(fixture) }
	code := m.Run()
	os.RemoveAll(dir)
	os.Exit(code)
}

func writeFixture(path string) error {
	raw, err := os.ReadFile(filepath.Join("testdata", "catalog.json"))
	if err != nil {
		return err
	}
	f, err := os.Create(path)
	if err != nil {
		return err
	}
	zw := gzip.NewWriter(f)
	if _, err := zw.Write(raw); err != nil {
		f.Close()
		return err
	}
	if err := zw.Close(); err != nil {
		f.Close()
		return err
	}
	return f.Close()
}
