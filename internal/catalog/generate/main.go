// Command generate updates the bundled catalog snapshot. It is run explicitly
// with go generate; normal builds never contact models.dev.
package main

import (
	"compress/gzip"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/http"
	"os"
	"path/filepath"
	"time"

	"github.com/hizkifw/kon/internal/buildinfo"
)

const sourceURL = "https://models.dev/api.json"

func main() {
	if err := run(); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}

func run() error {
	client := &http.Client{Timeout: 30 * time.Second}
	req, err := http.NewRequest(http.MethodGet, sourceURL, nil)
	if err != nil {
		return err
	}
	req.Header.Set("User-Agent", buildinfo.UserAgent())
	res, err := client.Do(req)
	if err != nil {
		return err
	}
	defer res.Body.Close()
	if res.StatusCode != http.StatusOK {
		return fmt.Errorf("fetch catalog: HTTP %d", res.StatusCode)
	}
	raw, err := io.ReadAll(io.LimitReader(res.Body, (16<<20)+1))
	if err != nil {
		return err
	}
	if len(raw) > 16<<20 {
		return errors.New("catalog exceeds size limit")
	}
	var providers map[string]struct {
		ID     string `json:"id"`
		Name   string `json:"name"`
		Models map[string]struct {
			ID   string `json:"id"`
			Name string `json:"name"`
		} `json:"models"`
	}
	if err := json.Unmarshal(raw, &providers); err != nil || len(providers) == 0 {
		return errors.New("invalid catalog response")
	}
	for id, provider := range providers {
		if provider.ID != id || provider.Name == "" || provider.Models == nil {
			return fmt.Errorf("invalid provider %q", id)
		}
		for modelID, model := range provider.Models {
			if model.ID != modelID || model.Name == "" {
				return fmt.Errorf("invalid model %q/%q", id, modelID)
			}
		}
	}
	compact, err := project(raw)
	if err != nil {
		return err
	}
	path := "snapshot.json.gz"
	tmp, err := os.CreateTemp(filepath.Dir(path), "catalog-*.tmp")
	if err != nil {
		return err
	}
	defer os.Remove(tmp.Name())
	zw := gzip.NewWriter(tmp)
	bundled := struct {
		FetchedAt time.Time       `json:"fetched_at"`
		Catalog   json.RawMessage `json:"catalog"`
	}{FetchedAt: time.Now().UTC(), Catalog: compact}
	if err := json.NewEncoder(zw).Encode(bundled); err != nil {
		zw.Close()
		tmp.Close()
		return err
	}
	if err := zw.Close(); err != nil {
		tmp.Close()
		return err
	}
	if err := tmp.Close(); err != nil {
		return err
	}
	if err := os.Rename(tmp.Name(), path); err != nil {
		return err
	}
	info, err := os.Stat(path)
	if err != nil {
		return err
	}
	fmt.Printf("updated %s: %d providers, %d bytes compressed\n", path, len(providers), info.Size())
	return nil
}

// The serving package exposes these fields; keeping the snapshot to that shape
// avoids decoding unused upstream data on every CLI launch.
func project(raw []byte) ([]byte, error) {
	var providers map[string]map[string]json.RawMessage
	if err := json.Unmarshal(raw, &providers); err != nil {
		return nil, err
	}
	providerFields := map[string]bool{"id": true, "name": true, "api": true, "npm": true, "env": true, "models": true}
	modelFields := map[string]bool{
		"id": true, "name": true, "description": true, "family": true,
		"attachment": true, "reasoning": true, "reasoning_options": true, "tool_call": true,
		"structured_output": true, "modalities": true, "limit": true, "cost": true,
	}
	for _, provider := range providers {
		var models map[string]map[string]json.RawMessage
		if err := json.Unmarshal(provider["models"], &models); err != nil {
			return nil, err
		}
		for _, model := range models {
			for key := range model {
				if !modelFields[key] {
					delete(model, key)
				}
			}
		}
		projected, err := json.Marshal(models)
		if err != nil {
			return nil, err
		}
		provider["models"] = projected
		for key := range provider {
			if !providerFields[key] {
				delete(provider, key)
			}
		}
	}
	return json.Marshal(providers)
}
