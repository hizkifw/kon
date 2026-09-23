package migrations

import (
	"bufio"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
)

const legacyBackupSuffix = ".v1.bak"

func readSessionVersion(path string) (int, error) {
	f, err := os.Open(path)
	if err != nil {
		return 0, err
	}
	defer f.Close()
	line, err := bufio.NewReader(f).ReadBytes('\n')
	if err != nil && !errors.Is(err, io.EOF) {
		return 0, err
	}
	var header struct {
		Type    string `json:"type"`
		Version int    `json:"version"`
	}
	if err := json.Unmarshal(line, &header); err != nil {
		return 0, fmt.Errorf("parse session header: %w", err)
	}
	if header.Type != "session" {
		return 0, errors.New("invalid session header")
	}
	return header.Version, nil
}
