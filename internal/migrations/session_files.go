package migrations

import (
	"bufio"
	"encoding/json"
	"errors"
	"io"
	"os"
)

const legacyBackupSuffix = ".v1.bak"

// errNoHeader marks a file with no readable session header, the normal result
// of a crash right after a session file was created. No kon version can open
// one, so steps skip it rather than refuse to start.
var errNoHeader = errors.New("no session header")

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
	if json.Unmarshal(line, &header) != nil || header.Type != "session" {
		return 0, errNoHeader
	}
	return header.Version, nil
}
