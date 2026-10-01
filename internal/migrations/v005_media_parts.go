package migrations

import (
	"bufio"
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"os"
	"path/filepath"
	"sort"
	"strconv"
	"strings"

	"kon.kitsu.red/internal/config"
)

// sessionV5 generalizes v4's image part into a media part, so audio, video,
// and documents share one representation. Nothing else changed.
const sessionV5 = 5

const (
	v5PartMedia      = "media"
	v4BackupSuffix   = ".v4.bak"
	v5TempFilePrefix = ".v5-"
)

type mediaPartsV5 struct{}

func (mediaPartsV5) Version() int { return 5 }
func (mediaPartsV5) Name() string { return "convert session image parts to media parts" }

// Run rewrites every v4 session as v5. Only the header and the lines holding
// an image part change; every other line, the root system prompt included,
// is copied byte for byte. Blobs keep their names, which are content hashes.
func (mediaPartsV5) Run(ctx context.Context, paths config.Paths) error {
	files := make(map[string]bool)
	err := filepath.WalkDir(paths.Sessions, func(path string, entry fs.DirEntry, walkErr error) error {
		if walkErr != nil {
			return walkErr
		}
		if entry.IsDir() {
			return nil
		}
		if strings.HasPrefix(entry.Name(), v5TempFilePrefix) {
			return os.Remove(path)
		}
		switch {
		case strings.HasSuffix(path, ".jsonl"):
			files[path] = true
		case strings.HasSuffix(path, ".jsonl"+v4BackupSuffix):
			files[strings.TrimSuffix(path, v4BackupSuffix)] = true
		}
		return nil
	})
	if errors.Is(err, os.ErrNotExist) {
		return nil
	}
	if err != nil {
		return fmt.Errorf("list sessions for migration: %w", err)
	}
	sorted := make([]string, 0, len(files))
	for path := range files {
		sorted = append(sorted, path)
	}
	sort.Strings(sorted)
	for _, path := range sorted {
		if err := ctx.Err(); err != nil {
			return err
		}
		if err := migrateV4File(ctx, path); err != nil {
			return fmt.Errorf("migrate session %s: %w", path, err)
		}
	}
	return nil
}

// migrateV4File converts one session. The original is renamed to a stable
// backup name before the converted copy is installed, so a process killed
// between the two renames resumes from the backup.
func migrateV4File(ctx context.Context, path string) error {
	backup := path + v4BackupSuffix
	_, backupErr := os.Stat(backup)
	backupExists := backupErr == nil
	if backupErr != nil && !errors.Is(backupErr, os.ErrNotExist) {
		return backupErr
	}
	version, err := readSessionVersion(path)
	if errors.Is(err, os.ErrNotExist) && backupExists {
		version, err = readSessionVersion(backup)
	} else if errors.Is(err, os.ErrNotExist) {
		return nil
	}
	if errors.Is(err, errNoHeader) {
		return nil
	}
	if err != nil {
		return err
	}
	if version == sessionV5 {
		if err := validateV5File(path); err != nil {
			return err
		}
		if backupExists {
			return os.Remove(backup)
		}
		return nil
	}
	if version != sessionV4 {
		return fmt.Errorf("unsupported session version %d", version)
	}
	source := path
	if backupExists {
		if _, err := os.Stat(path); err == nil {
			return errors.New("both a v4 session and its migration backup exist")
		} else if !errors.Is(err, os.ErrNotExist) {
			return err
		}
		source = backup
	}
	if err := validateV4File(source); err != nil {
		return err
	}
	tmp, err := os.CreateTemp(filepath.Dir(path), v5TempFilePrefix+"*")
	if err != nil {
		return err
	}
	defer os.Remove(tmp.Name())
	if err := tmp.Chmod(0o600); err != nil {
		_ = tmp.Close()
		return err
	}
	if err := rewriteV4(ctx, source, tmp); err != nil {
		_ = tmp.Close()
		return err
	}
	if err := tmp.Sync(); err != nil {
		_ = tmp.Close()
		return err
	}
	if err := tmp.Close(); err != nil {
		return err
	}
	if err := validateV5File(tmp.Name()); err != nil {
		return fmt.Errorf("validate converted session: %w", err)
	}
	if !backupExists {
		if err := os.Rename(path, backup); err != nil {
			return fmt.Errorf("save v4 session backup: %w", err)
		}
	}
	if err := os.Rename(tmp.Name(), path); err != nil {
		return fmt.Errorf("install converted session (original at %s): %w", backup, err)
	}
	return os.Remove(backup)
}

// rewriteV4 copies a v4 session to output as v5. A torn final record, which
// the v4 reader trimmed on open, is dropped.
func rewriteV4(ctx context.Context, source string, output io.Writer) error {
	b, err := os.ReadFile(source)
	if err != nil {
		return err
	}
	writer := bufio.NewWriter(output)
	lines := bytes.Split(b, []byte("\n"))
	for i, line := range lines {
		if err := ctx.Err(); err != nil {
			return err
		}
		if len(bytes.TrimSpace(line)) == 0 {
			continue
		}
		converted, err := convertV4Line(line, i == 0)
		if err != nil {
			if i > 0 && len(bytes.TrimSpace(bytes.Join(lines[i+1:], nil))) == 0 && !json.Valid(line) {
				break
			}
			return fmt.Errorf("line %d: %w", i+1, err)
		}
		if _, err := writer.Write(converted); err != nil {
			return err
		}
		if err := writer.WriteByte('\n'); err != nil {
			return err
		}
	}
	return writer.Flush()
}

// convertV4Line returns line as v5: the header with its new version, a
// message's image parts as media parts, and anything else unchanged.
func convertV4Line(line []byte, header bool) ([]byte, error) {
	var record map[string]json.RawMessage
	if err := json.Unmarshal(line, &record); err != nil {
		return nil, err
	}
	if header {
		record["version"] = []byte(strconv.Itoa(sessionV5))
		return json.Marshal(record)
	}
	var entry struct {
		Type    string `json:"type"`
		Message *struct {
			Parts []v4Part `json:"parts"`
		} `json:"message"`
	}
	if err := json.Unmarshal(line, &entry); err != nil {
		return nil, err
	}
	if entry.Type != "message" || entry.Message == nil || !hasV4Image(entry.Message.Parts) {
		return line, nil
	}
	var message map[string]json.RawMessage
	if err := json.Unmarshal(record["message"], &message); err != nil {
		return nil, err
	}
	var parts []map[string]json.RawMessage
	if err := json.Unmarshal(message["parts"], &parts); err != nil {
		return nil, err
	}
	for i, part := range parts {
		if entry.Message.Parts[i].Type != v4PartImage {
			continue
		}
		part["type"] = mustJSON(v5PartMedia)
		part["media_hash"], part["media_mime"] = part["image_hash"], part["image_mime"]
		delete(part, "image_hash")
		delete(part, "image_mime")
	}
	message["parts"] = mustJSON(parts)
	record["message"] = mustJSON(message)
	return json.Marshal(record)
}

func hasV4Image(parts []v4Part) bool {
	for _, part := range parts {
		if part.Type == v4PartImage {
			return true
		}
	}
	return false
}

// validateV5File checks a converted session. Conversion changes only the
// header version and image parts, and the source passed validateV4File, so
// this checks just what changed: the version, and that every media part is
// well formed with no image part left behind.
func validateV5File(path string) error {
	b, err := os.ReadFile(path)
	if err != nil {
		return fmt.Errorf("read session: %w", err)
	}
	lines := strings.Split(strings.TrimRight(string(b), "\n"), "\n")
	var header v4Header
	if err := json.Unmarshal([]byte(lines[0]), &header); err != nil {
		return fmt.Errorf("parse session header: %w", err)
	}
	if header.Type != "session" || header.ID.IsZero() || header.Version != sessionV5 {
		return errors.New("invalid v5 session header")
	}
	for i, line := range lines[1:] {
		if strings.TrimSpace(line) == "" {
			continue
		}
		var entry struct {
			Message *struct {
				Parts []struct {
					Type      string `json:"type"`
					Text      string `json:"text"`
					MediaHash string `json:"media_hash"`
					MediaMIME string `json:"media_mime"`
				} `json:"parts"`
			} `json:"message"`
		}
		if err := json.Unmarshal([]byte(line), &entry); err != nil {
			// The session reader trims one torn final record on open.
			if i == len(lines)-2 {
				break
			}
			return fmt.Errorf("parse session line %d: %w", i+2, err)
		}
		if entry.Message == nil {
			continue
		}
		for _, part := range entry.Message.Parts {
			if part.Type == v4PartImage {
				return fmt.Errorf("session line %d: image part was not converted", i+2)
			}
			if part.Type == v5PartMedia && (part.Text != "" || !validV4ImageHash(part.MediaHash) || part.MediaMIME == "") {
				return fmt.Errorf("session line %d: media part requires a blob hash and MIME type", i+2)
			}
		}
	}
	return nil
}
