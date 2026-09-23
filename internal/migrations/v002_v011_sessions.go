package migrations

import (
	"bufio"
	"bytes"
	"context"
	"crypto/sha256"
	"encoding/base64"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"os"
	"path/filepath"
	"sort"
	"strings"

	"github.com/hizkifw/kon/internal/config"
	"github.com/hizkifw/kon/internal/session"
	"github.com/hizkifw/kon/internal/typedid"
)

type v011SessionsV2 struct{}

func (v011SessionsV2) Version() int { return 2 }
func (v011SessionsV2) Name() string { return "convert v0.1.1 sessions to v4" }
func (v011SessionsV2) Run(ctx context.Context, paths config.Paths) error {
	return migrateV011Sessions(ctx, paths)
}

// migrateV011Sessions upgrades the v1 JSONL format shipped in v0.1.1. The
// backup name is stable so a process killed between the two renames can resume.
func migrateV011Sessions(ctx context.Context, paths config.Paths) error {
	files := make(map[string]bool)
	err := filepath.WalkDir(paths.Sessions, func(path string, entry fs.DirEntry, walkErr error) error {
		if walkErr != nil {
			return walkErr
		}
		if entry.IsDir() {
			return nil
		}
		if strings.HasPrefix(entry.Name(), ".v011-") {
			return os.Remove(path)
		}
		switch {
		case strings.HasSuffix(path, ".jsonl"):
			files[path] = true
		case strings.HasSuffix(path, ".jsonl"+legacyBackupSuffix):
			files[strings.TrimSuffix(path, legacyBackupSuffix)] = true
		}
		return nil
	})
	if errors.Is(err, os.ErrNotExist) {
		return nil
	}
	if err != nil {
		return fmt.Errorf("list sessions for migration: %w", err)
	}
	pathsToMigrate := make([]string, 0, len(files))
	for path := range files {
		pathsToMigrate = append(pathsToMigrate, path)
	}
	sort.Strings(pathsToMigrate)
	for _, path := range pathsToMigrate {
		if err := ctx.Err(); err != nil {
			return err
		}
		if err := migrateV1File(ctx, path); err != nil {
			return fmt.Errorf("migrate session %s: %w", path, err)
		}
	}
	return nil
}

func migrateV1File(ctx context.Context, path string) error {
	backup := path + legacyBackupSuffix
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
	if err != nil {
		return err
	}
	if version == session.SchemaVersion {
		if err := session.ValidateFile(path); err != nil {
			return err
		}
		if backupExists {
			return os.Remove(backup)
		}
		return nil
	}
	if version != 1 {
		return fmt.Errorf("unsupported session version %d", version)
	}
	if backupExists {
		if _, err := os.Stat(path); err == nil {
			return errors.New("both a v1 session and its migration backup exist")
		} else if !errors.Is(err, os.ErrNotExist) {
			return err
		}
	}
	source := path
	if backupExists {
		source = backup
	}
	tmp, err := os.CreateTemp(filepath.Dir(path), ".v011-*")
	if err != nil {
		return err
	}
	defer os.Remove(tmp.Name())
	if err := tmp.Chmod(0o600); err != nil {
		_ = tmp.Close()
		return err
	}
	if err := rewriteV1(ctx, source, path, tmp); err != nil {
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
	if err := session.ValidateFile(tmp.Name()); err != nil {
		return fmt.Errorf("validate converted session: %w", err)
	}
	if !backupExists {
		if err := os.Rename(path, backup); err != nil {
			return fmt.Errorf("save v1 session backup: %w", err)
		}
	}
	if err := os.Rename(tmp.Name(), path); err != nil {
		return fmt.Errorf("install converted session (original at %s): %w", backup, err)
	}
	if err := session.ValidateFile(path); err != nil {
		return fmt.Errorf("validate installed session (original at %s): %w", backup, err)
	}
	return os.Remove(backup)
}

func rewriteV1(ctx context.Context, source, destination string, output io.Writer) error {
	f, err := os.Open(source)
	if err != nil {
		return err
	}
	defer f.Close()
	reader := bufio.NewReader(f)
	writer := bufio.NewWriter(output)
	lineNumber := 0
	for {
		if err := ctx.Err(); err != nil {
			return err
		}
		line, readErr := reader.ReadBytes('\n')
		if len(line) == 0 && errors.Is(readErr, io.EOF) {
			break
		}
		if readErr != nil && !errors.Is(readErr, io.EOF) {
			return readErr
		}
		lineNumber++
		line = bytes.TrimSpace(line)
		if len(line) == 0 {
			continue
		}
		converted, err := convertV1Line(destination, line, lineNumber == 1)
		if err != nil {
			// v0.1.1 repaired an incomplete final JSONL record on open.
			if lineNumber > 1 {
				_, peekErr := reader.Peek(1)
				if errors.Is(peekErr, io.EOF) && !json.Valid(line) {
					break
				}
			}
			return fmt.Errorf("line %d: %w", lineNumber, err)
		}
		if _, err := writer.Write(converted); err != nil {
			return err
		}
		if err := writer.WriteByte('\n'); err != nil {
			return err
		}
		if errors.Is(readErr, io.EOF) {
			break
		}
	}
	return writer.Flush()
}

func convertV1Line(path string, line []byte, header bool) ([]byte, error) {
	var record map[string]json.RawMessage
	if err := json.Unmarshal(line, &record); err != nil {
		return nil, err
	}
	if header {
		version, err := readInt(record["version"])
		if err != nil || version != 1 {
			return nil, errors.New("expected v1 session header")
		}
		record["version"] = []byte("4")
		return json.Marshal(record)
	}
	var kind string
	if err := json.Unmarshal(record["type"], &kind); err != nil {
		return nil, err
	}
	switch kind {
	case "message":
		message, err := convertV1Message(path, record["message"])
		if err != nil {
			return nil, err
		}
		record["message"] = message
	case "model_change":
		model, err := convertV1Model(record["model"])
		if err != nil {
			return nil, err
		}
		record["model"] = model
	}
	return json.Marshal(record)
}

func readInt(raw json.RawMessage) (int, error) {
	var value int
	err := json.Unmarshal(raw, &value)
	return value, err
}

func convertV1Message(path string, raw json.RawMessage) (json.RawMessage, error) {
	var old struct {
		Role       session.Role       `json:"role"`
		Content    string             `json:"content"`
		Parts      []json.RawMessage  `json:"parts"`
		ToolCalls  []session.ToolCall `json:"tool_calls"`
		ToolCallID string             `json:"tool_call_id"`
		Name       string             `json:"name"`
	}
	if err := json.Unmarshal(raw, &old); err != nil {
		return nil, err
	}
	var message map[string]json.RawMessage
	if err := json.Unmarshal(raw, &message); err != nil {
		return nil, err
	}
	parts := old.Parts
	for i, rawPart := range parts {
		var part struct {
			Type string `json:"type"`
			Text string `json:"text"`
		}
		if err := json.Unmarshal(rawPart, &part); err != nil {
			return nil, err
		}
		if part.Type != session.PartImage {
			continue
		}
		hash, mime, err := saveLegacyImage(path, part.Text)
		if err != nil {
			return nil, err
		}
		var fields map[string]json.RawMessage
		if err := json.Unmarshal(rawPart, &fields); err != nil {
			return nil, err
		}
		delete(fields, "text")
		fields["image_hash"], _ = json.Marshal(hash)
		fields["image_mime"], _ = json.Marshal(mime)
		parts[i], _ = json.Marshal(fields)
	}
	switch old.Role {
	case session.RoleSystem, session.RoleUser:
		if !hasPart(parts, session.PartText) {
			parts = append([]json.RawMessage{mustJSON(session.Part{Type: session.PartText, Text: old.Content})}, parts...)
		}
	case session.RoleAssistant:
		if old.Content != "" && !hasPart(parts, session.PartText) {
			parts = append(parts, mustJSON(session.Part{Type: session.PartText, Text: old.Content}))
		}
		if !hasPart(parts, session.PartToolCall) {
			for _, call := range old.ToolCalls {
				parts = append(parts, mustJSON(session.Part{
					Type: session.PartToolCall, ToolCallID: call.ID, ToolName: call.Function.Name,
					ToolInput: call.Function.Arguments, ProviderOptions: call.Metadata,
				}))
			}
		}
	case session.RoleTool:
		if !hasPart(parts, session.PartToolResult) {
			result := session.Part{Type: session.PartToolResult, ToolCallID: typedid.ExternalToolCallID(old.ToolCallID), ToolName: old.Name, ToolOutput: old.Content}
			parts = append([]json.RawMessage{mustJSON(result)}, parts...)
		}
	default:
		return nil, fmt.Errorf("unknown message role %q", old.Role)
	}
	message["parts"] = mustJSON(parts)
	delete(message, "content")
	delete(message, "tool_calls")
	delete(message, "tool_call_id")
	delete(message, "name")
	return json.Marshal(message)
}

func hasPart(parts []json.RawMessage, kind string) bool {
	for _, raw := range parts {
		var part struct {
			Type string `json:"type"`
		}
		if json.Unmarshal(raw, &part) == nil && part.Type == kind {
			return true
		}
	}
	return false
}

func mustJSON(value any) json.RawMessage {
	b, _ := json.Marshal(value)
	return b
}

func convertV1Model(raw json.RawMessage) (json.RawMessage, error) {
	var old struct {
		Name       string `json:"name"`
		Provider   string `json:"provider"`
		ExternalID string `json:"external_id"`
	}
	if err := json.Unmarshal(raw, &old); err != nil {
		return nil, err
	}
	if old.Name == "" || old.Provider == "" || old.ExternalID == "" {
		return nil, errors.New("incomplete v1 model selection")
	}
	var model map[string]json.RawMessage
	if err := json.Unmarshal(raw, &model); err != nil {
		return nil, err
	}
	model["wire_format"] = mustJSON(old.Provider)
	delete(model, "provider")
	if connectionID, modelID, ok := strings.Cut(old.Name, "/"); ok && connectionID != "" && modelID == old.ExternalID {
		model["connection_id"] = mustJSON(connectionID)
	}
	return json.Marshal(model)
}

func saveLegacyImage(path, uri string) (string, string, error) {
	header, encoded, ok := strings.Cut(uri, ",")
	if !ok || !strings.HasPrefix(header, "data:image/") || !strings.HasSuffix(header, ";base64") {
		return "", "", errors.New("invalid v1 image data URI")
	}
	mime := strings.TrimSuffix(strings.TrimPrefix(header, "data:"), ";base64")
	data, err := base64.StdEncoding.DecodeString(encoded)
	if err != nil || len(data) == 0 || len(data) > 20<<20 {
		return "", "", errors.New("invalid or oversized v1 image")
	}
	digest := sha256.Sum256(data)
	hash := hex.EncodeToString(digest[:])
	dir := path + ".blobs"
	if err := os.MkdirAll(dir, 0o700); err != nil {
		return "", "", err
	}
	blob := filepath.Join(dir, hash)
	if existing, err := os.ReadFile(blob); err == nil {
		if !bytes.Equal(existing, data) {
			return "", "", fmt.Errorf("image blob %s differs from its hash", blob)
		}
		return hash, mime, nil
	} else if !errors.Is(err, os.ErrNotExist) {
		return "", "", err
	}
	tmp, err := os.CreateTemp(dir, ".blob-*")
	if err != nil {
		return "", "", err
	}
	defer os.Remove(tmp.Name())
	if err := tmp.Chmod(0o600); err != nil {
		_ = tmp.Close()
		return "", "", err
	}
	if _, err := tmp.Write(data); err != nil {
		_ = tmp.Close()
		return "", "", err
	}
	if err := tmp.Sync(); err != nil {
		_ = tmp.Close()
		return "", "", err
	}
	if err := tmp.Close(); err != nil {
		return "", "", err
	}
	if err := os.Rename(tmp.Name(), blob); err != nil {
		return "", "", err
	}
	return hash, mime, nil
}
