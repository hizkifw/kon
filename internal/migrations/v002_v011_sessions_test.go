package migrations

import (
	"bytes"
	"context"
	"encoding/json"
	"image/png"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"testing"

	"kon.kitsu.red/core/session"
	"kon.kitsu.red/internal/config"
	"kon.kitsu.red/internal/migrate"
)

func testPaths(root string) config.Paths {
	return config.Paths{DataDir: root, Sessions: filepath.Join(root, "sessions")}
}

func storageVersion(t *testing.T, paths config.Paths) int {
	t.Helper()
	b, err := os.ReadFile(filepath.Join(paths.DataDir, "storage-version"))
	if os.IsNotExist(err) {
		return 0
	}
	if err != nil {
		t.Fatal(err)
	}
	lines := strings.Split(strings.TrimSpace(string(b)), "\n")
	version, err := strconv.Atoi(lines[len(lines)-1])
	if err != nil {
		t.Fatal(err)
	}
	return version
}

// This fixture was written by the session.Store at tag v0.1.1. It includes a
// model change, an assistant tool call, an embedded image, and a compaction.
func copyV011Fixture(t *testing.T, pathsRoot string) string {
	t.Helper()
	b, err := os.ReadFile(filepath.Join("testdata", "v011", "session.jsonl"))
	if err != nil {
		t.Fatal(err)
	}
	path := filepath.Join(pathsRoot, "sessions", "workspace", "session.jsonl")
	if err := os.MkdirAll(filepath.Dir(path), 0o700); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(path, b, 0o600); err != nil {
		t.Fatal(err)
	}
	return path
}

func TestV011SessionMigratesToV4(t *testing.T) {
	paths := testPaths(t.TempDir())
	path := copyV011Fixture(t, paths.DataDir)
	g, err := migrate.Enter(context.Background(), paths, Ordered(), nil)
	if err != nil {
		t.Fatal(err)
	}
	if err := g.Close(); err != nil {
		t.Fatal(err)
	}
	version := storageVersion(t, paths)
	if version != len(Ordered()) {
		t.Fatalf("storage version = %d", version)
	}
	if _, err := os.Stat(path + legacyBackupSuffix); !os.IsNotExist(err) {
		t.Fatalf("migration backup remains: %v", err)
	}
	store, err := session.Open(path)
	if err != nil {
		t.Fatal(err)
	}
	defer store.Close()
	entries := store.ActivePath()
	if len(entries) != 7 {
		t.Fatalf("entries = %d, want 7", len(entries))
	}
	if got := entries[0].Message.Text(); got != "stable system prompt\nsecond line" {
		t.Fatalf("system prompt changed: %q", got)
	}
	if model := entries[1].Model; model == nil || model.WireFormat != "openai-compatible" || model.ConnectionID != "work" || model.ExternalID.String() != "gpt-test" {
		t.Fatalf("model change = %#v", model)
	}
	assistant := entries[3].Message
	if assistant.Text() != "I will read it." || len(assistant.ToolCalls()) != 1 || assistant.Parts[0].Type != session.PartReasoning {
		t.Fatalf("assistant = %#v", assistant)
	}
	tool := entries[4].Message
	if tool.Text() != "loaded image" || len(tool.Parts) != 2 || tool.Parts[1].Type != session.PartImage {
		t.Fatalf("tool result = %#v", tool)
	}
	imageBytes, err := store.ReadImage(tool.Parts[1].ImageHash)
	if err != nil {
		t.Fatal(err)
	}
	image, err := png.Decode(bytes.NewReader(imageBytes))
	if err != nil || image.Bounds().Dx() != 1 || image.Bounds().Dy() != 1 {
		t.Fatalf("migrated image = %v, %v", image, err)
	}
	if entries[5].Summary != "earlier work summarized" || entries[6].Message.Text() != "continue" {
		t.Fatal("compaction or later message changed")
	}
	first, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	g, err = migrate.Enter(context.Background(), paths, Ordered(), nil)
	if err != nil {
		t.Fatal(err)
	}
	if err := g.Close(); err != nil {
		t.Fatal(err)
	}
	second, err := os.ReadFile(path)
	if err != nil || !bytes.Equal(first, second) {
		t.Fatalf("second startup rewrote migrated session: %v", err)
	}
}

func TestV011MigrationResumesAfterFirstRename(t *testing.T) {
	paths := testPaths(t.TempDir())
	path := copyV011Fixture(t, paths.DataDir)
	staleTemp := filepath.Join(filepath.Dir(path), ".v011-crashed")
	if err := os.WriteFile(staleTemp, []byte("partial"), 0o600); err != nil {
		t.Fatal(err)
	}
	if err := os.Rename(path, path+legacyBackupSuffix); err != nil {
		t.Fatal(err)
	}
	g, err := migrate.Enter(context.Background(), paths, Ordered(), nil)
	if err != nil {
		t.Fatal(err)
	}
	if err := g.Close(); err != nil {
		t.Fatal(err)
	}
	if _, err := session.OpenView(path); err != nil {
		t.Fatal(err)
	}
	if _, err := os.Stat(path + legacyBackupSuffix); !os.IsNotExist(err) {
		t.Fatalf("migration backup remains: %v", err)
	}
	if _, err := os.Stat(staleTemp); !os.IsNotExist(err) {
		t.Fatalf("stale migration file remains: %v", err)
	}
}

// v4Session was written by the session.Store when v4 was the current format.
// It is a literal, not written through session.New, because a later format
// would not be what an unmarked installation held beside its v0.1.1 files.
const v4Session = `{"type":"session","version":4,"id":"ses_Q2wPz8LkT4mVn1RbX7cY","app_version":"v0.2.0","timestamp":"2026-09-24T10:00:00Z","cwd":"/work"}
{"type":"message","id":"ent_A1b2C3d4E5f6G7h8I9j0","parent_id":null,"timestamp":"2026-09-24T10:00:00Z","message":{"role":"system","parts":[{"type":"text","text":"new system prompt"}]}}
{"type":"message","id":"ent_K1l2M3n4O5p6Q7r8S9t0","parent_id":"ent_A1b2C3d4E5f6G7h8I9j0","timestamp":"2026-09-24T10:00:01Z","message":{"role":"user","parts":[{"type":"text","text":"new session"}]}}
`

func TestV011MigrationKeepsExistingV4Session(t *testing.T) {
	paths := testPaths(t.TempDir())
	copyV011Fixture(t, paths.DataDir)
	currentPath := filepath.Join(paths.Sessions, "workspace", "current.jsonl")
	if err := os.WriteFile(currentPath, []byte(v4Session), 0o600); err != nil {
		t.Fatal(err)
	}
	// The previous framework release recorded version 1 for v4 sessions.
	if err := os.WriteFile(filepath.Join(paths.DataDir, "storage-version"), []byte("1\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	if err := (v011SessionsV2{}).Run(context.Background(), paths); err != nil {
		t.Fatal(err)
	}
	after, err := os.ReadFile(currentPath)
	if err != nil || string(after) != v4Session {
		t.Fatalf("current session changed: %v", err)
	}
}

// Step 2 must produce exactly v4, whatever the current session format is.
func TestV011StepWritesV4(t *testing.T) {
	paths := testPaths(t.TempDir())
	path := copyV011Fixture(t, paths.DataDir)
	if err := (v011SessionsV2{}).Run(context.Background(), paths); err != nil {
		t.Fatal(err)
	}
	if err := validateV4File(path); err != nil {
		t.Fatal(err)
	}
	version, err := readSessionVersion(path)
	if err != nil || version != sessionV4 {
		t.Fatalf("version = %d, %v", version, err)
	}
}

func TestV4ValidationRejectsBrokenSessions(t *testing.T) {
	for name, content := range map[string]string{
		"later version":  strings.Replace(v4Session, `"version":4`, `"version":5`, 1),
		"missing parent": strings.Replace(v4Session, `"parent_id":"ent_A1b2C3d4E5f6G7h8I9j0"`, `"parent_id":"ent_Z1b2C3d4E5f6G7h8I9j0"`, 1),
		"empty user":     strings.Replace(v4Session, `"text":"new session"`, `"text":""`, 1),
	} {
		path := filepath.Join(t.TempDir(), "session.jsonl")
		if err := os.WriteFile(path, []byte(content), 0o600); err != nil {
			t.Fatal(err)
		}
		if validateV4File(path) == nil {
			t.Errorf("%s: accepted", name)
		}
	}
}

// A crash right after a session file is created leaves it without a header.
// No kon can open it, so it must not stop the upgrade either.
func TestSessionWithoutHeaderIsSkipped(t *testing.T) {
	paths := testPaths(t.TempDir())
	path := copyV011Fixture(t, paths.DataDir)
	dir := filepath.Dir(path)
	for name, content := range map[string]string{"empty.jsonl": "", "torn.jsonl": `{"type":"sess`} {
		if err := os.WriteFile(filepath.Join(dir, name), []byte(content), 0o600); err != nil {
			t.Fatal(err)
		}
	}
	g, err := migrate.Enter(context.Background(), paths, Ordered(), nil)
	if err != nil {
		t.Fatal(err)
	}
	if err := g.Close(); err != nil {
		t.Fatal(err)
	}
	if _, err := session.OpenView(path); err != nil {
		t.Fatal(err)
	}
	if got := readFile(t, filepath.Join(dir, "torn.jsonl")); got != `{"type":"sess` {
		t.Fatalf("headerless file changed: %q", got)
	}
}

func TestV011ExplicitModelHasNoConnection(t *testing.T) {
	b, err := convertV1Model([]byte(`{"name":"review","provider":"openai","external_id":"gpt-4o"}`))
	if err != nil {
		t.Fatal(err)
	}
	var model struct {
		WireFormat   string `json:"wire_format"`
		ConnectionID string `json:"connection_id"`
		ExternalID   string `json:"external_id"`
	}
	if err := json.Unmarshal(b, &model); err != nil {
		t.Fatal(err)
	}
	if model.WireFormat != "openai" || model.ConnectionID != "" || model.ExternalID != "gpt-4o" {
		t.Fatalf("converted model = %#v", model)
	}
}

func TestV011MigrationFailureKeepsOriginal(t *testing.T) {
	paths := testPaths(t.TempDir())
	path := copyV011Fixture(t, paths.DataDir)
	b, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	// Keep valid JSON while making the embedded image undecodable.
	b = bytes.Replace(b, []byte("data:image/png;base64,"), []byte("data:image/png;base64,!"), 1)
	if err := os.WriteFile(path, b, 0o600); err != nil {
		t.Fatal(err)
	}
	if _, err := migrate.Enter(context.Background(), paths, Ordered(), nil); err == nil || !strings.Contains(err.Error(), "invalid or oversized v1 image") {
		t.Fatalf("migration error = %v", err)
	}
	remaining, err := os.ReadFile(path)
	if err != nil || !bytes.Equal(b, remaining) {
		t.Fatalf("v1 source changed after failed conversion: %v", err)
	}
	if _, err := os.Stat(path + legacyBackupSuffix); !os.IsNotExist(err) {
		t.Fatalf("backup created before conversion completed: %v", err)
	}
	if version := storageVersion(t, paths); version != 1 {
		t.Fatalf("version after failed conversion = %d", version)
	}
}

func TestBaselineRejectsUnsupportedSession(t *testing.T) {
	paths := testPaths(t.TempDir())
	dir := filepath.Join(paths.Sessions, "workspace")
	if err := os.MkdirAll(dir, 0o700); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, "old.jsonl"), []byte("{\"type\":\"session\",\"version\":3}\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	_, err := migrate.Enter(context.Background(), paths, Ordered(), nil)
	if err == nil || !strings.Contains(err.Error(), "unsupported version 3") {
		t.Fatalf("Enter = %v", err)
	}
	if version := storageVersion(t, paths); version != 0 {
		t.Fatalf("version after rejected baseline = %d", version)
	}
}
