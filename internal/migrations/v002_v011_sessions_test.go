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

	"github.com/hizkifw/kon/internal/config"
	"github.com/hizkifw/kon/internal/migrate"
	"github.com/hizkifw/kon/internal/session"
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
	if err := session.ValidateFile(path); err != nil {
		t.Fatal(err)
	}
	if _, err := os.Stat(path + legacyBackupSuffix); !os.IsNotExist(err) {
		t.Fatalf("migration backup remains: %v", err)
	}
	if _, err := os.Stat(staleTemp); !os.IsNotExist(err) {
		t.Fatalf("stale migration file remains: %v", err)
	}
}

func TestV011MigrationKeepsExistingV4Session(t *testing.T) {
	paths := testPaths(t.TempDir())
	copyV011Fixture(t, paths.DataDir)
	store, err := session.New(paths.Sessions, t.TempDir(), "current", "new system prompt")
	if err != nil {
		t.Fatal(err)
	}
	if _, err := store.AppendMessage(session.TextMessage(session.RoleUser, "new session")); err != nil {
		t.Fatal(err)
	}
	currentPath := store.Path()
	if err := store.Close(); err != nil {
		t.Fatal(err)
	}
	before, err := os.ReadFile(currentPath)
	if err != nil {
		t.Fatal(err)
	}
	// The previous framework release recorded version 1 for v4 sessions.
	if err := os.WriteFile(filepath.Join(paths.DataDir, "storage-version"), []byte("1\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	g, err := migrate.Enter(context.Background(), paths, Ordered(), nil)
	if err != nil {
		t.Fatal(err)
	}
	if err := g.Close(); err != nil {
		t.Fatal(err)
	}
	after, err := os.ReadFile(currentPath)
	if err != nil || !bytes.Equal(before, after) {
		t.Fatalf("current session changed: %v", err)
	}
}

func TestV011ExplicitModelHasNoConnection(t *testing.T) {
	b, err := convertV1Model([]byte(`{"name":"review","provider":"openai","external_id":"gpt-4o"}`))
	if err != nil {
		t.Fatal(err)
	}
	var model session.ModelSelection
	if err := json.Unmarshal(b, &model); err != nil {
		t.Fatal(err)
	}
	if model.WireFormat != "openai" || model.ConnectionID != "" || model.ExternalID.String() != "gpt-4o" {
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
