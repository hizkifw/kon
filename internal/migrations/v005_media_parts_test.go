package migrations

import (
	"context"
	"crypto/sha256"
	"encoding/hex"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"kon.kitsu.red/core/session"
	"kon.kitsu.red/internal/config"
)

// writeV4ImageSession writes a v4 session whose tool result holds an image,
// with the image's blob beside it, and returns its path and blob hash.
func writeV4ImageSession(t *testing.T, paths config.Paths) (string, string) {
	t.Helper()
	blob := []byte("png bytes")
	digest := sha256.Sum256(blob)
	hash := hex.EncodeToString(digest[:])
	path := filepath.Join(paths.Sessions, "workspace", "image.jsonl")
	if err := os.MkdirAll(path+".blobs", 0o700); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(path+".blobs", hash), blob, 0o600); err != nil {
		t.Fatal(err)
	}
	content := v4Session +
		`{"type":"message","id":"ent_U1v2W3x4Y5z6A7b8C9d0","parent_id":"ent_K1l2M3n4O5p6Q7r8S9t0","timestamp":"2026-09-24T10:00:02Z","message":{"role":"assistant","parts":[{"type":"tool_call","tool_call_id":"call_1","tool_name":"read","tool_input":{"path":"p.png"}}]}}` + "\n" +
		`{"type":"message","id":"ent_E1f2G3h4I5j6K7l8M9n0","parent_id":"ent_U1v2W3x4Y5z6A7b8C9d0","timestamp":"2026-09-24T10:00:03Z","message":{"role":"tool","parts":[{"type":"tool_result","tool_call_id":"call_1","tool_name":"read","tool_output":"loaded image p.png"},{"type":"image","image_hash":"` + hash + `","image_mime":"image/png"}]}}` + "\n"
	if err := os.WriteFile(path, []byte(content), 0o600); err != nil {
		t.Fatal(err)
	}
	return path, hash
}

func runMediaStep(t *testing.T, paths config.Paths) {
	t.Helper()
	if err := (mediaPartsV5{}).Run(context.Background(), paths); err != nil {
		t.Fatal(err)
	}
}

func TestV4ImagePartsBecomeMediaParts(t *testing.T) {
	paths := testPaths(t.TempDir())
	path, hash := writeV4ImageSession(t, paths)
	before := readFile(t, path)
	runMediaStep(t, paths)
	after := readFile(t, path)
	// Lines without an image are copied byte for byte, the root system prompt
	// above all: its bytes key the provider's prompt cache.
	beforeLines, afterLines := strings.Split(before, "\n"), strings.Split(after, "\n")
	for i := 1; i <= 3; i++ {
		if beforeLines[i] != afterLines[i] {
			t.Fatalf("line %d changed:\n%s\n%s", i+1, beforeLines[i], afterLines[i])
		}
	}
	store, err := session.Open(path)
	if err != nil {
		t.Fatal(err)
	}
	defer store.Close()
	entries := store.ActivePath()
	tool := entries[len(entries)-1].Message
	if len(tool.Parts) != 2 || tool.Parts[1].Type != session.PartMedia || tool.Parts[1].MediaHash != hash || tool.Parts[1].MediaMIME != "image/png" {
		t.Fatalf("tool result = %#v", tool)
	}
	if data, err := store.ReadMedia(hash); err != nil || string(data) != "png bytes" {
		t.Fatalf("blob = %q, %v", data, err)
	}
	runMediaStep(t, paths)
	if again := readFile(t, path); again != after {
		t.Fatal("a second run rewrote the converted session")
	}
}

func TestV4MediaMigrationResumesAfterFirstRename(t *testing.T) {
	paths := testPaths(t.TempDir())
	path, _ := writeV4ImageSession(t, paths)
	staleTemp := filepath.Join(filepath.Dir(path), v5TempFilePrefix+"crashed")
	if err := os.WriteFile(staleTemp, []byte("partial"), 0o600); err != nil {
		t.Fatal(err)
	}
	if err := os.Rename(path, path+v4BackupSuffix); err != nil {
		t.Fatal(err)
	}
	runMediaStep(t, paths)
	if _, err := session.OpenView(path); err != nil {
		t.Fatal(err)
	}
	for _, leftover := range []string{path + v4BackupSuffix, staleTemp} {
		if _, err := os.Stat(leftover); !os.IsNotExist(err) {
			t.Fatalf("%s remains: %v", leftover, err)
		}
	}
}

func TestV4MediaMigrationDropsTornTail(t *testing.T) {
	paths := testPaths(t.TempDir())
	path, _ := writeV4ImageSession(t, paths)
	f, err := os.OpenFile(path, os.O_APPEND|os.O_WRONLY, 0o600)
	if err != nil {
		t.Fatal(err)
	}
	if _, err := f.WriteString(`{"type":"message","id":"ent_`); err != nil {
		t.Fatal(err)
	}
	f.Close()
	runMediaStep(t, paths)
	if _, err := session.OpenView(path); err != nil {
		t.Fatal(err)
	}
}
