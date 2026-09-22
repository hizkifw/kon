package tools

import (
	"context"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// pngBytes is a minimal valid 1×1 PNG (magic header + IHDR + IDAT + IEND).
var pngBytes = []byte{
	0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A, 0x00, 0x00, 0x00, 0x0D,
	0x49, 0x48, 0x44, 0x52, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01,
	0x08, 0x06, 0x00, 0x00, 0x00, 0x1F, 0x15, 0xC4, 0x89, 0x00, 0x00, 0x00,
	0x0D, 0x49, 0x44, 0x41, 0x54, 0x78, 0x9C, 0x62, 0x00, 0x01, 0x00, 0x00,
	0x05, 0x00, 0x01, 0x0D, 0x0A, 0x2D, 0xB4, 0x00, 0x00, 0x00, 0x00, 0x49,
	0x45, 0x4E, 0x44, 0xAE, 0x42, 0x60, 0x82,
}

func TestReadImageAttachesForVision(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, "shot.png")
	if err := os.WriteFile(path, pngBytes, 0o644); err != nil {
		t.Fatal(err)
	}
	executor := New(dir, true)
	result, failed := executor.Execute(context.Background(), "read", raw(map[string]any{"path": "shot.png"}))
	if failed {
		t.Fatalf("image read failed: %s", result.Content)
	}
	if len(result.Images) != 1 || string(result.Images[0].Data) != string(pngBytes) || result.Images[0].MIME != "image/png" {
		t.Fatalf("image = %#v", result.Images)
	}
	if !strings.Contains(result.Content, "shot.png") || strings.Contains(result.Content, "base64") {
		t.Fatalf("content should describe the image without inlining bytes: %q", result.Content)
	}
}

func TestReadImageWithoutVisionExplains(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, "shot.png")
	if err := os.WriteFile(path, pngBytes, 0o644); err != nil {
		t.Fatal(err)
	}
	result, failed := New(dir, false).Execute(context.Background(), "read", raw(map[string]any{"path": "shot.png"}))
	if failed {
		t.Fatalf("non-vision read failed: %s", result.Content)
	}
	if len(result.Images) != 0 {
		t.Fatalf("images attached without vision: %#v", result.Images)
	}
	if !strings.Contains(result.Content, "vision") {
		t.Fatalf("content does not explain the limitation: %q", result.Content)
	}
}

func TestReadImageRejectsSpoofedExtension(t *testing.T) {
	// A large non-image under an image extension now reads as text; the
	// content routing means there is no "spoofed extension" failure anymore.
	// The oversize text failure still applies through the text path.
	dir := t.TempDir()
	big := make([]byte, 2*1024*1024)
	for i := range big {
		big[i] = 'x'
	}
	if err := os.WriteFile(filepath.Join(dir, "notes.png"), big, 0o644); err != nil {
		t.Fatal(err)
	}
	result, failed := New(dir, true).Execute(context.Background(), "read", raw(map[string]any{"path": "notes.png"}))
	if !failed || !strings.Contains(result.Content, "larger than 1048576") {
		t.Fatalf("oversize text under an image extension = %q, failed=%v", result.Content, failed)
	}
}

// An image read by its real content must not hit the text-mode 1 MiB limit or
// the UTF-8 check, whatever the file is named.
func TestReadImageRoutesByContentNotExtension(t *testing.T) {
	dir := t.TempDir()
	for _, name := range []string{"screenshot.heic", "unnamed", "notes.png"} {
		path := filepath.Join(dir, name)
		if err := os.WriteFile(path, pngBytes, 0o644); err != nil {
			t.Fatal(err)
		}
		result, failed := New(dir, true).Execute(context.Background(), "read", raw(map[string]any{"path": name}))
		if failed || len(result.Images) != 1 {
			t.Fatalf("read %s = %q, failed=%v", name, result.Content, failed)
		}
	}
}

// Known-but-unsendable formats get a convert-it hint instead of the text-mode
// "not UTF-8" or byte-limit failure.
func TestReadUnsendableImageFormatExplains(t *testing.T) {
	dir := t.TempDir()
	big := append([]byte("BM"), make([]byte, 2*1024*1024)...) // 2 MiB BMP
	if err := os.WriteFile(filepath.Join(dir, "shot.bmp"), big, 0o644); err != nil {
		t.Fatal(err)
	}
	result, failed := New(dir, true).Execute(context.Background(), "read", raw(map[string]any{"path": "shot.bmp"}))
	if !failed || !strings.Contains(result.Content, "BMP") || !strings.Contains(result.Content, "convert") {
		t.Fatalf("BMP read = %q, failed=%v", result.Content, failed)
	}
}

func TestReadImageRejectsOversize(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, "big.png")
	big := append([]byte(nil), pngBytes...)
	big = append(big, make([]byte, maxImageBytes+1)...)
	if err := os.WriteFile(path, big, 0o644); err != nil {
		t.Fatal(err)
	}
	result, failed := New(dir, true).Execute(context.Background(), "read", raw(map[string]any{"path": "big.png"}))
	if !failed || !strings.Contains(result.Content, "larger than") {
		t.Fatalf("oversize image = %q, failed=%v", result.Content, failed)
	}
	// The image cap (5 MB), not the text cap (1 MiB), must be the one enforced.
	if strings.Contains(result.Content, "1048576") {
		t.Fatalf("oversize image hit the text limit: %q", result.Content)
	}
}

// Text-mode limits and checks apply only to files that are not images by
// content: a text file named .png reads as text, and binary junk never
// reaches the UTF-8 check with a confusing message.
func TestReadTextFileWithImageExtensionReadsAsText(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, "notes.png")
	if err := os.WriteFile(path, []byte("just text in a png name\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	result, failed := New(dir, false).Execute(context.Background(), "read", raw(map[string]any{"path": "notes.png"}))
	if failed || !strings.Contains(result.Content, "just text") {
		t.Fatalf("text read = %q, failed=%v", result.Content, failed)
	}
}

func TestReadBinaryNonImageIsRejected(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, "data.bin")
	if err := os.WriteFile(path, []byte{0x00, 0x01, 0x02}, 0o644); err != nil {
		t.Fatal(err)
	}
	result, failed := New(dir, true).Execute(context.Background(), "read", raw(map[string]any{"path": "data.bin"}))
	if !failed || !strings.Contains(result.Content, "not UTF-8 text") {
		t.Fatalf("binary read = %q, failed=%v", result.Content, failed)
	}
}
