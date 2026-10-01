package codetools

import (
	"context"
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"testing"

	"kon.kitsu.red/core/session"
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

func TestReadImageAttaches(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, "shot.png")
	if err := os.WriteFile(path, pngBytes, 0o644); err != nil {
		t.Fatal(err)
	}
	executor := newExecutor(dir, allInputs, nil)
	result, failed := executor.Execute(context.Background(), "read", raw(map[string]any{"path": "shot.png"}), nil)
	if failed {
		t.Fatalf("image read failed: %s", result.Content)
	}
	if len(result.Media) != 1 || string(result.Media[0].Data) != string(pngBytes) || result.Media[0].MIME != "image/png" {
		t.Fatalf("image = %#v", result.Media)
	}
	if !strings.Contains(result.Content, "shot.png") || strings.Contains(result.Content, "base64") {
		t.Fatalf("content should describe the image without inlining bytes: %q", result.Content)
	}
}

func TestReadImageWithoutImageInputExplains(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, "shot.png")
	if err := os.WriteFile(path, pngBytes, 0o644); err != nil {
		t.Fatal(err)
	}
	result, failed := newExecutor(dir, nil, nil).Execute(context.Background(), "read", raw(map[string]any{"path": "shot.png"}), nil)
	if failed {
		t.Fatalf("read without image input failed: %s", result.Content)
	}
	if len(result.Media) != 0 {
		t.Fatalf("image attached without image input: %#v", result.Media)
	}
	if !strings.Contains(result.Content, "does not accept image input") {
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
	result, failed := newExecutor(dir, allInputs, nil).Execute(context.Background(), "read", raw(map[string]any{"path": "notes.png"}), nil)
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
		result, failed := newExecutor(dir, allInputs, nil).Execute(context.Background(), "read", raw(map[string]any{"path": name}), nil)
		if failed || len(result.Media) != 1 {
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
	result, failed := newExecutor(dir, allInputs, nil).Execute(context.Background(), "read", raw(map[string]any{"path": "shot.bmp"}), nil)
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
	result, failed := newExecutor(dir, allInputs, nil).Execute(context.Background(), "read", raw(map[string]any{"path": "big.png"}), nil)
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
	result, failed := newExecutor(dir, nil, nil).Execute(context.Background(), "read", raw(map[string]any{"path": "notes.png"}), nil)
	if failed || !strings.Contains(result.Content, "just text") {
		t.Fatalf("text read = %q, failed=%v", result.Content, failed)
	}
}

// An oversize file is refused from its size without being loaded, and a large
// unsupported image is still named as an image (from its prefix) rather than
// as an oversized text file.
func TestReadRejectsOversizeFromStatWithoutLoading(t *testing.T) {
	dir := t.TempDir()
	big := filepath.Join(dir, "big.txt")
	f, err := os.Create(big)
	if err != nil {
		t.Fatal(err)
	}
	chunk := make([]byte, 1<<20)
	for i := range chunk {
		chunk[i] = 'a'
	}
	for i := 0; i < 40; i++ {
		if _, err := f.Write(chunk); err != nil {
			t.Fatal(err)
		}
	}
	f.Close()

	var before, after runtime.MemStats
	runtime.GC()
	runtime.ReadMemStats(&before)
	result, failed := newExecutor(dir, allInputs, nil).Execute(context.Background(), "read", raw(map[string]any{"path": "big.txt"}), nil)
	runtime.ReadMemStats(&after)
	if !failed || !strings.Contains(result.Content, "larger than") {
		t.Fatalf("oversize read = %q, failed=%v", result.Content, failed)
	}
	// The whole file is 40 MiB; rejecting it must not load it. Allow generous
	// slack for GC noise while still catching a full read.
	if grew := after.TotalAlloc - before.TotalAlloc; grew > 10<<20 {
		t.Fatalf("oversize read allocated %d bytes; it should be refused from its size", grew)
	}

	// A large BMP is refused as an unsupported image, not as oversized text.
	bmp := filepath.Join(dir, "huge.bmp")
	if err := os.WriteFile(bmp, append([]byte("BM"), make([]byte, maxMediaBytes)...), 0o644); err != nil {
		t.Fatal(err)
	}
	result, failed = newExecutor(dir, allInputs, nil).Execute(context.Background(), "read", raw(map[string]any{"path": "huge.bmp"}), nil)
	if !failed || !strings.Contains(result.Content, "BMP") {
		t.Fatalf("oversize BMP read = %q, failed=%v", result.Content, failed)
	}
}

func TestReadBinaryNonImageIsRejected(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, "data.bin")
	if err := os.WriteFile(path, []byte{0x00, 0x01, 0x02}, 0o644); err != nil {
		t.Fatal(err)
	}
	result, failed := newExecutor(dir, allInputs, nil).Execute(context.Background(), "read", raw(map[string]any{"path": "data.bin"}), nil)
	if !failed || !strings.Contains(result.Content, "not UTF-8 text") {
		t.Fatalf("binary read = %q, failed=%v", result.Content, failed)
	}
}

// Each attachable format is recognized by its content and attached only for
// a model that accepts its modality.
func TestReadMediaAttachesByModality(t *testing.T) {
	mp4 := append([]byte("\x00\x00\x00\x18ftypisom"), make([]byte, 16)...)
	webm := append([]byte("\x1a\x45\xdf\xa3\x9f\x42\x86\x81\x01\x42\x82\x84webm"), make([]byte, 16)...)
	for _, test := range []struct {
		name     string
		data     []byte
		mime     string
		modality session.Modality
	}{
		{"clip.wav", []byte("RIFF\x24\x00\x00\x00WAVEfmt "), "audio/wav", session.ModalityAudio},
		{"song.mp3", []byte("ID3\x04\x00\x00\x00\x00\x00\x00"), "audio/mpeg", session.ModalityAudio},
		{"bare.mp3", []byte{0xFF, 0xFB, 0x90, 0x64, 0x00}, "audio/mpeg", session.ModalityAudio},
		{"demo.mp4", mp4, "video/mp4", session.ModalityVideo},
		{"demo.mov", []byte("\x00\x00\x00\x14ftypqt  \x00\x00\x00\x00"), "video/quicktime", session.ModalityVideo},
		{"demo.webm", webm, "video/webm", session.ModalityVideo},
		{"paper.pdf", []byte("%PDF-1.7\n%\xe2\xe3\xcf\xd3\n"), "application/pdf", session.ModalityPDF},
	} {
		dir := t.TempDir()
		if err := os.WriteFile(filepath.Join(dir, test.name), test.data, 0o644); err != nil {
			t.Fatal(err)
		}
		args := raw(map[string]any{"path": test.name})
		result, failed := newExecutor(dir, []session.Modality{test.modality}, nil).Execute(context.Background(), "read", args, nil)
		if failed || len(result.Media) != 1 || result.Media[0].MIME != test.mime {
			t.Fatalf("read %s = %q, media %#v, failed=%v", test.name, result.Content, result.Media, failed)
		}
		// An image-only model is told what the file is instead.
		result, failed = newExecutor(dir, []session.Modality{session.ModalityImage}, nil).Execute(context.Background(), "read", args, nil)
		if failed || len(result.Media) != 0 || !strings.Contains(result.Content, "does not accept "+string(test.modality)+" input") {
			t.Fatalf("read %s without %s input = %q, failed=%v", test.name, test.modality, result.Content, failed)
		}
	}
}

// Audio and video run larger than images, so they get the larger cap.
func TestReadMediaAllowsLargerAudioThanImages(t *testing.T) {
	dir := t.TempDir()
	wav := append([]byte("RIFF\x24\x00\x00\x00WAVEfmt "), make([]byte, maxImageBytes+1)...)
	if err := os.WriteFile(filepath.Join(dir, "long.wav"), wav, 0o644); err != nil {
		t.Fatal(err)
	}
	result, failed := newExecutor(dir, allInputs, nil).Execute(context.Background(), "read", raw(map[string]any{"path": "long.wav"}), nil)
	if failed || len(result.Media) != 1 {
		t.Fatalf("read long.wav = %q, failed=%v", result.Content, failed)
	}
}

// Known-but-unsendable audio and video get the same convert-it hint as
// unsendable images.
func TestReadUnsendableMediaFormatExplains(t *testing.T) {
	for name, data := range map[string][]byte{
		"song.flac": []byte("fLaC\x00\x00\x00\x22"),
		"voice.m4a": []byte("\x00\x00\x00\x20ftypM4A \x00\x00\x00\x00"),
		"film.mkv":  []byte("\x1a\x45\xdf\xa3\x9f\x42\x86\x81\x01\x42\x82\x88matroska"),
	} {
		dir := t.TempDir()
		if err := os.WriteFile(filepath.Join(dir, name), data, 0o644); err != nil {
			t.Fatal(err)
		}
		result, failed := newExecutor(dir, allInputs, nil).Execute(context.Background(), "read", raw(map[string]any{"path": name}), nil)
		if !failed || !strings.Contains(result.Content, "convert") {
			t.Fatalf("read %s = %q, failed=%v", name, result.Content, failed)
		}
	}
}

// The hint names what the active model accepts: formats to convert to when
// it takes the modality, and the modalities it does take otherwise.
func TestReadUnsendableMediaHintFollowsTheModel(t *testing.T) {
	dir := t.TempDir()
	if err := os.WriteFile(filepath.Join(dir, "song.flac"), []byte("fLaC\x00\x00\x00\x22"), 0o644); err != nil {
		t.Fatal(err)
	}
	args := raw(map[string]any{"path": "song.flac"})
	for _, test := range []struct {
		inputs []session.Modality
		want   string
	}{
		{[]session.Modality{session.ModalityAudio}, "convert it to wav or mp3 audio"},
		{[]session.Modality{session.ModalityImage, session.ModalityPDF}, "does not accept audio input; it accepts png, jpeg, gif, or webp images; or PDF documents"},
		{nil, "accepts no media input"},
	} {
		result, failed := newExecutor(dir, test.inputs, nil).Execute(context.Background(), "read", args, nil)
		if !failed || !strings.Contains(result.Content, test.want) {
			t.Fatalf("inputs %v: read = %q, failed=%v", test.inputs, result.Content, failed)
		}
	}
}

// The description is part of every request's cached prefix, so building it
// from mediaKinds must not change its bytes.
func TestReadDescriptionListsAttachableFormats(t *testing.T) {
	const want = "Read a UTF-8 text file with one-based line offsets, or load a media file whole for models that accept it: png, jpeg, gif, or webp images; wav or mp3 audio; mp4, mov, or webm video; or PDF documents."
	if got := (readTool{}).Definition().Description; got != want {
		t.Fatalf("description = %q", got)
	}
}

// WebM is a Matroska file kon can send, so an oversized one is named by its
// size limit rather than refused as unsupported Matroska.
func TestReadOversizeWebMIsNotMatroska(t *testing.T) {
	dir := t.TempDir()
	webm := append([]byte("\x1a\x45\xdf\xa3\x9f\x42\x86\x81\x01\x42\x82\x84webm"), make([]byte, maxMediaBytes)...)
	if err := os.WriteFile(filepath.Join(dir, "long.webm"), webm, 0o644); err != nil {
		t.Fatal(err)
	}
	result, failed := newExecutor(dir, allInputs, nil).Execute(context.Background(), "read", raw(map[string]any{"path": "long.webm"}), nil)
	if !failed || !strings.Contains(result.Content, "larger than") || strings.Contains(result.Content, "Matroska video file") {
		t.Fatalf("oversize webm = %q, failed=%v", result.Content, failed)
	}
}
