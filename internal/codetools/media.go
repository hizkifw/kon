package codetools

import (
	"bytes"
	"fmt"
	"slices"
	"strings"

	"kon.kitsu.red/core/session"
)

const (
	maxImageBytes = 5 * 1024 * 1024
	// maxMediaBytes bounds audio, video, and documents, which run larger
	// than images. It matches the most a session stores per attachment.
	maxMediaBytes = 20 * 1024 * 1024
)

// mediaKind is what the read tool says about, and allows of, one modality.
type mediaKind struct {
	noun    string // the file, as a notice names it
	formats string // the formats attachableMIME accepts
	limit   int    // the largest file attached
}

var mediaKinds = map[session.Modality]mediaKind{
	session.ModalityImage: {"an image", "png, jpeg, gif, or webp images", maxImageBytes},
	session.ModalityAudio: {"an audio file", "wav or mp3 audio", maxMediaBytes},
	session.ModalityVideo: {"a video", "mp4, mov, or webm video", maxMediaBytes},
	session.ModalityPDF:   {"a PDF document", "PDF documents", maxMediaBytes},
}

// formatsOf lists the formats kon attaches for the given modalities, in
// session.Modalities order, as one phrase.
func formatsOf(modalities []session.Modality) string {
	var formats []string
	for _, modality := range session.Modalities() {
		if slices.Contains(modalities, modality) {
			formats = append(formats, mediaKinds[modality].formats)
		}
	}
	if len(formats) > 1 {
		formats[len(formats)-1] = "or " + formats[len(formats)-1]
	}
	return strings.Join(formats, "; ")
}

// sniffMedia identifies media by its magic bytes. A format kon attaches has
// only its mime set; one it recognizes but cannot attach also has format, its
// human-readable name. Both are empty for anything else. Attachable formats
// are checked first: WebM is a Matroska file that kon can send.
func sniffMedia(b []byte) (mime, format string) {
	if mime := attachableMIME(b); mime != "" {
		return mime, ""
	}
	return unsupportedMIME(b)
}

// unsupported explains a media file kon cannot attach in terms of the active
// model: the formats to convert to when it accepts the file's modality, and
// otherwise what it does accept, so the model does not convert in vain.
func unsupported(path, mime, format string, inputs []session.Modality) error {
	modality := session.ModalityOf(mime)
	switch {
	case slices.Contains(inputs, modality):
		return fmt.Errorf("%s is a %s file; convert it to %s to attach it", path, format, mediaKinds[modality].formats)
	case formatsOf(inputs) == "":
		return fmt.Errorf("%s is a %s file, and the active model accepts no media input, so it cannot be attached", path, format)
	}
	return fmt.Errorf("%s is a %s file, and the active model does not accept %s input; it accepts %s", path, format, modality, formatsOf(inputs))
}

// attachableMIME identifies a format kon attaches from its magic bytes, or
// returns "" for anything else. The formats are the ones every wire format
// that carries the modality accepts; mediaKinds names them for messages.
func attachableMIME(b []byte) string {
	switch {
	case len(b) >= 8 && bytes.Equal(b[:8], []byte("\x89PNG\r\n\x1a\n")):
		return "image/png"
	case len(b) >= 3 && b[0] == 0xFF && b[1] == 0xD8 && b[2] == 0xFF:
		return "image/jpeg"
	case len(b) >= 6 && (bytes.Equal(b[:6], []byte("GIF87a")) || bytes.Equal(b[:6], []byte("GIF89a"))):
		return "image/gif"
	case riff(b, "WEBP"):
		return "image/webp"
	case riff(b, "WAVE"):
		return "audio/wav"
	case len(b) >= 3 && bytes.Equal(b[:3], []byte("ID3")):
		return "audio/mpeg"
	// An MPEG audio frame sync with layer III; JPEG's 0xFFD8 fails the
	// sync bits and AAC's layer bits are zero.
	case len(b) >= 2 && b[0] == 0xFF && b[1]&0xE0 == 0xE0 && b[1]&0x06 == 0x02:
		return "audio/mpeg"
	case ftyp(b, "isom", "iso2", "iso4", "iso5", "iso6", "mp41", "mp42", "avc1", "dash", "M4V ", "mmp4"):
		return "video/mp4"
	case ftyp(b, "qt  "):
		return "video/quicktime"
	case ebml(b) && bytes.Contains(b[:min(len(b), 64)], []byte("webm")):
		return "video/webm"
	case len(b) >= 5 && bytes.Equal(b[:5], []byte("%PDF-")):
		return "application/pdf"
	default:
		return ""
	}
}

// unsupportedMIME identifies common media formats kon cannot attach (BMP,
// TIFF, ICO, HEIF, and AVIF images; FLAC, Ogg, and M4A audio; AVI and
// Matroska video). format is a human-readable name for messages; both are
// empty for anything else.
func unsupportedMIME(b []byte) (mime, format string) {
	switch {
	case len(b) >= 2 && b[0] == 'B' && b[1] == 'M':
		return "image/bmp", "BMP image"
	case len(b) >= 4 && (bytes.Equal(b[:4], []byte("II*\x00")) || bytes.Equal(b[:4], []byte("MM\x00*"))):
		return "image/tiff", "TIFF image"
	case len(b) >= 4 && bytes.Equal(b[:4], []byte("\x00\x00\x01\x00")):
		return "image/x-icon", "ICO image"
	case ftyp(b, "heic", "heif", "mif1", "hevc"):
		return "image/heic", "HEIF image"
	case ftyp(b, "avif"):
		return "image/avif", "AVIF image"
	case len(b) >= 4 && bytes.Equal(b[:4], []byte("fLaC")):
		return "audio/flac", "FLAC audio"
	case len(b) >= 4 && bytes.Equal(b[:4], []byte("OggS")):
		return "audio/ogg", "Ogg"
	case ftyp(b, "M4A ", "M4B "):
		return "audio/mp4", "M4A audio"
	case riff(b, "AVI "):
		return "video/x-msvideo", "AVI video"
	case ebml(b):
		return "video/x-matroska", "Matroska video"
	default:
		return "", ""
	}
}

// riff reports whether b is a RIFF container of the given form type.
func riff(b []byte, form string) bool {
	return len(b) >= 12 && bytes.Equal(b[:4], []byte("RIFF")) && string(b[8:12]) == form
}

// ftyp reports whether b is an ISO base media file whose major brand is one
// of brands.
func ftyp(b []byte, brands ...string) bool {
	return len(b) >= 12 && string(b[4:8]) == "ftyp" && slices.Contains(brands, string(b[8:12]))
}

// ebml reports whether b starts an EBML document, the container of both
// Matroska and WebM.
func ebml(b []byte) bool {
	return len(b) >= 4 && bytes.Equal(b[:4], []byte("\x1a\x45\xdf\xa3"))
}
