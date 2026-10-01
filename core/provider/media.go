package provider

import (
	"encoding/base64"
	"fmt"
	"slices"
	"strings"

	"kon.kitsu.red/core/session"
)

// mediaReader loads the attachments a request carries. inputs is what the
// model accepts and read is where the session keeps the bytes. A session
// keeps media read by an earlier model, and a /model switch must not send it
// to one that rejects it.
type mediaReader struct {
	inputs []session.Modality
	read   func(hash string) ([]byte, error)
}

// load returns a media part's bytes when the model accepts its modality and
// the protocol can carry it, which carried lists. Otherwise it returns the
// placeholder text that stands in for the part: failing the request instead
// would break every later turn, compaction included.
func (r mediaReader) load(part session.Part, carried ...session.Modality) ([]byte, string) {
	modality := part.Modality()
	if r.read == nil || !slices.Contains(carried, modality) || !slices.Contains(r.inputs, modality) {
		return nil, omittedText(modality)
	}
	data, err := r.read(part.MediaHash)
	if err != nil {
		return nil, unavailableText(modality)
	}
	return data, ""
}

// hasMedia reports whether a message holds any media part.
func hasMedia(message session.Message) bool {
	return slices.ContainsFunc(message.Parts, func(part session.Part) bool { return part.Type == session.PartMedia })
}

// Placeholders stand in for media the request cannot carry. They depend only
// on the modality, so a session renders the same bytes on every request,
// which keeps the cached prompt prefix intact.
func omittedText(modality session.Modality) string {
	label := mediaLabel(modality)
	return fmt.Sprintf("[%s omitted: the active model does not accept %s input]", label, label)
}

func unavailableText(modality session.Modality) string {
	return fmt.Sprintf("[%s unavailable: its stored copy could not be read]", mediaLabel(modality))
}

func mediaLabel(modality session.Modality) string {
	if modality == "" {
		return "attachment"
	}
	return string(modality)
}

func dataURI(mime string, data []byte) string {
	return "data:" + mime + ";base64," + base64.StdEncoding.EncodeToString(data)
}

// mediaFilename names a document for formats that require a filename. The
// session keeps no name, so the hash stands in: stable across requests, and
// distinct per document.
func mediaFilename(part session.Part) string {
	_, subtype, _ := strings.Cut(part.MediaMIME, "/")
	return part.MediaHash[:min(len(part.MediaHash), 16)] + "." + subtype
}
