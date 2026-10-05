package acp

import (
	"encoding/base64"
	"fmt"
	"net/url"
	"path/filepath"
	"strconv"
	"strings"
	"unicode"

	"kon.kitsu.red/core/acp"
	"kon.kitsu.red/core/agent"
	"kon.kitsu.red/core/tool"
)

// convertPrompt turns a prompt's content blocks into one user message. Text
// and mentions are joined with nothing between them, since a client may
// split one sentence around a mention. Embedded text follows the prompt, so
// the sentence it was mentioned in stays whole.
func (s *liveSession) convertPrompt(blocks []acp.ContentBlock) (agent.Prompt, error) {
	var text, attached strings.Builder
	var prompt agent.Prompt
	for i, block := range blocks {
		switch block.Type {
		case "text":
			text.WriteString(block.Text)
		case "resource_link":
			text.WriteString(s.mention(block.URI))
		case "image", "audio":
			media, err := decodeMedia(block.Data, block.MIMEType)
			if err != nil {
				return agent.Prompt{}, invalidParams(fmt.Errorf("block %d: %w", i, err))
			}
			prompt.Media = append(prompt.Media, media)
		case "resource":
			r := block.Resource
			if r == nil {
				return agent.Prompt{}, invalidParams(fmt.Errorf("block %d: resource has no contents", i))
			}
			if r.Text == nil {
				media, err := decodeMedia(r.Blob, r.MIMEType)
				if err != nil {
					return agent.Prompt{}, invalidParams(fmt.Errorf("block %d: %w", i, err))
				}
				prompt.Media = append(prompt.Media, media)
				continue
			}
			mention := s.mention(r.URI)
			text.WriteString(mention)
			fmt.Fprintf(&attached, "\n\n%s\n%s", mention, fence(*r.Text))
		default:
			return agent.Prompt{}, invalidParams(fmt.Errorf("block %d: unsupported content type %q", i, block.Type))
		}
	}
	prompt.Text = text.String() + attached.String()
	if strings.TrimSpace(prompt.Text) == "" && len(prompt.Media) == 0 {
		return agent.Prompt{}, &acp.Error{Code: acp.CodeInvalidParams, Message: "empty prompt"}
	}
	return prompt, nil
}

func decodeMedia(data, mime string) (tool.Media, error) {
	if mime == "" {
		return tool.Media{}, fmt.Errorf("attachment has no MIME type")
	}
	b, err := base64.StdEncoding.DecodeString(data)
	if err != nil {
		return tool.Media{}, fmt.Errorf("attachment is not base64: %w", err)
	}
	if len(b) == 0 {
		return tool.Media{}, fmt.Errorf("attachment is empty")
	}
	return tool.Media{Data: b, MIME: mime}, nil
}

// mention writes a resource as the @-mention the full-screen UI inserts for
// a file: relative to the session's directory when inside it, and quoted
// when the path would otherwise end early. A URI that is not a file is
// mentioned as it is.
func (s *liveSession) mention(uri string) string {
	path := uri
	if u, err := url.Parse(uri); err == nil && u.Scheme == "file" {
		path = filepath.FromSlash(u.Path)
		if rel, err := filepath.Rel(s.cwd, path); err == nil && rel != ".." && !strings.HasPrefix(rel, ".."+string(filepath.Separator)) {
			path = rel
		}
	}
	if strings.ContainsAny(path, "\"\\`()[]{},;") || strings.ContainsFunc(path, unicode.IsSpace) {
		return "@" + strconv.Quote(path)
	}
	return "@" + path
}

// fence wraps text in a code fence longer than any backtick run inside it.
func fence(text string) string {
	longest, run := 0, 0
	for _, r := range text {
		if r == '`' {
			run++
			longest = max(longest, run)
		} else {
			run = 0
		}
	}
	marks := strings.Repeat("`", max(3, longest+1))
	return marks + "\n" + strings.TrimSuffix(text, "\n") + "\n" + marks
}
