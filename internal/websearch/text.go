package websearch

import (
	"fmt"
	"html"
	"io"
	"strings"
	"time"
)

// maxSnippetBytes bounds one result's snippet. Some providers return whole
// passages, and a search is for choosing pages, which webfetch then reads.
const maxSnippetBytes = 500

// Write prints results for a model: a numbered title, then the URL, then the
// date and snippet when the provider gave them.
func Write(w io.Writer, results []Result) error {
	var out strings.Builder
	for i, result := range results {
		title := oneLine(result.Title)
		if title == "" {
			title = result.URL
		}
		fmt.Fprintf(&out, "%d. %s\n   %s\n", i+1, title, result.URL)
		detail := truncate(oneLine(result.Snippet), maxSnippetBytes)
		if result.Date != "" && detail != "" {
			detail = result.Date + " - " + detail
		} else if result.Date != "" {
			detail = result.Date
		}
		if detail != "" {
			out.WriteString("   " + detail + "\n")
		}
		if i < len(results)-1 {
			out.WriteString("\n")
		}
	}
	_, err := io.WriteString(w, strings.ToValidUTF8(out.String(), "�"))
	return err
}

// Plain turns a provider's HTML fragment into text: tags are dropped and
// entities decoded. Use it only for fields a provider documents as markup,
// since it would also drop a literal "<b>" from plain text.
func Plain(fragment string) string {
	var out strings.Builder
	inTag := false
	for _, r := range fragment {
		switch {
		case r == '<':
			inTag = true
		case r == '>' && inTag:
			inTag = false
		case !inTag:
			out.WriteRune(r)
		}
	}
	return oneLine(html.UnescapeString(out.String()))
}

// Day reduces a provider's timestamp to YYYY-MM-DD. It returns "" for
// anything that does not start with a date, such as "3 days ago".
func Day(timestamp string) string {
	timestamp = strings.TrimSpace(timestamp)
	if len(timestamp) < len(time.DateOnly) {
		return ""
	}
	day := timestamp[:len(time.DateOnly)]
	if _, err := time.Parse(time.DateOnly, day); err != nil {
		return ""
	}
	return day
}

func oneLine(text string) string {
	return strings.Join(strings.Fields(text), " ")
}

// truncate cuts text to at most limit bytes on a character boundary, marking
// the cut.
func truncate(text string, limit int) string {
	if len(text) <= limit {
		return text
	}
	cut := limit
	for cut > 0 && !isRuneStart(text[cut]) {
		cut--
	}
	return strings.TrimSpace(text[:cut]) + "…"
}

func isRuneStart(b byte) bool { return b&0xC0 != 0x80 }
