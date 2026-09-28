// Package web fetches web pages as text a model can read. The agent reaches
// it through `kon tool webfetch` from its shell, so it prints for a model and
// owns no tool schema or agent state.
package web

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"io"
	"mime"
	"net/http"
	"net/url"
	"strings"
	"time"

	"golang.org/x/net/html"

	"github.com/hizkifw/kon/internal/buildinfo"
)

// MaxBodyBytes bounds how much of a response kon reads. A page's Markdown is
// a fraction of its HTML, and the rare larger page is still worth reading in
// part.
const MaxBodyBytes = 5 << 20

// fetchTimeout bounds a whole fetch, redirects and body included.
const fetchTimeout = 30 * time.Second

var client = &http.Client{Timeout: fetchTimeout}

// Page is one fetched URL as text.
type Page struct {
	// Text is Markdown for an HTML page and the body as it arrived for any
	// other text.
	Text string
	// Truncated reports that the body was longer than MaxBodyBytes, so Text
	// covers only its beginning.
	Truncated bool
	// RedirectedTo is the URL the server sent the request on to, if it did.
	// Paths in Text start from its site, which may not be the one asked for.
	RedirectedTo string
}

// Fetch GETs rawURL and returns it as text: HTML becomes Markdown, and other
// text passes through. A URL without a scheme is fetched over https. A
// response that is not text, or an HTML page with no text until a script
// builds it, is an error the caller can act on.
func Fetch(ctx context.Context, rawURL string) (Page, error) {
	target, err := parseURL(rawURL)
	if err != nil {
		return Page{}, err
	}
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, target.String(), nil)
	if err != nil {
		return Page{}, err
	}
	req.Header.Set("User-Agent", buildinfo.UserAgent())
	// Some sites serve Markdown to a client that asks for it, which is the
	// page as its author wrote it.
	req.Header.Set("Accept", "text/markdown, text/html;q=0.9, text/plain;q=0.8, */*;q=0.5")
	resp, err := client.Do(req)
	if err != nil {
		return Page{}, err
	}
	defer resp.Body.Close()
	if resp.StatusCode >= 400 {
		return Page{}, fmt.Errorf("GET %s: %s", resp.Request.URL, resp.Status)
	}
	body, err := io.ReadAll(io.LimitReader(resp.Body, MaxBodyBytes+1))
	if err != nil {
		return Page{}, fmt.Errorf("read %s: %w", resp.Request.URL, err)
	}
	page := Page{Truncated: len(body) > MaxBodyBytes}
	if page.Truncated {
		body = body[:MaxBodyBytes]
	}
	if final := resp.Request.URL.String(); final != target.String() {
		page.RedirectedTo = final
	}

	switch mediaType := mediaType(resp.Header.Get("Content-Type"), body); {
	case mediaType == "text/html" || mediaType == "application/xhtml+xml":
		doc, err := html.Parse(bytes.NewReader(body))
		if err != nil {
			return Page{}, fmt.Errorf("parse %s: %w", resp.Request.URL, err)
		}
		page.Text = Markdown(doc, resp.Request.URL)
		if page.Text == "" {
			return Page{}, fmt.Errorf("%s has no text in its HTML; the page may build its content with JavaScript", resp.Request.URL)
		}
	case textual(mediaType):
		page.Text = string(body)
	default:
		return Page{}, fmt.Errorf("%s is %s, not text; download it to a file instead", resp.Request.URL, mediaType)
	}
	// kon reads every page as UTF-8, which nearly every page is today; bytes
	// that are not, or a character cut at the size limit, become U+FFFD.
	page.Text = strings.ToValidUTF8(page.Text, "�")
	if !strings.HasSuffix(page.Text, "\n") {
		page.Text += "\n"
	}
	return page, nil
}

// parseURL accepts an http or https URL, taking one without a scheme as
// https, since a model often writes a bare "example.com/path".
func parseURL(raw string) (*url.URL, error) {
	raw = strings.TrimSpace(raw)
	if raw == "" {
		return nil, errors.New("URL must not be empty")
	}
	if !strings.Contains(raw, "://") {
		raw = "https://" + raw
	}
	u, err := url.Parse(raw)
	if err != nil {
		return nil, err
	}
	if (u.Scheme != "http" && u.Scheme != "https") || u.Host == "" {
		return nil, fmt.Errorf("%q is not an http or https URL", raw)
	}
	return u, nil
}

// mediaType is the response's declared media type, or the one its body
// sniffs as when the server named none or only a generic binary type, as
// servers often do for source files.
func mediaType(contentType string, body []byte) string {
	declared, _, err := mime.ParseMediaType(contentType)
	if err == nil && declared != "application/octet-stream" {
		return declared
	}
	sniffed, _, _ := mime.ParseMediaType(http.DetectContentType(body))
	return sniffed
}

// textual reports whether a media type other than HTML is text worth
// printing as it arrived.
func textual(mediaType string) bool {
	switch mediaType {
	case "application/json", "application/xml", "application/javascript",
		"application/yaml", "application/x-yaml", "application/toml":
		return true
	}
	return strings.HasPrefix(mediaType, "text/") ||
		strings.HasSuffix(mediaType, "+json") || strings.HasSuffix(mediaType, "+xml")
}
