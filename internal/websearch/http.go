package websearch

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"net/url"
	"strings"
	"time"

	"kon.kitsu.red/internal/buildinfo"
)

// requestTimeout bounds a whole search, body included.
const requestTimeout = 30 * time.Second

// maxResponseBytes bounds a provider's response. A page of results is a few
// kilobytes of JSON; the limit only guards against a server that never stops.
const maxResponseBytes = 4 << 20

// maxErrorBytes is how much of a failed response an error quotes.
const maxErrorBytes = 300

var client = &http.Client{Timeout: requestTimeout}

// StatusError is a response the provider refused. Body is the start of what
// it said, which usually names the reason.
type StatusError struct {
	Code   int
	Status string
	Body   string
}

func (e *StatusError) Error() string {
	if e.Body == "" {
		return e.Status
	}
	return e.Status + ": " + e.Body
}

// Get requests path under the connection's base URL and returns the body.
// headers carry the provider's authentication; they may also replace kon's
// User-Agent. A status of 400 or above is a *StatusError.
func (c Connection) Get(ctx context.Context, path string, query url.Values, headers map[string]string) ([]byte, error) {
	return c.send(ctx, http.MethodGet, path, query, headers, nil)
}

// GetJSON is Get with the response decoded into out.
func (c Connection) GetJSON(ctx context.Context, path string, query url.Values, headers map[string]string, out any) error {
	body, err := c.Get(ctx, path, query, withAccept(headers))
	if err != nil {
		return err
	}
	return decode(body, out)
}

// PostJSON sends in as a JSON body to path and decodes the response into out.
func (c Connection) PostJSON(ctx context.Context, path string, headers map[string]string, in, out any) error {
	payload, err := json.Marshal(in)
	if err != nil {
		return fmt.Errorf("encode request: %w", err)
	}
	body, err := c.send(ctx, http.MethodPost, path, nil, withAccept(headers), payload)
	if err != nil {
		return err
	}
	return decode(body, out)
}

func (c Connection) send(ctx context.Context, method, path string, query url.Values, headers map[string]string, payload []byte) ([]byte, error) {
	target := c.BaseURL + path
	if len(query) > 0 {
		target += "?" + query.Encode()
	}
	var reader io.Reader
	if payload != nil {
		reader = bytes.NewReader(payload)
	}
	req, err := http.NewRequestWithContext(ctx, method, target, reader)
	if err != nil {
		return nil, err
	}
	req.Header.Set("User-Agent", buildinfo.UserAgent())
	if payload != nil {
		req.Header.Set("Content-Type", "application/json")
	}
	for name, value := range headers {
		req.Header.Set(name, value)
	}
	resp, err := client.Do(req)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()
	body, err := io.ReadAll(io.LimitReader(resp.Body, maxResponseBytes))
	if err != nil {
		return nil, fmt.Errorf("read response: %w", err)
	}
	if resp.StatusCode >= 400 {
		return nil, &StatusError{Code: resp.StatusCode, Status: resp.Status, Body: truncate(strings.Join(strings.Fields(string(body)), " "), maxErrorBytes)}
	}
	return body, nil
}

func withAccept(headers map[string]string) map[string]string {
	merged := map[string]string{"Accept": "application/json"}
	for name, value := range headers {
		merged[name] = value
	}
	return merged
}

func decode(body []byte, out any) error {
	if err := json.Unmarshal(body, out); err != nil {
		return fmt.Errorf("decode response: %w", err)
	}
	return nil
}
