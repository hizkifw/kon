package login

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/http"
	"net/url"
	"slices"
	"strings"
	"time"

	"kon.kitsu.red/core/provider/wire"
	"kon.kitsu.red/internal/buildinfo"
	"kon.kitsu.red/internal/config"
)

const maxModelListSize = 8 << 20

// Model is one entry from a provider's model list. Nil InputModalities means
// the server did not report them; an empty list explicitly accepts no media.
type Model struct {
	ID              string   `json:"id"`
	InputModalities []string `json:"input_modalities"`
}

// Discover checks a connection on explicit login and returns its models.
// A server whose wire format makes the listing optional can still be saved
// without GET /models, but is reported as unverified.
func Discover(ctx context.Context, connection config.Provider) ([]Model, bool, error) {
	spec, ok := wire.Lookup(connection.Type)
	if !ok {
		return nil, false, fmt.Errorf("unsupported wire format %q", connection.Type)
	}
	baseURL, err := spec.BaseURL(connection.BaseURL)
	if err != nil {
		return nil, false, err
	}
	parsed, err := url.Parse(baseURL)
	if err != nil || (parsed.Scheme != "http" && parsed.Scheme != "https") || parsed.Host == "" {
		return nil, false, errors.New("base URL must be an HTTP or HTTPS URL")
	}
	client := &http.Client{
		Timeout: 15 * time.Second,
		// Credentials must not follow a server-directed redirect to another host.
		CheckRedirect: func(*http.Request, []*http.Request) error { return http.ErrUseLastResponse },
	}
	// OpenRouter the service, not every server speaking its format, exposes
	// /key, which rejects a bad key that the public model list would accept.
	if connection.Type == wire.OpenRouter && (connection.ID == "openrouter" || connection.CatalogProvider == "openrouter") {
		keyURL := strings.TrimRight(baseURL, "/") + "/key"
		if _, err := get(ctx, client, keyURL, spec, connection); err != nil {
			return nil, false, fmt.Errorf("verify OpenRouter key: %w", err)
		}
	}
	modelsURL := strings.TrimRight(baseURL, "/") + "/models"
	if spec.Protocol == wire.Messages {
		// The Messages API pages its model list, 20 to a page by default.
		modelsURL += "?limit=1000"
	}
	body, err := get(ctx, client, modelsURL, spec, connection)
	if err != nil {
		var status *httpStatusError
		if spec.ListingOptional && errors.As(err, &status) && (status.code == http.StatusNotFound || status.code == http.StatusMethodNotAllowed) {
			return nil, false, nil
		}
		return nil, false, fmt.Errorf("list provider models: %w", err)
	}
	var response struct {
		Data []struct {
			ID           string `json:"id"`
			Architecture struct {
				InputModalities []string `json:"input_modalities"`
			} `json:"architecture"`
		} `json:"data"`
	}
	if err := json.Unmarshal(body, &response); err != nil {
		return nil, false, fmt.Errorf("parse provider models: %w", err)
	}
	if response.Data == nil {
		return nil, false, errors.New("provider model list has no data array")
	}
	models := make([]Model, 0, len(response.Data))
	for _, model := range response.Data {
		if model.ID != "" {
			models = append(models, Model{ID: model.ID, InputModalities: model.Architecture.InputModalities})
		}
	}
	slices.SortStableFunc(models, func(a, b Model) int { return strings.Compare(a.ID, b.ID) })
	models = slices.CompactFunc(models, func(a, b Model) bool { return a.ID == b.ID })
	return models, true, nil
}

type httpStatusError struct{ code int }

func (e *httpStatusError) Error() string { return fmt.Sprintf("HTTP %d", e.code) }

func get(ctx context.Context, client *http.Client, endpoint string, spec wire.Spec, connection config.Provider) ([]byte, error) {
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, endpoint, nil)
	if err != nil {
		return nil, err
	}
	// Set before the configured headers so a connection can override it.
	req.Header.Set("User-Agent", buildinfo.UserAgent())
	for key, value := range spec.AuthHeaders(connection.APIKey) {
		req.Header.Set(key, value)
	}
	for key, value := range connection.Headers {
		req.Header.Set(key, value)
	}
	res, err := client.Do(req)
	if err != nil {
		return nil, err
	}
	defer res.Body.Close()
	if res.StatusCode != http.StatusOK {
		return nil, &httpStatusError{code: res.StatusCode}
	}
	body, err := io.ReadAll(io.LimitReader(res.Body, maxModelListSize+1))
	if err != nil {
		return nil, err
	}
	if len(body) > maxModelListSize {
		return nil, errors.New("model list exceeds size limit")
	}
	return body, nil
}
