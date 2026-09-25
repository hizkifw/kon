package provider

import (
	"encoding/json"
	"errors"
	"fmt"
	"net/http"
	"strconv"
	"strings"
)

// APIError is a provider-side failure: a non-2xx HTTP response, or an error
// object delivered inside a successful stream. Backends return it so kon can
// classify failures from the structured fields and fall back to the raw body.
type APIError struct {
	Status  int    // HTTP status; 0 for errors reported inside a stream
	Code    string // provider error code, e.g. "context_length_exceeded"
	Type    string // provider error type/category, e.g. "invalid_request_error"
	Message string // human-readable provider message
	Body    string // raw response, used when the error is not structured
}

func (e *APIError) Error() string {
	if e.Message != "" {
		return fmt.Sprintf("provider returned status %d: %s", e.Status, e.Message)
	}
	return fmt.Sprintf("provider returned status %d: %s", e.Status, strings.TrimSpace(e.Body))
}

// overflowMarkers are the phrasings providers use when a request exceeds the
// model's context window. They are matched as lowercase substrings against the
// code, type, message, and raw body.
var overflowMarkers = []string{
	"context_length_exceeded",
	"context length exceeded",
	"maximum context length",
	"context window",
	"prompt is too long",
	"too many tokens",
	"reduce the length",
	"exceed context limit",
}

// contextOverflow reports whether this error is a request-size rejection.
// Only request problems qualify: 400/413 responses, or in-stream rejections
// (Status 0). Other statuses mean auth, rate limits, or outages.
func (e *APIError) contextOverflow() bool {
	if e.Status != 0 && e.Status != http.StatusBadRequest && e.Status != http.StatusRequestEntityTooLarge {
		return false
	}
	for _, field := range []string{e.Code, e.Type, e.Message, e.Body} {
		haystack := strings.ToLower(field)
		for _, marker := range overflowMarkers {
			if strings.Contains(haystack, marker) {
				return true
			}
		}
	}
	return false
}

// IsContextOverflow reports whether err is a provider rejection because the
// conversation no longer fits the model's context window. The agent uses it
// to trigger emergency compaction and retry the turn.
func IsContextOverflow(err error) bool {
	var apiErr *APIError
	return errors.As(err, &apiErr) && apiErr.contextOverflow()
}

// parseAPIError builds an APIError from a failed HTTP response. Providers
// usually send {"error": {"message", "type", "code"}}; anything else is kept
// as the raw body.
func parseAPIError(status int, body []byte) *APIError {
	apiErr := &APIError{Status: status, Body: string(body)}
	var structured struct {
		Error *struct {
			Message string `json:"message"`
			Type    string `json:"type"`
			Code    any    `json:"code"`
		} `json:"error"`
	}
	if err := json.Unmarshal(body, &structured); err == nil && structured.Error != nil {
		apiErr.Message = structured.Error.Message
		apiErr.Type = structured.Error.Type
		switch code := structured.Error.Code.(type) {
		case string:
			apiErr.Code = code
		case float64:
			apiErr.Code = strconv.FormatFloat(code, 'f', -1, 64)
		}
	}
	return apiErr
}
