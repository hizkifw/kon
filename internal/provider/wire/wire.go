// Package wire names the wire formats kon speaks and records what each one
// implies. A wire format is how kon talks to a server: the endpoint it
// defaults to and how requests encode reasoning. Which service answers is a
// separate question, the provider, identified by a connection's ID and its
// models.dev key; quirks of a particular service do not belong here.
//
// The package is a leaf so that config validates against the same table the
// backends in internal/provider read.
package wire

import (
	"errors"
	"slices"
	"strings"
)

// Format is a wire format, spelled as the config's "type" value.
type Format string

// Most formats below are dialects of OpenAI chat completions, differing in
// defaults and in how reasoning is encoded. OpenAI Responses and Anthropic
// are protocols of their own.
const (
	OpenAI           Format = "openai"
	OpenAICompatible Format = "openai-compatible"
	OpenRouter       Format = "openrouter"
	Ollama           Format = "ollama"
	OpenAIResponses  Format = "openai-responses"
	Anthropic        Format = "anthropic"
)

// Protocol is the request and response shape a format uses; each has its own
// backend in internal/provider.
type Protocol string

const (
	// ChatCompletions is OpenAI's POST /chat/completions.
	ChatCompletions Protocol = "chat_completions"
	// Responses is OpenAI's POST /responses.
	Responses Protocol = "responses"
	// Messages is Anthropic's POST /messages.
	Messages Protocol = "messages"
)

// anthropicVersion is the Messages API version kon is written against. It is
// sent on every request, as the API requires.
const anthropicVersion = "2023-06-01"

// Default is the format of a model profile that names none.
const Default = OpenAICompatible

// Spec is what a wire format implies for requests to any server speaking it.
type Spec struct {
	// Protocol selects the backend; the zero value is ChatCompletions.
	Protocol Protocol
	// DefaultBaseURL is the API root used when a connection names none. Empty
	// means the format has no canonical server, so a base URL is required.
	DefaultBaseURL string
	// APIPath is appended to a base URL that does not already end in it.
	// Ollama serves chat completions under /v1, beside its native API.
	APIPath string
	// NestedEffort sends reasoning effort as {"reasoning": {"effort": …}}
	// instead of the top-level reasoning_effort field.
	NestedEffort bool
	// ReasoningField is where replayed reasoning goes when a message did not
	// record the field it arrived in.
	ReasoningField string
	// ListingOptional accepts a server without GET /models: login saves the
	// connection as unverified instead of failing. Compatible servers vary.
	ListingOptional bool
}

var specs = map[Format]Spec{
	OpenAI:           {DefaultBaseURL: "https://api.openai.com/v1", ReasoningField: "reasoning_content"},
	OpenAICompatible: {ReasoningField: "reasoning_content", ListingOptional: true},
	OpenRouter:       {DefaultBaseURL: "https://openrouter.ai/api/v1", NestedEffort: true, ReasoningField: "reasoning"},
	Ollama:           {DefaultBaseURL: "http://localhost:11434", APIPath: "/v1", ReasoningField: "reasoning_content"},
	// Anthropic-compatible services do not all list models; a bad key still
	// fails the listing with 401 rather than 404.
	OpenAIResponses: {Protocol: Responses, DefaultBaseURL: "https://api.openai.com/v1"},
	Anthropic:       {Protocol: Messages, DefaultBaseURL: "https://api.anthropic.com/v1", ListingOptional: true},
}

// AuthHeaders are the headers that carry an API key, and any the protocol
// requires on every request. A chat completions server takes a bearer token;
// the Messages API takes x-api-key and a version.
func (s Spec) AuthHeaders(apiKey string) map[string]string {
	headers := map[string]string{}
	if s.Protocol == Messages {
		headers["anthropic-version"] = anthropicVersion
		if apiKey != "" {
			headers["x-api-key"] = apiKey
		}
		return headers
	}
	if apiKey != "" {
		headers["Authorization"] = "Bearer " + apiKey
	}
	return headers
}

// Lookup returns the spec for a format kon implements.
func Lookup(format Format) (Spec, bool) {
	spec, ok := specs[format]
	return spec, ok
}

// Formats lists every implemented format in a stable order, for messages.
func Formats() []Format {
	formats := make([]Format, 0, len(specs))
	for format := range specs {
		formats = append(formats, format)
	}
	slices.Sort(formats)
	return formats
}

// RequiresBaseURL reports whether a connection must name its own server.
func (s Spec) RequiresBaseURL() bool { return s.DefaultBaseURL == "" }

// BaseURL resolves the API root for a configured URL, which may be empty.
func (s Spec) BaseURL(configured string) (string, error) {
	base := strings.TrimRight(strings.TrimSpace(configured), "/")
	if base == "" {
		base = s.DefaultBaseURL
	}
	if base == "" {
		return "", errors.New("base URL is required")
	}
	if s.APIPath != "" && !strings.HasSuffix(base, s.APIPath) {
		base += s.APIPath
	}
	return base, nil
}
