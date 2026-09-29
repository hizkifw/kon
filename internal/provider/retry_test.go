package provider

import (
	"context"
	"errors"
	"fmt"
	"io"
	"net"
	"net/http"
	"net/url"
	"strings"
	"syscall"
	"testing"
	"time"

	"github.com/hizkifw/kon/internal/provider/wire"
	"github.com/hizkifw/kon/internal/session"
)

// fastRetries keeps the default number of retries with millisecond backoff.
var fastRetries = retryPolicy{retries: defaultRetryPolicy.retries, base: time.Millisecond, limit: 5 * time.Millisecond}

// failThen answers each request in turn with failures, then with ok.
func failThen(failures []func(http.ResponseWriter), ok func(http.ResponseWriter)) (http.HandlerFunc, *int) {
	requests := 0
	return func(w http.ResponseWriter, r *http.Request) {
		_, _ = io.Copy(io.Discard, r.Body)
		requests++
		if requests <= len(failures) {
			failures[requests-1](w)
			return
		}
		ok(w)
	}, &requests
}

func status(code int, header ...string) func(http.ResponseWriter) {
	return func(w http.ResponseWriter) {
		for i := 0; i+1 < len(header); i += 2 {
			w.Header().Set(header[i], header[i+1])
		}
		w.WriteHeader(code)
		_, _ = io.WriteString(w, `{"error":{"message":"failed","type":"error"}}`)
	}
}

func chatOK(w http.ResponseWriter) { _, _ = io.WriteString(w, okStream) }

func TestRetriesTransientFailures(t *testing.T) {
	always := func(fail func(http.ResponseWriter)) []func(http.ResponseWriter) {
		var failures []func(http.ResponseWriter)
		for range fastRetries.retries + 1 {
			failures = append(failures, fail)
		}
		return failures
	}
	for name, test := range map[string]struct {
		failures []func(http.ResponseWriter)
		requests int
		reasons  []string
		wantErr  bool
	}{
		"rate limit waits as asked": {failures: []func(http.ResponseWriter){status(429, "Retry-After-Ms", "1")}, requests: 2, reasons: []string{"rate limited (429)"}},
		"overload backs off":        {failures: []func(http.ResponseWriter){status(529), status(503)}, requests: 3, reasons: []string{"overloaded (529)", "overloaded (503)"}},
		"stream cut short":          {failures: []func(http.ResponseWriter){func(w http.ResponseWriter) { _, _ = io.WriteString(w, ": keep-alive\n\n") }}, requests: 2, reasons: []string{"connection lost"}},
		"bad request":               {failures: []func(http.ResponseWriter){status(400)}, requests: 1, wantErr: true},
		"server says no":            {failures: []func(http.ResponseWriter){status(500, "X-Should-Retry", "false")}, requests: 1, wantErr: true},
		"server says yes":           {failures: []func(http.ResponseWriter){status(400, "X-Should-Retry", "true")}, requests: 2, reasons: []string{"server error (400)"}},
		"wait too long":             {failures: []func(http.ResponseWriter){status(429, "Retry-After", "120")}, requests: 1, wantErr: true},
		"retries run out":           {failures: always(status(500)), requests: fastRetries.retries + 1, reasons: strings.Split(strings.Repeat("server error (500),", fastRetries.retries), ",")[:fastRetries.retries], wantErr: true},
	} {
		t.Run(name, func(t *testing.T) {
			handler, requests := failThen(test.failures, chatOK)
			model := newTestModel(t, handler)
			model.retry = fastRetries
			var reasons []string
			response, err := model.Stream(context.Background(), []session.Message{session.TextMessage(session.RoleUser, "hi")}, nil, func(event Event) {
				if event.Retry != nil {
					if event.Retry.Attempt != len(reasons)+1 || event.Retry.Max != fastRetries.retries {
						t.Errorf("retry = %+v after %d", event.Retry, len(reasons))
					}
					reasons = append(reasons, event.Retry.Reason)
				}
			})
			if *requests != test.requests || strings.Join(reasons, ",") != strings.Join(test.reasons, ",") {
				t.Fatalf("requests = %d, reasons = %q; want %d, %q", *requests, reasons, test.requests, test.reasons)
			}
			if test.wantErr != (err != nil) || (err == nil && response.Text() == "") {
				t.Fatalf("response = %q, err = %v", response.Text(), err)
			}
			// Running out of retries is said once the marker is gone, and a
			// failure that was never retried reads as it always did.
			var apiErr *APIError
			if err != nil && (!errors.As(err, &apiErr) || strings.HasPrefix(err.Error(), "gave up after") != (len(test.reasons) == fastRetries.retries)) {
				t.Fatalf("err = %v", err)
			}
		})
	}
}

// Once a response has begun streaming, a retry would repeat what the user
// already saw, so a failure after that point is returned as it is. Before it,
// an error inside the stream is retried like one in the status line.
func TestRetriesOnlyBeforeOutput(t *testing.T) {
	overloaded := sse(`{"type":"error","error":{"type":"overloaded_error","message":"Overloaded"}}`)
	ok := sse(`{"type":"content_block_start","index":0,"content_block":{"type":"text","text":"ok"}}`) + sse(`{"type":"message_stop"}`)
	for name, test := range map[string]struct {
		first    string
		requests int
		wantErr  bool
	}{
		"before output": {first: sse(`{"type":"message_start","message":{}}`) + overloaded, requests: 2},
		"after output":  {first: sse(`{"type":"content_block_start","index":0,"content_block":{"type":"text","text":"par"}}`) + overloaded, requests: 1, wantErr: true},
	} {
		t.Run(name, func(t *testing.T) {
			requests := 0
			model := newMessagesTestModel(t, func(w http.ResponseWriter, r *http.Request) {
				requests++
				if requests == 1 {
					_, _ = io.WriteString(w, test.first)
					return
				}
				_, _ = io.WriteString(w, ok)
			})
			model.retry = fastRetries
			_, err := model.Stream(context.Background(), []session.Message{session.TextMessage(session.RoleUser, "hi")}, nil, func(Event) {})
			if requests != test.requests || test.wantErr != (err != nil) {
				t.Fatalf("requests = %d, err = %v", requests, err)
			}
		})
	}
}

func TestRetryWaitStopsWithTheContext(t *testing.T) {
	model := newResponsesTestModel(t, func(w http.ResponseWriter, r *http.Request) {
		status(503, "Retry-After", "30")(w)
	})
	model.retry = fastRetries
	ctx, cancel := context.WithCancel(context.Background())
	start := time.Now()
	_, err := model.Stream(ctx, []session.Message{session.TextMessage(session.RoleUser, "hi")}, nil, func(event Event) {
		if event.Retry != nil {
			cancel()
		}
	})
	if !errors.Is(err, context.Canceled) || time.Since(start) > 5*time.Second {
		t.Fatalf("err = %v after %s", err, time.Since(start))
	}
	var apiErr *APIError
	if !errors.As(err, &apiErr) || apiErr.Status != 503 {
		t.Fatalf("the failure that was being retried is lost: %v", err)
	}
}

func TestRetryAfter(t *testing.T) {
	now := time.Date(2026, 9, 30, 12, 0, 0, 0, time.UTC)
	for _, test := range []struct {
		header http.Header
		want   time.Duration
	}{
		{http.Header{"Retry-After-Ms": {"1500"}, "Retry-After": {"9"}}, 1500 * time.Millisecond},
		{http.Header{"Retry-After": {"2.5"}}, 2500 * time.Millisecond},
		{http.Header{"Retry-After": {now.Add(20 * time.Second).Format(http.TimeFormat)}}, 20 * time.Second},
		{http.Header{"Retry-After": {now.Add(-time.Second).Format(http.TimeFormat)}}, 0},
		{http.Header{"Retry-After": {"soon"}}, 0},
		{http.Header{}, 0},
	} {
		if got := retryAfter(test.header, now); got != test.want {
			t.Errorf("retryAfter(%v) = %s, want %s", test.header, got, test.want)
		}
	}
}

func TestBackoffDoublesUpToTheLimit(t *testing.T) {
	policy := retryPolicy{retries: 99, base: time.Second, limit: 30 * time.Second}
	for retry, full := range map[int]time.Duration{1: time.Second, 2: 2 * time.Second, 5: 16 * time.Second, 6: 30 * time.Second, 64: 30 * time.Second} {
		for range 20 {
			if got := policy.backoff(retry); got > full || got < full*3/4 {
				t.Fatalf("backoff(%d) = %s, want within [%s, %s]", retry, got, full*3/4, full)
			}
		}
	}
}

func TestTransientConnectionErrors(t *testing.T) {
	wrap := func(err error) error {
		return fmt.Errorf("chat completions request: %w", &url.Error{Op: "Post", URL: "https://example.test", Err: err})
	}
	for err, want := range map[error]bool{
		wrap(&net.OpError{Op: "read", Err: syscall.ECONNRESET}): true,
		wrap(io.EOF): true,
		fmt.Errorf("read chat stream: %w", errStreamClosed):        true,
		wrap(&net.DNSError{Err: "timeout", IsTimeout: true}):       true,
		wrap(&net.OpError{Op: "dial", Err: syscall.ECONNREFUSED}):  false,
		wrap(&net.DNSError{Err: "no such host", IsNotFound: true}): false,
		wrap(context.Canceled):                                     false,
		errors.New("provider returned malformed arguments"):        false,
	} {
		if got := transient(err); got != want {
			t.Errorf("transient(%v) = %v, want %v", err, got, want)
		}
	}
}

// Every backend a connection can select retries by default.
func TestModelsRetryByDefault(t *testing.T) {
	spec := Spec{Name: "m", ModelID: "m", BaseURL: "http://localhost"}
	chatSpec, _ := wire.Lookup(wire.OpenAICompatible)
	responsesSpec, _ := wire.Lookup(wire.OpenAIResponses)
	messagesSpec, _ := wire.Lookup(wire.Anthropic)
	chat, _ := newChatModel(spec, chatSpec, nil)
	responses, _ := newResponsesModel(spec, responsesSpec, nil)
	messages, _ := newMessagesModel(spec, messagesSpec, nil)
	for name, policy := range map[string]retryPolicy{"chat": chat.retry, "responses": responses.retry, "messages": messages.retry} {
		if policy != defaultRetryPolicy {
			t.Errorf("%s retry policy = %+v", name, policy)
		}
	}
}
