package provider

import (
	"context"
	"errors"
	"fmt"
	"io"
	"math/rand/v2"
	"net"
	"net/http"
	"strconv"
	"strings"
	"syscall"
	"time"
)

// Retry reports a request that failed with a transient error and will be
// sent again once Delay has passed.
type Retry struct {
	// Attempt numbers this retry from 1; Max is the most there will be.
	Attempt, Max int
	Delay        time.Duration
	// Reason is a short account of the failure, such as "rate limited (429)".
	Reason string
	Err    error
}

// retryPolicy is how a model retries transient failures. The zero value never
// retries, which keeps tests that build a model directly from waiting.
type retryPolicy struct {
	// retries is how many times a failed request is sent again.
	retries int
	// base is the first backoff, doubling with each retry up to limit.
	base, limit time.Duration
}

// defaultRetryPolicy gives a busy or rate-limited server about half a minute
// of backoff in all, longer when the server asks for it.
var defaultRetryPolicy = retryPolicy{retries: 5, base: time.Second, limit: 30 * time.Second}

// maxRetryAfter is the longest wait a server can ask for that kon honors. A
// longer one, such as a spent daily quota, is reported instead of waited out.
const maxRetryAfter = time.Minute

// errStreamClosed is a stream the server ended before its final event.
var errStreamClosed = errors.New("connection closed before the stream finished")

// withRetries runs attempt until it succeeds, fails for good, or the retries
// run out, announcing each retry through emit. A failure is retried only
// while nothing has been emitted: once a response has begun streaming, a
// retry would repeat what the caller already showed. When the retries run
// out the error says so, since the marker that showed them is gone by the
// time the error is read; it still wraps the last failure for errors.As.
func withRetries(ctx context.Context, policy retryPolicy, emit func(Event), attempt func(emit func(Event)) (generation, error)) (generation, error) {
	for retry := 1; ; retry++ {
		emitted := false
		response, err := attempt(func(event Event) {
			emitted = true
			if emit != nil {
				emit(event)
			}
		})
		if err == nil && abandonedReasons[response.Finish] {
			err = &FinishError{Reason: response.Finish}
		}
		if err == nil || emitted {
			return response, err
		}
		if retry > policy.retries {
			if policy.retries > 0 {
				err = fmt.Errorf("gave up after %d retries: %w", policy.retries, err)
			}
			return response, err
		}
		delay, ok := policy.delay(err, retry)
		if !ok {
			return response, err
		}
		if emit != nil {
			emit(Event{Retry: &Retry{Attempt: retry, Max: policy.retries, Delay: delay, Reason: retryReason(err), Err: err}})
		}
		timer := time.NewTimer(delay)
		select {
		case <-ctx.Done():
			timer.Stop()
			return response, fmt.Errorf("%w while waiting to retry after: %w", ctx.Err(), err)
		case <-timer.C:
		}
	}
}

// delay reports how long to wait before retry, or false when err is not worth
// retrying. The server's own estimate wins over backoff.
func (p retryPolicy) delay(err error, retry int) (time.Duration, bool) {
	var apiErr *APIError
	if errors.As(err, &apiErr) {
		if !apiErr.retryable() || apiErr.retryAfter > maxRetryAfter {
			return 0, false
		}
		if apiErr.retryAfter > 0 {
			return apiErr.retryAfter, true
		}
		return p.backoff(retry), true
	}
	if !transient(err) {
		return 0, false
	}
	return p.backoff(retry), true
}

// backoff doubles from base with each retry, up to limit, less up to a
// quarter at random so clients that failed together do not retry together.
func (p retryPolicy) backoff(retry int) time.Duration {
	delay := p.limit
	if shift := retry - 1; shift < 16 {
		delay = min(p.base<<shift, p.limit)
	}
	if jitter := delay / 4; jitter > 0 {
		delay -= rand.N(jitter)
	}
	return delay
}

// retryableTypes are the error types and codes that providers report, in a
// response or inside a stream, for a failure that may pass on its own.
var retryableTypes = map[string]bool{
	"overloaded_error":    true,
	"rate_limit_error":    true,
	"api_error":           true,
	"timeout_error":       true,
	"server_error":        true,
	"rate_limit_exceeded": true,
}

// retryable reports whether the failure may pass on its own. A server's
// x-should-retry header decides when present.
func (e *APIError) retryable() bool {
	switch e.shouldRetry {
	case "true":
		return true
	case "false":
		return false
	}
	switch {
	case e.Status == 0:
		return retryableTypes[e.Type] || retryableTypes[e.Code]
	case e.Status == http.StatusRequestTimeout, e.Status == http.StatusConflict, e.Status == http.StatusTooManyRequests:
		return true
	}
	return e.Status >= 500
}

// transient reports a connection failure that may pass on its own: a reset,
// a timeout, or a stream cut short. A refused connection or an unknown host
// fails at once, since a server that is not there will not appear in a few
// seconds, and the user is better told now.
func transient(err error) bool {
	if errors.Is(err, context.Canceled) || errors.Is(err, context.DeadlineExceeded) {
		return false
	}
	var finish *FinishError
	if errors.As(err, &finish) {
		return abandonedReasons[finish.Reason]
	}
	if errors.Is(err, errStreamClosed) || errors.Is(err, io.EOF) || errors.Is(err, io.ErrUnexpectedEOF) ||
		errors.Is(err, syscall.ECONNRESET) || errors.Is(err, syscall.EPIPE) {
		return true
	}
	var dnsErr *net.DNSError
	if errors.As(err, &dnsErr) {
		return dnsErr.IsTimeout || dnsErr.IsTemporary
	}
	var netErr net.Error
	return errors.As(err, &netErr) && netErr.Timeout()
}

// retryReason names a retried failure for the user.
func retryReason(err error) string {
	var finish *FinishError
	if errors.As(err, &finish) {
		return "stopped early (" + string(finish.Reason) + ")"
	}
	var apiErr *APIError
	if !errors.As(err, &apiErr) {
		var netErr net.Error
		if errors.As(err, &netErr) && netErr.Timeout() {
			return "timed out"
		}
		return "connection lost"
	}
	var reason string
	switch {
	case apiErr.Status == http.StatusTooManyRequests, strings.Contains(apiErr.Type+apiErr.Code, "rate_limit"):
		reason = "rate limited"
	case apiErr.Status == 529, apiErr.Status == http.StatusServiceUnavailable, apiErr.Type == "overloaded_error":
		reason = "overloaded"
	case apiErr.Status == http.StatusRequestTimeout, apiErr.Type == "timeout_error":
		reason = "timed out"
	default:
		reason = "server error"
	}
	if apiErr.Status != 0 {
		reason += fmt.Sprintf(" (%d)", apiErr.Status)
	}
	return reason
}

// retryAfter reads how long a server asked the client to wait, preferring
// the millisecond header OpenAI and Anthropic send beside the standard one.
func retryAfter(header http.Header, now time.Time) time.Duration {
	if ms, err := strconv.ParseFloat(header.Get("Retry-After-Ms"), 64); err == nil && ms > 0 {
		return time.Duration(ms * float64(time.Millisecond))
	}
	value := header.Get("Retry-After")
	if seconds, err := strconv.ParseFloat(value, 64); err == nil && seconds > 0 {
		return time.Duration(seconds * float64(time.Second))
	}
	if at, err := http.ParseTime(value); err == nil && at.After(now) {
		return at.Sub(now)
	}
	return 0
}

// responseError builds the APIError for a failed HTTP response, with the
// retry hints its headers carry.
func responseError(response *http.Response) *APIError {
	raw, _ := io.ReadAll(io.LimitReader(response.Body, maxEventSize))
	apiErr := parseAPIError(response.StatusCode, raw)
	apiErr.retryAfter = retryAfter(response.Header, time.Now())
	apiErr.shouldRetry = response.Header.Get("X-Should-Retry")
	return apiErr
}
