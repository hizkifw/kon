package acp

import (
	"encoding/json"
	"io"
	"sync"

	"kon.kitsu.red/core/acp"
)

// invalidParams is the error for params kon could not read. A handler returns
// an *acp.Error to choose the code; any other error is reported as an
// internal error with its message.
func invalidParams(err error) *acp.Error {
	return &acp.Error{Code: acp.CodeInvalidParams, Message: "invalid params: " + err.Error()}
}

// incoming is any message the client sends. A request has an ID and a
// method, a notification only a method, and a response, which kon never asks
// for, only an ID.
type incoming struct {
	ID     json.RawMessage `json:"id"`
	Method string          `json:"method"`
	Params json.RawMessage `json:"params"`
}

type response struct {
	JSONRPC string          `json:"jsonrpc"`
	ID      json.RawMessage `json:"id"`
	Result  any             `json:"result,omitempty"`
	Error   *acp.Error      `json:"error,omitempty"`
}

type notification struct {
	JSONRPC string `json:"jsonrpc"`
	Method  string `json:"method"`
	Params  any    `json:"params"`
}

// writer sends one JSON message per line. Turns of several sessions write
// concurrently, so each message is written whole under the lock, and the
// first write error is kept: a client that has gone away is not written to
// again.
type writer struct {
	mu  sync.Mutex
	enc *json.Encoder
	err error
}

func newWriter(w io.Writer) *writer { return &writer{enc: json.NewEncoder(w)} }

func (w *writer) send(v any) {
	w.mu.Lock()
	defer w.mu.Unlock()
	if w.err == nil {
		w.err = w.enc.Encode(v)
	}
}

func (w *writer) respond(id json.RawMessage, result any, err error) {
	if err == nil {
		w.send(response{JSONRPC: "2.0", ID: id, Result: result})
		return
	}
	failure, ok := err.(*acp.Error)
	if !ok {
		failure = &acp.Error{Code: acp.CodeInternalError, Message: err.Error()}
	}
	w.send(response{JSONRPC: "2.0", ID: id, Error: failure})
}

func (w *writer) notify(method string, params any) {
	w.send(notification{JSONRPC: "2.0", Method: method, Params: params})
}
