package acp

import (
	"encoding/json"
	"io"
	"sync"
)

// JSON-RPC error codes ACP uses.
const (
	codeParseError     = -32700
	codeInvalidRequest = -32600
	codeMethodNotFound = -32601
	codeInvalidParams  = -32602
	codeInternalError  = -32603
	codeNotFound       = -32002
)

// rpcError is a JSON-RPC error. A handler returns one to choose the code; any
// other error is reported as an internal error with its message.
type rpcError struct {
	Code    int    `json:"code"`
	Message string `json:"message"`
}

func (e *rpcError) Error() string { return e.Message }

func invalidParams(err error) *rpcError {
	return &rpcError{Code: codeInvalidParams, Message: "invalid params: " + err.Error()}
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
	Error   *rpcError       `json:"error,omitempty"`
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
	failure, ok := err.(*rpcError)
	if !ok {
		failure = &rpcError{Code: codeInternalError, Message: err.Error()}
	}
	w.send(response{JSONRPC: "2.0", ID: id, Error: failure})
}

func (w *writer) notify(method string, params any) {
	w.send(notification{JSONRPC: "2.0", Method: method, Params: params})
}
