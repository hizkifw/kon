package ui

import (
	"time"

	"github.com/hizkifw/kon/core/agent"
)

// fakeTurn puts m in a turn, as a submitted prompt would, without starting
// anything; a test replaces its cancel to observe an interrupt.
func fakeTurn(m *Model) {
	m.runEpoch++
	m.turn = &run{epoch: m.runEpoch, cancel: func() {}, start: time.Now(), verb: "Working"}
}

// turnEvent is event arriving from m's turn.
func turnEvent(m Model, event agent.Event) runMsg {
	return runMsg{epoch: m.turn.epoch, event: event}
}

// turnDone is m's turn ending with err.
func turnDone(m Model, err error) runMsg {
	return runMsg{epoch: m.turn.epoch, done: true, err: err}
}
