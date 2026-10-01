package agent

import (
	"kon.kitsu.red/core/session"
	"kon.kitsu.red/core/tokens"
)

// A projected compaction summary is delivered as a user message wrapped in
// these markers rather than folded into the system prompt. Keeping the system
// prompt byte-identical across compactions preserves the stable prefix that
// provider prompt caches key on. Only the summary is persisted; the markers
// wrap it each time the context is projected, so a change to them applies to
// every session.
const (
	summaryPrefix = checkpointPreamble + "\n\n<compacted-summary>\n"
	summarySuffix = "\n</compacted-summary>"
)

// checkpointPreamble tells the model what a summary is and how to go on from
// it. It is the conversation checkpoint preamble of DeepSeek Harness's
// compaction-basic package, verbatim, under the MIT license (see
// THIRD_PARTY_NOTICES).
const checkpointPreamble = "This is an automatically generated checkpoint condensing an earlier span of the conversation to free up context. Treat the captured context as established background and build on it without restating it. Continue the task directly from the messages that follow, without acknowledging this checkpoint."

// Context projects a session's active path for a model request, with each
// compaction summary wrapped in its checkpoint markers.
func Context(store Store) ([]session.ContextMessage, error) {
	items, err := store.Context()
	if err != nil {
		return nil, err
	}
	for i := range items {
		if items[i].Summary {
			items[i].Message = session.TextMessage(session.RoleUser, summaryPrefix+items[i].Message.Text()+summarySuffix)
		}
	}
	return items, nil
}

// Limits sizes the context a runner keeps: the model's window and output
// limit, and the compaction budgets within them. They are the runner's own
// options, resolved by the caller from the model and configuration.
//
// Compaction is sized from the window W, the way DeepSeek Harness sizes it,
// so a 32K local model and a 1M one are both served. A reply and a summary
// each get room of min(32K, W/8). Compaction runs at min(80% of W, W less
// both), since models work worse in a nearly full window, and keeps the newest
// 16% of what a reply leaves verbatim. A configured budget replaces the size
// derived for it.
type Limits struct {
	// ContextWindow is the model's context size. Zero means unknown, which
	// turns off automatic compaction; a forced one still runs.
	ContextWindow tokens.Count
	// OutputLimit is the most the model writes in one response, or zero when
	// unknown.
	OutputLimit tokens.Count
	// ReserveTokens, when set, is the headroom kept free below the window:
	// compaction runs once the context would eat into it.
	ReserveTokens tokens.Count
	// KeepRecentTokens, when set, is how much recent conversation a
	// compaction keeps verbatim instead of summarizing.
	KeepRecentTokens tokens.Count
}

const (
	// replyTokens is the most room kept for a reply: what kon asks for on the
	// Messages API, the one that requires a cap and refuses a request whose
	// context and cap together overrun the window.
	replyTokens tokens.Count = 32_000
	// summaryTokens caps a summary, reasoning included, for a model whose
	// output limit is known.
	summaryTokens tokens.Count = 32_000
	// unknownOutputSummaryTokens caps a summary for a model whose output
	// limit is unknown. It is the smallest limit common among models kon
	// talks to, so the request is not refused for asking too much.
	unknownOutputSummaryTokens tokens.Count = 8_192
	// unknownWindowKeepTokens is kept verbatim when the window is unknown and
	// there is no share of it to take.
	unknownWindowKeepTokens tokens.Count = 20_000
)

// replyRoom is the room kept free for a reply.
func (l Limits) replyRoom() tokens.Count {
	room := min(replyTokens, l.ContextWindow/8)
	if l.OutputLimit > 0 {
		room = min(room, l.OutputLimit)
	}
	return room
}

// summaryBudget caps a compaction summary's output tokens, reasoning
// included. A configured reserve holds it to half the reserve, the room left
// once compaction starts.
func (l Limits) summaryBudget() tokens.Count {
	budget := unknownOutputSummaryTokens
	if l.OutputLimit > 0 {
		budget = min(summaryTokens, l.OutputLimit)
	}
	if l.ContextWindow > 0 {
		budget = min(budget, l.ContextWindow/8)
	}
	if l.ReserveTokens > 0 {
		budget = min(budget, l.ReserveTokens/2)
	}
	return budget
}

// threshold is the context size past which automatic compaction runs. It
// needs a known window.
func (l Limits) threshold() tokens.Count {
	if l.ReserveTokens > 0 {
		return l.ContextWindow - l.ReserveTokens
	}
	return min(l.ContextWindow*4/5, l.ContextWindow-l.replyRoom()-l.summaryBudget())
}

// keepRecent is how much recent conversation a compaction keeps verbatim.
func (l Limits) keepRecent() tokens.Count {
	switch {
	case l.KeepRecentTokens > 0:
		return l.KeepRecentTokens
	case l.ContextWindow <= 0:
		return unknownWindowKeepTokens
	}
	return (l.ContextWindow - l.replyRoom()) * 4 / 25
}
