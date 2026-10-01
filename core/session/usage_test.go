package session

import (
	"testing"

	"github.com/hizkifw/kon/core/tokens"
)

// reply is an assistant message that used and cost what it is given.
func reply(prompt tokens.Count, cost float64) Message {
	message := TextMessage(RoleAssistant, "done")
	message.Usage = &Usage{PromptTokens: prompt, CompletionTokens: 1, TotalTokens: prompt + 1, Cost: cost}
	return message
}

func TestTotalUsageCountsRepliesAndCompactions(t *testing.T) {
	answer := reply(100, 0.5)
	entries := []Entry{
		{Type: EntryTypeMessage, Message: &Message{Role: RoleUser}},
		{Type: EntryTypeMessage, Message: &answer},
		{Type: EntryTypeCompaction, Usage: &Usage{PromptTokens: 40, CompletionTokens: 4, CachedTokens: 30, Cost: 0.25}},
	}
	got := TotalUsage(entries)
	want := Usage{PromptTokens: 140, CompletionTokens: 5, TotalTokens: 101, CachedTokens: 30, Cost: 0.75}
	if got != want {
		t.Fatalf("usage = %+v, want %+v", got, want)
	}
}
