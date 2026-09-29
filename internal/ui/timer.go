package ui

import (
	"fmt"
	"strings"
	"time"
)

// The turn marker is a dot that reads as a status light: outline and filled
// alternate once a second while a turn is running, so the transcript shows
// motion even when the seconds digit has not changed, and a finished turn stays
// filled. Both glyphs are one cell wide, so the marker never shifts the text.
const (
	markOutline = "○"
	markFilled  = "●"
)

// runningLabel is the running form of the turn marker, for a turn or any other
// timed activity such as a side answer. The dot flips on the elapsed second's
// parity, so it is a pure function of the clock and cannot drift out of step
// with the displayed total.
func runningLabel(verb string, elapsed time.Duration) string {
	mark := markFilled
	if int(elapsed/time.Second)%2 == 1 {
		mark = markOutline
	}
	return mark + " " + verb + "… " + formatDuration(elapsed)
}

// workedLabel is the finished form of the turn marker: the dot stays filled and
// the total is frozen. Live and replayed turns both build from here so they
// render identically.
func workedLabel(elapsed time.Duration) string {
	return markFilled + " Worked for " + formatDuration(elapsed)
}

// stoppedLabel replaces the total for a replayed turn whose process died before
// it finished, so no duration was recorded. The outline dot sets it apart from a
// completed turn's filled one.
const stoppedLabel = markOutline + " Stopped abruptly"

// formatDuration renders a duration at second precision, dropping zero-valued
// components so it reads naturally: "30s", "2m 30s", "1h 20m 50s".
func formatDuration(d time.Duration) string {
	seconds := int(d / time.Second)
	if seconds < 0 {
		seconds = 0
	}
	h, m, s := seconds/3600, seconds%3600/60, seconds%60
	parts := make([]string, 0, 3)
	if h > 0 {
		parts = append(parts, fmt.Sprintf("%dh", h))
	}
	if m > 0 {
		parts = append(parts, fmt.Sprintf("%dm", m))
	}
	if s > 0 || len(parts) == 0 {
		parts = append(parts, fmt.Sprintf("%ds", s))
	}
	return strings.Join(parts, " ")
}
