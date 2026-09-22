// Package markdown renders CommonMark (plus GFM tables, strikethrough, task
// lists, and autolinks) into transcript-ready lines and styled spans.
//
// The design goal mirrors internal/ui's streaming invariants: a message
// rendered incrementally while it streams must be byte-identical to the same
// message rendered from scratch after it finalizes. The package therefore
// provides one core (blockRenderer) used by both paths:
//
//   - Render parses once and returns finished lines.
//   - Stream wraps the same core in append-only frozen blocks, so the stream's
//     published lines are already finished and never revised. Finish closes
//     the stream (freezing the open tail) before the result folds into
//     stable history.
//
// Blocks expose Boundary() (their byte offset after the last closed block) so
// a caller can re-parse only the grown tail.
//
// Inline coverage is deliberately partial for now: headings, code blocks,
// list markers, quote gutters, and task checkboxes emit styled spans, while
// emphasis, strikethrough, and link URLs keep their text but drop their
// styling and destinations (StyleEmph/StyleStrong/StyleStrikethrough/
// StyleLink/StyleLinkURL are reserved). Raw HTML and autolink URLs are
// dropped from the text. Extending inlineText to walk those node types with
// span accumulation is the next layer and must stay wrap-compatible: spans
// annotate whole display lines after wrapping, so inline styling must survive
// re-wrapping by construction.
package markdown

import "strings"

// Style identifies one visual role. The terminal layer maps these to actual
// colors and attributes (see the internal/ui palette), keeping this package
// presentation-free.
type Style uint8

const (
	StyleText          Style = iota // ordinary prose
	StyleHeading                    // ATX and setext headings
	StyleFaint                      // rules and horizontal filler
	StyleCodeBlock                  // fenced and indented code block text
	StyleCodeInline                 // inline code spans
	StyleQuote                      // blockquote prose
	StyleQuoteMark                  // blockquote gutter bar
	StyleListBullet                 // bullet or ordered-list marker
	StyleEmph                       // emphasis
	StyleStrong                     // strong emphasis
	StyleLink                       // link text
	StyleLinkURL                    // link destination
	StyleStrikethrough              // ~~struck~~ text
	StyleTask                       // task-list checkbox
)

// Theme resolves styles into display attributes. Values are meaningful only to
// the layer that consumes them.
type Theme struct {
	Styles map[Style]string
}

// NewTheme builds a theme from style/attribute pairs.
func NewTheme(styles map[Style]string) Theme {
	if styles == nil {
		styles = map[Style]string{}
	}
	return Theme{Styles: styles}
}

// Attr returns the attributes registered for a style.
func (t Theme) Attr(s Style) string { return t.Styles[s] }

// Styled is one styled region of a line.
type Styled struct {
	Text string
	// Attr carries the theme attributes for the region.
	Attr string
}

// Line is one display line with its styled regions. Plain lines carry no
// spans; callers must treat text as authoritative when spans are absent.
type Line struct {
	Text  string
	Spans []Styled
}

// Plain builds an unstyled line.
func Plain(s string) Line { return Line{Text: s} }

// oscStripper removes OSC 8 hyperlink sequences from a stream of deltas.
// It is the OSC counterpart of ui.ansiStripper (which also drops CSI and
// C0 controls): stateful across calls, so a sequence split between deltas
// is dropped whole. kon strips CSI/control bytes at the transcript boundary
// before text reaches this package, so only OSC 8 markers — which are
// legitimate markdown content from the model — can appear here.
type oscStripper struct {
	state uint8
}

const (
	oscGround = iota // plain text
	oscEsc           // saw ESC
	oscOSC           // inside OSC; consume through BEL or ST (ESC \)
	oscOSCESC        // inside OSC; saw the ESC of an ST terminator
)

// strip returns text with OSC 8 hyperlink sequences removed. The result may be
// short when text ends inside a sequence; the rest of that sequence is dropped
// when it arrives in a later delta.
func (s *oscStripper) strip(text string) string {
	if s.state == oscGround {
		i := strings.IndexByte(text, 0x1b)
		if i < 0 {
			return text
		}
	}
	var out strings.Builder
	out.Grow(len(text) + 16)
	for i := 0; i < len(text); i++ {
		b := text[i]
		switch s.state {
		case oscGround:
			switch b {
			case 0x1b:
				s.state = oscEsc
			default:
				out.WriteByte(b)
			}
		case oscEsc:
			switch b {
			case ']':
				s.state = oscOSC
			default:
				out.WriteByte(0x1b)
				out.WriteByte(b)
				s.state = oscGround
			}
		case oscOSC:
			switch b {
			case 0x07: // BEL terminator
				s.state = oscGround
			case 0x1b:
				s.state = oscOSCESC
			}
		case oscOSCESC:
			// ST is ESC \. Anything else after ESC inside an OSC is invalid;
			// consume the byte and resume ground either way.
			s.state = oscGround
		}
	}
	return out.String()
}
