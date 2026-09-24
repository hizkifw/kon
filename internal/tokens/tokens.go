// Package tokens defines the token count shared by configuration, sessions,
// providers, and the terminal, so a count cannot be mixed with other integers
// and renders the same way wherever it is shown.
package tokens

import "fmt"

// Count is a number of model tokens. Its JSON form is a plain integer, so
// persisted configuration and sessions keep their existing shape.
type Count int

// String renders the count compactly, such as 950, 12.4k, or 1.0m, which is how
// every token figure is shown to the user.
func (c Count) String() string {
	if c >= 1_000_000 {
		return fmt.Sprintf("%.1fm", float64(c)/1_000_000)
	}
	if c >= 1_000 {
		return fmt.Sprintf("%.1fk", float64(c)/1_000)
	}
	return fmt.Sprintf("%d", int(c))
}
