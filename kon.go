package main

import (
	"fmt"
	"os"
)

func LogError(l Loc, format string, a ...any) {
	fmt.Printf("%s: Error: %s\n", l, fmt.Sprintf(format, a...))
}

func realMain() int {
	if len(os.Args) < 2 {
		fmt.Printf("Usage: %s <input.kon>\n", os.Args[0])
		return 1
	}

	tokens, err := TokenizeFile(os.Args[1])
	if err != nil {
		fmt.Printf("Error tokenizing file: %v", err)
		return 1
	}

	fmt.Println("Got tokens:")
	for _, tok := range tokens {
		fmt.Printf("  %s\t%#v\n", tok.Typ, tok.Val)
	}

	return 0
}

func main() {
	os.Exit(realMain())
}
