package main

import (
	"fmt"
	"os"

	"github.com/hizkifw/kon/ast"
	"github.com/hizkifw/kon/token"
)

func realMain() int {
	if len(os.Args) < 2 {
		fmt.Printf("Usage: %s <input.kon>\n", os.Args[0])
		return 1
	}

	tokens, err := token.TokenizeFile(os.Args[1])
	if err != nil {
		fmt.Printf("Error tokenizing file: %v", err)
		return 1
	}

	fmt.Println("Got tokens:")
	for _, tok := range tokens {
		fmt.Printf("  %s\t%#v\n", tok.Typ, tok.Val)
	}

	parser := ast.NewParser(tokens)
	parsed := parser.Parse()
	fmt.Printf("Parsed: %#v\n", parsed)

	return 0
}

func main() {
	os.Exit(realMain())
}
