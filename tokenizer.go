package main

import (
	"bufio"
	"fmt"
	"os"
)

type Loc struct {
	File string
	Line int
	Col  int
}

func (l Loc) String() string {
	return fmt.Sprintf("%s:%d:%d", l.File, l.Line, l.Col)
}

type Token struct {
	Typ TokenType
	Val string
	Loc Loc
}

func IsDigit(b byte) bool {
	return b >= '0' && b <= '9'
}

func IsAlpha(b byte) bool {
	return (b >= 'a' && b <= 'z') || (b >= 'A' && b <= 'Z')
}

func IdentifyKeyword(ident string) (TokenType, string) {
	// If we add more keyword tokens, this will fail to compile
	const nKeywords uint = 5
	const _ uint = nKeywords - uint(endTokenKeywords-startTokenKeywords-1)

	switch ident {
	case "fun":
		return TokenFunctionKw, ""
	case "return":
		return TokenReturnKw, ""
	case "if":
		return TokenIfKw, ""
	case "type":
		return TokenTypeKw, ""
	case "struct":
		return TokenStructKw, ""
	default:
		return TokenIdent, ident
	}
}

func TokenizeFile(fileName string) ([]Token, error) {
	file, err := os.Open(fileName)
	if err != nil {
		return nil, fmt.Errorf("failed to read file %s: %w", fileName, err)
	}
	defer file.Close()

	loc := Loc{
		File: fileName,
		Line: 0,
		Col:  0,
	}

	tokens := make([]Token, 0)

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		loc.Line += 1

		for col := 0; col < len(line); col++ {
			// add buffer so slicing doesn't go out of range
			s := line[col:] + "  "
			c := line[col]
			loc.Col = col

			if IsDigit(c) {
				for {
					col++
					if col >= len(line) {
						break
					}

					c = line[col]
					if !IsDigit(c) {
						break
					}
				}
				tokens = append(tokens, Token{
					Typ: TokenNumLiteral,
					Val: line[loc.Col:col],
					Loc: loc,
				})
				col--
			} else if IsAlpha(c) {
				for {
					col++
					if col >= len(line) {
						break
					}

					c = line[col]
					if !IsAlpha(c) && !IsDigit(c) {
						break
					}
				}

				typ, val := IdentifyKeyword(line[loc.Col:col])
				tokens = append(tokens, Token{
					Typ: typ,
					Val: val,
					Loc: loc,
				})
				col--
			} else if c == ' ' || c == '\t' || c == '\r' || c == '\n' {
				// Whitespace, skip for now
			} else if s[:2] == "//" {
				tokens = append(tokens, Token{Typ: TokenComment, Val: line[col:], Loc: loc})
				col = len(s)
			} else if c == '(' {
				tokens = append(tokens, Token{Typ: TokenParenO, Val: "", Loc: loc})
			} else if c == ')' {
				tokens = append(tokens, Token{Typ: TokenParenC, Val: "", Loc: loc})
			} else if c == '[' {
				tokens = append(tokens, Token{Typ: TokenBracketO, Val: "", Loc: loc})
			} else if c == ']' {
				tokens = append(tokens, Token{Typ: TokenBracketC, Val: "", Loc: loc})
			} else if c == '{' {
				tokens = append(tokens, Token{Typ: TokenBraceO, Val: "", Loc: loc})
			} else if c == '}' {
				tokens = append(tokens, Token{Typ: TokenBraceC, Val: "", Loc: loc})
			} else if c == ';' {
				tokens = append(tokens, Token{Typ: TokenSemi, Val: "", Loc: loc})
			} else if s[:2] == "::" {
				col++
				tokens = append(tokens, Token{Typ: TokenScope, Val: "", Loc: loc})
			} else if c == ',' {
				tokens = append(tokens, Token{Typ: TokenComma, Val: "", Loc: loc})
			} else if c == '.' {
				tokens = append(tokens, Token{Typ: TokenPeriod, Val: "", Loc: loc})
			} else if s[:2] == "<=" {
				col++
				tokens = append(tokens, Token{Typ: TokenLTE, Val: "", Loc: loc})
			} else if c == '<' {
				tokens = append(tokens, Token{Typ: TokenLT, Val: "", Loc: loc})
			} else if s[:2] == ">=" {
				col++
				tokens = append(tokens, Token{Typ: TokenGTE, Val: "", Loc: loc})
			} else if c == '>' {
				tokens = append(tokens, Token{Typ: TokenGT, Val: "", Loc: loc})
			} else if s[:2] == "==" {
				col++
				tokens = append(tokens, Token{Typ: TokenEqual, Val: "", Loc: loc})
			} else if c == '=' {
				tokens = append(tokens, Token{Typ: TokenAssign, Val: "", Loc: loc})
			} else if s[:2] == "&&" {
				col++
				tokens = append(tokens, Token{Typ: TokenLogicalAnd, Val: "", Loc: loc})
			} else if c == '&' {
				tokens = append(tokens, Token{Typ: TokenBitwiseAnd, Val: "", Loc: loc})
			} else if s[:2] == "||" {
				col++
				tokens = append(tokens, Token{Typ: TokenLogicalOr, Val: "", Loc: loc})
			} else if c == '|' {
				tokens = append(tokens, Token{Typ: TokenBitwiseOr, Val: "", Loc: loc})
			} else if s[:2] == "->" {
				col++
				tokens = append(tokens, Token{Typ: TokenArrowR, Val: "", Loc: loc})
			} else {
				LogError(loc, "Unknown token: %s", string(c))
			}
		}
	}

	return tokens, nil
}
