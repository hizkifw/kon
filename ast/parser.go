package ast

import (
	"fmt"
	"strconv"

	"github.com/hizkifw/kon/token"
)

func LogError(l token.Loc, format string, a ...any) {
	fmt.Printf("%s: Error: %s\n", l, fmt.Sprintf(format, a...))
}

// Parser is a recursive descent parser
type Parser struct {
	tokens  []token.Token
	current int
}

func NewParser(tokens []token.Token) *Parser {
	return &Parser{
		tokens:  tokens,
		current: 0,
	}
}

func (p *Parser) peek() *token.Token {
	return &p.tokens[p.current]
}

func (p *Parser) previous() *token.Token {
	return &p.tokens[p.current-1]
}

func (p *Parser) isAtEnd() bool {
	return p.current == len(p.tokens)
}

func (p *Parser) advance() *token.Token {
	if !p.isAtEnd() {
		p.current++
	}
	return p.previous()
}

func (p *Parser) check(typ token.TokenType) bool {
	if p.isAtEnd() {
		return false
	}
	return p.peek().Typ == typ
}

func (p *Parser) match(types ...token.TokenType) bool {
	for _, typ := range types {
		if p.check(typ) {
			p.advance()
			return true
		}
	}
	return false
}

func (p *Parser) consume(typ token.TokenType, msg string) *token.Token {
	if p.check(typ) {
		return p.advance()
	}
	LogError(p.peek().Loc, "at '%s': %s", p.peek(), msg)
	return nil
}

func (p *Parser) synchronize() {
	p.advance()

	for !p.isAtEnd() {
		if p.previous().Typ == token.TokenSemi {
			return
		}

		t := p.peek().Typ
		if t == token.TokenIfKw || t == token.TokenReturnKw {
			return
		}

		p.advance()
	}
}

func (p *Parser) expression() Expr {
	if expr := p.binary(); expr != nil {
		return expr
	}

	if expr := p.unary(); expr != nil {
		return expr
	}

	if expr := p.literal(); expr != nil {
		return expr
	}

	return nil
}

func (p *Parser) literal() Expr {
	if p.match(token.TokenNumLiteral) {
		i, err := strconv.Atoi(p.previous().Val)
		if err != nil {
			LogError(p.previous().Loc, "Invalid number literal: %s", p.previous().Val)
			return nil
		}

		return &IntLiteralExpr{
			Val: i,
		}
	}

	if p.match(token.TokenStrLiteral) {
		return &StrLiteralExpr{
			Val: p.previous().Val,
		}
	}

	return nil
}

func (p *Parser) unary() Expr {
	if p.match(token.TokenMinus, token.TokenBang) {
		operator := p.previous()
		right := p.expression()
		if right == nil {
			return nil
		}
		return &UnaryExpr{
			Operator: *operator,
			Right:    right,
		}
	}

	return nil
}

func (p *Parser) binary() Expr {
	left := p.expression()

	for p.match(token.TokenPlus, token.TokenMinus, token.TokenMult, token.TokenDiv) {
		operator := p.previous()
		right := p.expression()
		if right == nil {
			return nil
		}
		left = &BinaryExpr{
			Left:     left,
			Operator: *operator,
			Right:    right,
		}
	}

	return left
}

func (p *Parser) Parse() Expr {
	return p.expression()
}
