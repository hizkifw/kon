package main

type TokenType uint

//go:generate stringer -type TokenType -trimprefix Token
const (
	TokenUnknown TokenType = iota
	TokenWhitespace
	TokenComment
	TokenIdent

	TokenParenO   // (
	TokenParenC   // )
	TokenBracketO // [
	TokenBracketC // ]
	TokenBraceO   // {
	TokenBraceC   // }
	TokenSemi     // ;
	TokenScope    // ::
	TokenComma    // ,
	TokenPeriod   // .

	TokenNumLiteral
	TokenStrLiteral

	startTokenKeywords

	TokenFunctionKw
	TokenIfKw
	TokenReturnKw
	TokenTypeKw
	TokenStructKw

	endTokenKeywords

	TokenLT         // <
	TokenLTE        // <=
	TokenGT         // >
	TokenGTE        // >=
	TokenAssign     // =
	TokenEqual      // ==
	TokenLogicalAnd // &&
	TokenLogicalOr  // ||
	TokenBitwiseAnd // &
	TokenBitwiseOr  // |
	TokenArrowR     // ->

	totalTokens
)
