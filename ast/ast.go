package ast

import (
	"github.com/hizkifw/kon/token"
)

type Loc struct {
	File string
	Line int
	Col  int
}

type Ident struct {
	Name string
	Pos  *Loc
}

type Field struct {
	Name *Ident
	Type Expr
}

type Expr interface{}

type IntLiteralExpr struct {
	Val int
}

type StrLiteralExpr struct {
	Val string
}

type UnaryExpr struct {
	Operator token.Token
	Right    Expr
}

type BinaryExpr struct {
	Left     Expr
	Operator token.Token
	Right    Expr
}

type GroupingExpr struct{}
