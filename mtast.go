package monte

import (
	"fmt"
	"strings"
)

type Expr interface {
	String() string
}

type IntExpr struct {
	value int64 // TODO: bignum
}

type NounExpr struct {
	name string
}

type DefExpr struct {
	patt Pattern
	exit Expr
	expr Expr
}

type CallExpr struct {
	target Expr
	verb   string
	args   []Expr
	//namedArgs
}

type ObjectExpr struct {
	// doc string
	name Pattern
	//asExpr Expr
	//implements []Expr
	methods []Method
	// TODO: matchers
}

type Method struct {
	// doc string
	verb   string
	params []Pattern
	// guardOpt Expr
	body Expr
}

type Pattern interface {
	String() string
}

type FinalPatt struct {
	name  string // ISSUE: NounExpr?
	guard Expr
}

func (fp *FinalPatt) String() string {
	if fp.guard != nil {
		return fmt.Sprintf("%v :%v", fp.name, fp.guard)
	}
	return fp.name
}

type IgnorePatt struct {
	guard Expr
}

func (pat *IgnorePatt) String() string {
	if pat.guard == nil {
		return "_"
	}
	return fmt.Sprintf("_ :%v", pat.guard)
}

func printExprs(sep string, items ...Expr) string {
	parts := make([]string, len(items))
	for ix, item := range items {
		parts[ix] = item.String()
	}
	return strings.Join(parts, sep)
}
func printPatts(sep string, items ...Pattern) string {
	parts := make([]string, len(items))
	for ix, item := range items {
		parts[ix] = item.String()
	}
	return strings.Join(parts, sep)
}
func printMethods(sep string, items ...Method) string {
	parts := make([]string, len(items))
	for ix, item := range items {
		parts[ix] = item.String()
	}
	return strings.Join(parts, sep)
}

func (oe *ObjectExpr) String() string {
	return fmt.Sprintf("object %s {\n  %s\n}", oe.name, printMethods("\n  ", oe.methods...))
}

func (m Method) String() string {
	return fmt.Sprintf("method %s(%s) {\n    %s\n  }", m.verb,
		printPatts(", ", m.params...), m.body)
}

func (expr *CallExpr) String() string {
	return fmt.Sprintf("%s.%s(%s)", expr.target, expr.verb,
		printExprs(", ", expr.args...))
}

func (expr *DefExpr) String() string {
	if expr.exit == nil {
		return fmt.Sprintf("def %s := %s", expr.patt, expr.expr)
	}
	return fmt.Sprintf("def %s exit %s := %s", expr.patt, expr.exit, expr.expr)
}

func (expr *NounExpr) String() string {
	return expr.name
}

func (expr *IntExpr) String() string {
	return fmt.Sprintf("%d", expr.value)
}
