package monte

import (
	"fmt"
	"strings"
)

type Expr interface {
	String() string
}

type IntExpr struct {
	value int // TODO: bignum
}

type NounExpr struct {
	name string
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
		panic("not implemented")
	}
	return fp.name
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

func (ce *CallExpr) String() string {
	return fmt.Sprintf("%s.%s(%s)", ce.target, ce.verb,
		printExprs(", ", ce.args...))
}

func (e *NounExpr) String() string {
	return e.name
}

func (i *IntExpr) String() string {
	return fmt.Sprintf("%d", i.value)
}
