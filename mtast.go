package monte

import (
	"fmt"
	"strings"
)

type Expr interface {
	String() string
}

type IntLit struct {
	value int64 // TODO: bignum
}

type StrLit struct {
	value string
}

type DoubleLit struct {
	value float64
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
	target    Expr
	verb      string
	args      []Expr
	namedArgs []NamedExpr
}

type NamedExpr struct {
	name  Expr
	value Expr
}

type IfExpr struct {
	cond Expr
	cons Expr
	alt  Expr
}

type EscapeExpr struct {
	patt Expr
	expr Expr
}

type AssignExpr struct {
	target string
	rhs    Expr
}

type SeqExpr struct {
	exprs []Expr
}

type BindingExpr struct {
	name string
}

type ObjectExpr struct {
	// doc string
	name   Pattern
	asExpr Expr
	//implements []Expr
	methods []Method
	// TODO: matchers
}

type Method struct {
	// doc string
	verb        string
	params      []Pattern
	namedParams []NamedPattern
	guardOpt    Expr
	body        Expr
}

type Pattern interface {
	String() string
}
type NamedPattern struct {
	key   Expr
	patt  Pattern
	value Expr
}

type FinalPatt struct {
	name  string // ISSUE: NounExpr?
	guard Expr
}

type IgnorePatt struct {
	guard Expr
}

type ListPatt struct {
	items []Pattern
}

type ViaPatt struct {
	expr Expr
	patt Pattern
}

type VarPatt struct {
	name  string
	guard Expr
}

func fmtGuardOpt(guardOpt Expr) string {
	if guardOpt == nil {
		return ""
	}
	return fmt.Sprintf(" :%v", guardOpt)
}

func (pat *IgnorePatt) String() string {
	return "_" + fmtGuardOpt(pat.guard)
}

func (fp *FinalPatt) String() string {
	return fp.name + fmtGuardOpt(fp.guard)
}

func (pat *ListPatt) String() string {
	return fmt.Sprintf("[%s]", printPatts(", ", pat.items...))
}

func (pat *ViaPatt) String() string {
	return fmt.Sprintf("via (%s) %s", pat.expr, pat.patt)
}

func (pat *VarPatt) String() string {
	return "var " + pat.name + fmtGuardOpt(pat.guard)
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
func printNamedPatts(sep string, items ...NamedPattern) string {
	if len(items) == 0 {
		return ""
	}

	parts := make([]string, len(items))
	for ix, item := range items {
		if item.value != nil {
			parts[ix] = fmt.Sprintf("%v => %v := %v",
				item.key, item.patt, item.value)
		} else {
			parts[ix] = fmt.Sprintf("%v => %v",
				item.key, item.patt)
		}
	}
	return sep + strings.Join(parts, sep)
}
func printMethods(sep string, items ...Method) string {
	parts := make([]string, len(items))
	for ix, item := range items {
		parts[ix] = item.String()
	}
	return strings.Join(parts, sep)
}

func (oe *ObjectExpr) String() string {
	return fmt.Sprintf("object %s%s {\n  %s\n}",
		oe.name,
		optWithSigil(" as", oe.asExpr),
		printMethods("\n  ", oe.methods...))
}

func optWithSigil(sigil string, expr Expr) string {
	if expr == nil {
		return ""
	}
	return sigil + " " + expr.String()
}

func (m Method) String() string {
	return fmt.Sprintf("method %s(%s%s) {\n    %s\n  }", m.verb,
		printPatts(", ", m.params...), printNamedPatts(", ", m.namedParams...), m.body)
}

func (expr *CallExpr) String() string {
	return fmt.Sprintf("%s.%s(%s)", expr.target, expr.verb,
		printExprs(", ", expr.args...))
}

func (expr *EscapeExpr) String() string {
	return fmt.Sprintf("escape %s {\n %s }\n", expr.patt, expr.expr)
}

func (expr *SeqExpr) String() string {
	return "# SeqExpr\n" + printExprs(";\n", expr.exprs...)
}

func (expr *AssignExpr) String() string {
	return fmt.Sprintf("%s := %v", expr.target, expr.rhs)
}

func (expr *IfExpr) String() string {
	if expr.alt == nil {
		return fmt.Sprintf("if (%s) { %s }", expr.cond, expr.cons)
	}
	return fmt.Sprintf("if (%s) { %s } else { %s }", expr.cond, expr.cons, expr.alt)
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

func (expr *BindingExpr) String() string {
	return "&&" + expr.name
}

func (expr *IntLit) String() string {
	return fmt.Sprintf("%d", expr.value)
}

func (expr *StrLit) String() string {
	return fmt.Sprintf("%q", expr.value) // TODO: quoting / escaping?
}

func (expr *DoubleLit) String() string {
	return fmt.Sprintf("%v", expr.value)
}
