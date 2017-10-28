package monte

import "fmt"
import "strings"
import "errors"


type Object interface {
	recv(verb string, args []Object, nargs []NamedArg) (reply Object, err error)
}

type NamedArg struct {
	name  string
	value Object
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

type Expr interface {
	String() string
}

type ObjectExpr struct {
	// doc string
	name Pattern
	//asExpr Expr
	//implements []Expr
	methods []Method
	// TODO: matchers
}
type UserObject struct {
	scopes []map[string]Object
	code *ObjectExpr
}
func (self *UserObject) String() string {
	return fmt.Sprintf("<%v>", self.code.name)
}

func (self *UserObject) recv(verb string, args []Object, nargs []NamedArg) (reply Object, err error) {
	arity := len(args)
	for _, meth := range self.code.methods {
		if (arity == len(meth.params) && meth.verb == verb) {
			e := EvalCtx{make(map[string]Object), self.scopes}
			for px, param := range meth.params {
				err := e.matchBind(param, args[px])
				if (err != nil) {
					return nil, err
				}
			}
			return e.run(meth.body)
		}
	}
	return nil, errors.New("@@refused")
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

type Method struct {
	// doc string
	verb   string
	params []Pattern
	// guardOpt Expr
	body Expr
}

func (m Method) String() string {
	return fmt.Sprintf("method %s(%s) {\n    %s\n  }", m.verb,
		printPatts(", ", m.params...), m.body)
}

type CallExpr struct {
	target Expr
	verb   string
	args   []Expr
	//namedArgs
}

func (ce *CallExpr) String() string {
	return fmt.Sprintf("%s.%s(%s)", ce.target, ce.verb,
		printExprs(", ", ce.args...))
}

type NounExpr struct {
	name string
}

func (e *NounExpr) String() string {
	return e.name
}

type IntExpr struct {
	value int // TODO: bignum
}
type IntObj struct {
	value int // TODO: bignum
}
func (it *IntObj) recv(verb string, args []Object, nargs []NamedArg) (reply Object, err error) {
	switch {
	case verb == "add" && len(args) == 1:
		that, err := unwrapInt(args[0])
		if (err != nil) {
			return nil, err
		}
		return &IntObj{it.value + that}, nil

	case verb == "subtract" && len(args) == 1:
		that, err := unwrapInt(args[0])
		if (err != nil) {
			return nil, err
		}
		return &IntObj{it.value - that}, nil
	}
	return nil, errors.New("@@refused")
}

func unwrapInt(obj Object) (int, error) {
	switch it := obj.(type) {
	case *IntObj:
		return it.value, nil
	}
	return 0, errors.New("@@not an int")
}


func (i *IntExpr) String() string {
	return fmt.Sprintf("%d", i.value)
}


type Evaluator interface {
	run(expr Expr) (Object, error)
	matchBind(patt Pattern, val Object) error // TODO: ejector
}

type EvalCtx struct {
	locals map[string]Object
	scopes []map[string]Object
}

func (ctx EvalCtx) run(expr Expr) (Object, error) {
	switch it := expr.(type) {
	case *IntExpr:
		return &IntObj{it.value}, nil
	case *NounExpr:
		value, ok := ctx.locals[it.name]
		if(ok) {
			return value, nil
		}
		return nil, errors.New("@@not bound: " + it.name)
	case *CallExpr:
		rx, err := ctx.run(it.target)
		if (err != nil) {
			return nil, err
		}
		params := make([]Object, len(it.args))
		for ix, arg := range it.args {
			params[ix], err = ctx.run(arg)
			if (err != nil) {
				return nil, err
			}
		}
		return rx.recv(it.verb, params, []NamedArg{})

	case *ObjectExpr:
		// TODO: bind it.name in its scope(s)
		obj := UserObject{ctx.scopes, it}
		err := ctx.matchBind(it.name, &obj)
		if (err != nil) {
			return nil, err
		}
		return &obj, nil
	}
	return nil, errors.New(fmt.Sprintf("@@eval not implemented for %v", expr))
}


func (ctx *EvalCtx) matchBind(patt Pattern, specimen Object) (error) {
	// fmt.Printf("matchBind(%v, %v)\n", patt, specimen)
	switch it := patt.(type) {
	case *FinalPatt:
		ctx.locals[it.name] = specimen
		return nil
	}
	return errors.New(fmt.Sprintf("@@matchBind not implmented for %v", patt))
}
