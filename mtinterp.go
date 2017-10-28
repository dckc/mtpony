package monte

import (
	"errors"
	"fmt"
)

type Evaluator interface {
	run(expr Expr) (Object, error)
	matchBind(patt Pattern, val Object) error // TODO: ejector
}

type EvalCtx struct {
	locals map[string]Object
	parent *EvalCtx
}

type Object interface {
	recv(verb string, args []Object, nargs []NamedArg) (reply Object, err error)
}
type NamedArg struct {
	name  string
	value Object
}

type UserObject struct {
	env  *EvalCtx
	code *ObjectExpr
}

func (self *UserObject) String() string {
	return fmt.Sprintf("<%v>", self.code.name)
}

func (self *UserObject) recv(verb string, args []Object, nargs []NamedArg) (reply Object, err error) {
	arity := len(args)
	for _, meth := range self.code.methods {
		if arity == len(meth.params) && meth.verb == verb {
			e := EvalCtx{make(map[string]Object), self.env}
			for px, param := range meth.params {
				err := e.matchBind(param, args[px])
				if err != nil {
					return nil, err
				}
			}
			return e.run(meth.body)
		}
	}
	return nil, errors.New("@@refused")
}


func (self *EvalCtx) String() string {
	return fmt.Sprintf("%v in (%v)", self.locals, self.parent)
}

func (ctx *EvalCtx) run(expr Expr) (Object, error) {
	// fmt.Fprintf(os.Stderr, "eval %v in %v\n", expr, ctx)
	switch it := expr.(type) {
	case *IntExpr:
		return &IntObj{it.value}, nil
	case *NounExpr:
		value, ok := ctx.lookup(it.name)
		if ok {
			return value, nil
		}
		return nil, errors.New("@@not bound: " + it.name)
	case *CallExpr:
		rx, err := ctx.run(it.target)
		if err != nil {
			return nil, err
		}
		params := make([]Object, len(it.args))
		for ix, arg := range it.args {
			params[ix], err = ctx.run(arg)
			if err != nil {
				return nil, err
			}
		}
		return rx.recv(it.verb, params, []NamedArg{})

	case *ObjectExpr:
		obj := UserObject{ctx, it}
		err := ctx.matchBind(it.name, &obj)
		if err != nil {
			return nil, err
		}
		return &obj, nil
	}
	return nil, errors.New(fmt.Sprintf("@@eval not implemented for %v", expr))
}

func (self *EvalCtx) lookup(name string) (Object, bool) {
	value, ok := self.locals[name]
	if ok {
		return value, ok
	}
	if self.parent == nil {
		return nil, false
	} else {
		return self.parent.lookup(name)
	}
}

func (ctx *EvalCtx) matchBind(patt Pattern, specimen Object) error {
	// fmt.Printf("matchBind(%v, %v)\n", patt, specimen)
	switch it := patt.(type) {
	case *FinalPatt:
		ctx.locals[it.name] = specimen
		return nil
	}
	return errors.New(fmt.Sprintf("@@matchBind not implmented for %v", patt))
}
