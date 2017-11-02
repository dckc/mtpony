package monte

import (
	"errors"
	"fmt"
)

type Evaluator interface {
	run(expr Expr) (Object, error)
	matchBind(patt Pattern, val Object) error // TODO: ejector
}

type evalCtx struct {
	locals map[string]Object
	parent *evalCtx
}

type Object interface {
	recv(verb string, args []Object, nargs []NamedArg) (reply Object, err error)
}
type NamedArg struct {
	name  string
	value Object
}

type UserObject struct {
	env  *evalCtx
	code *ObjectExpr
}

func (obj *UserObject) String() string {
	return fmt.Sprintf("<%v>", obj.code.name)
}

func (obj *UserObject) recv(verb string, args []Object, nargs []NamedArg) (reply Object, err error) {
	arity := len(args)
	for _, meth := range obj.code.methods {
		if arity == len(meth.params) && meth.verb == verb {
			if (meth.guardOpt != nil) {
				panic("method guard not implemented")
			}
			e := evalCtx{make(map[string]Object), obj.env}
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


func (ctx *evalCtx) String() string {
	return fmt.Sprintf("%v in (%v)", ctx.locals, ctx.parent)
}

func (ctx *evalCtx) run(expr Expr) (Object, error) {
	// fmt.Fprintf(os.Stderr, "eval %v in %v\n", expr, ctx)
	switch it := expr.(type) {
	case *IntLit:
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
	return nil, fmt.Errorf("@@eval not implemented for %v", expr)
}

func (ctx *evalCtx) lookup(name string) (Object, bool) {
	value, ok := ctx.locals[name]
	if ok {
		return value, ok
	}
	if ctx.parent == nil {
		return nil, false
	}
	return ctx.parent.lookup(name)
}

func (ctx *evalCtx) matchBind(patt Pattern, specimen Object) error {
	// fmt.Printf("matchBind(%v, %v)\n", patt, specimen)
	switch it := patt.(type) {
	case *FinalPatt:
		ctx.locals[it.name] = specimen
		return nil
	}
	return fmt.Errorf("@@matchBind not implmented for %v", patt)
}
