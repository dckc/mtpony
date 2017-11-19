package monte

import (
	"errors"
	"fmt"
	"log"
	"reflect"
	"strings"
)

type Evaluator interface {
	run(expr Expr) (interface{}, error)
	matchBind(patt Pattern, val interface{}) error // TODO: ejector
}

type evalCtx struct {
	locals map[string]interface{}
	parent *evalCtx
}

type NamedArg struct {
	name  string
	value interface{}
}

type UserObject struct {
	env  *evalCtx
	code *ObjectExpr
}

func (obj *UserObject) String() string {
	return fmt.Sprintf("<%v>", obj.code.name)
}

func MCall(rx interface{}, verb string, args []interface{}, nargs []NamedArg) (interface{}, error) {
	log.Printf("MCall: %v.%v(%v %v)", rx, verb, args, nargs)
	switch obj := rx.(type) {
	case *UserObject:
		return obj.recv(verb, args, nargs)
	}

	value := reflect.ValueOf(rx)
	fn := value.MethodByName(strings.Title(verb))  // KLUDGE: go exports are capitalized
	if !fn.IsValid() {
		return nil, fmt.Errorf("@@refused: %v.%v(%v %v)", rx, verb, args, nargs)
	}
	if len(nargs) > 0 {
		panic("named args not implemented for MCall")
	}
	goArgs := make([]reflect.Value, len(args))
	for ix, arg := range args {
		goArgs[ix] = reflect.ValueOf(arg)
	}
	// log.Printf("%v (Kind: %v) / %v.Call(%v)", value, value.Kind(), fn, goArgs)
	results := fn.Call(goArgs)
	var err error
	if len(results) != 2 {
		return nil, fmt.Errorf("expected (interface{}, error) got: %v", results)
	}
	result := results[0].Interface()
	if ! results[1].IsNil() {
		err = results[1].Interface().(error)
	}
	return result, err
}

func (obj *UserObject) recv(verb string, args []interface{}, nargs []NamedArg) (reply interface{}, err error) {
	arity := len(args)
	for _, meth := range obj.code.methods {
		if arity == len(meth.params) && meth.verb == verb {
			if meth.guardOpt != nil {
				log.Printf("WARNING! method guard not implemented: %v", meth.guardOpt)
			}
			e := evalCtx{make(map[string]interface{}), obj.env}
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

func (ctx *evalCtx) run(expr Expr) (interface{}, error) {
	// fmt.Fprintf(os.Stderr, "eval %v in %v\n", expr, ctx)
	switch it := expr.(type) {
	case *IntLit:
		return &IntObj{it.value}, nil
	case *StrLit:
		return &StrObj{it.value}, nil
	case *NounExpr:
		value, ok := ctx.lookup(it.name)
		if ok {
			return value, nil
		}
		return nil, errors.New("@@not bound: " + it.name)
	case *DefExpr:
		rhs, err := ctx.run(it.expr)
		if err != nil {
			return nil, err
		}
		err = ctx.matchBind(it.patt, rhs)
		if err != nil {
			return nil, err
		}
		return rhs, nil
	case *CallExpr:
		rx, err := ctx.run(it.target)
		if err != nil {
			return nil, err
		}
		params := make([]interface{}, len(it.args))
		for ix, arg := range it.args {
			params[ix], err = ctx.run(arg)
			if err != nil {
				return nil, err
			}
		}
		return MCall(rx, it.verb, params, []NamedArg{})

	case *SeqExpr:
		var rv interface{}
		for _, expr := range it.exprs {
			result, err := ctx.run(expr)
			if err != nil {
				return nil, err
			}
			rv = result
		}
		return rv, nil
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

func (ctx *evalCtx) lookup(name string) (interface{}, bool) {
	value, ok := ctx.locals[name]
	if ok {
		return value, ok
	}
	if ctx.parent == nil {
		return nil, false
	}
	return ctx.parent.lookup(name)
}

func (ctx *evalCtx) matchBind(patt Pattern, specimen interface{}) error {
	// fmt.Printf("matchBind(%v, %v)\n", patt, specimen)
	switch it := patt.(type) {
	case *IgnorePatt:
		if it.guard != nil {
			log.Printf("WARNING! TODO guard checking: %v: %v", specimen, it.guard)
		}
		return nil
	case *FinalPatt:
		if it.guard != nil {
			log.Printf("WARNING! TODO guard checking: %v: %v", specimen, it.guard)
		}
		ctx.locals[it.name] = specimen
		return nil
	}
	return fmt.Errorf("@@matchBind not implmented for %v", patt)
}
