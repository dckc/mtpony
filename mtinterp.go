package monte

import (
	"errors"
	"fmt"
	"log"
	"reflect"
	"strings"
)

type Scope map[string]interface{}

var noSpecimen = Stub{"noSpecimen"}

type Thrower struct {
}

type scopeStack struct {
	locals Scope
	parent *scopeStack
}

type evaluator struct {
	scopes         *scopeStack
	specimen       Any
	patternFailure Any
	throw          *Thrower
}

type NamedArg struct {
	name  string
	value interface{}
}

type UserObject struct {
	env  *scopeStack
	code *ObjectExpr
}

func Evaluate(expr Expr, scope Scope) (interface{}, error) {
	value, _, err := EvalAndBind(expr, scope)
	return value, err
}

func EvalAndBind(expr Expr, scope Scope) (Any, Scope, error) {
	// ISSUE: throw, ok := scope["throw"]
	throw := &Thrower{}

	emptyLocals := make(Scope)
	interp := evaluator{
		&scopeStack{emptyLocals, &scopeStack{scope, nil}},
		noSpecimen,
		throw,
		throw,
	}
	value, err := interp.run(expr)
	if err != nil {
		return nil, nil, err
	}
	return value, interp.scopes.locals, nil
}

func (obj *UserObject) String() string {
	return fmt.Sprintf("<%v>", obj.code.name)
}

func (f *Thrower) String() string {
	return "throw"
}

func (f *Thrower) Run(payload Any) (Any, error) {
	return nil, fmt.Errorf("@@TODO: throw(%v)", payload)
}

func (f *Thrower) Eject(ej Any, payload Any) (Any, error) {
	return nil, fmt.Errorf("@@TODO: throw.eject(%v, %v)", ej, payload)
}

func MCall(rx interface{}, verb string, args []Any, nargs []NamedArg) (interface{}, error) {
	log.Printf("MCall: %v.%v(%v %v)", rx, verb, args, nargs)
	switch obj := rx.(type) {
	case *UserObject:
		return obj.recv(verb, args, nargs)
	}

	value := reflect.ValueOf(rx)
	fn := value.MethodByName(strings.Title(verb)) // KLUDGE: go exports are capitalized
	if !fn.IsValid() {
		return nil, fmt.Errorf("@@refused: %v.%v(%v %v) (%v)", rx, verb, args, nargs,
			strings.Title(verb))
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
	if !results[1].IsNil() {
		err = results[1].Interface().(error)
	}
	return result, err
}

func (obj *UserObject) recv(verb string, args []Any, nargs []NamedArg) (reply interface{}, err error) {
	arity := len(args)
	for _, meth := range obj.code.methods {
		if arity == len(meth.params) && meth.verb == verb {
			if meth.guardOpt != nil {
				log.Printf("WARNING! method guard not implemented: %v", meth.guardOpt)
			}
			throw, foundThrow := obj.env.lookup("throw")
			if !foundThrow {
				// ISSUE: object's vat's throw?
				throw = &Thrower{}
			}
			e := evaluator{obj.env, noSpecimen, throw, throw.(*Thrower)}
			for px, param := range meth.params {
				err := e.matchBind(param, args[px], throw)
				if err != nil {
					return nil, err
				}
			}
			return e.run(meth.body)
		}
	}
	return nil, errors.New("@@refused")
}

func (ctx *scopeStack) String() string {
	return fmt.Sprintf("%v in (%v)", ctx.locals, ctx.parent)
}

func (frame *evaluator) run(expr Expr) (interface{}, error) {
	// fmt.Fprintf(os.Stderr, "eval %v in %v\n", expr, ctx)
	switch it := expr.(type) {
	case *IntLit:
		return &IntObj{it.value}, nil
	case *StrLit:
		return &StrObj{it.value}, nil
	case *NounExpr:
		value, ok := frame.scopes.lookup(it.name)
		if ok {
			return value, nil
		}
		return nil, errors.New("@@not bound: " + it.name)
	case *DefExpr:
		ex := Any(frame.throw)
		if it.exit != nil {
			var err error
			ex, err = frame.run(it.exit)
			if err != nil {
				return nil, err
			}
		}
		rhs, err := frame.run(it.expr)
		if err != nil {
			return nil, err
		}
		err = frame.matchBind(it.patt, rhs, ex)
		if err != nil {
			return nil, err
		}
		return rhs, nil
	case *HideExpr:
		return frame.withFreshScope(
			func() (Any, error) { return frame.run(it.body) })
	case *CallExpr:
		rx, err := frame.run(it.target)
		if err != nil {
			return nil, err
		}
		params := make([]Any, len(it.args))
		for ix, arg := range it.args {
			params[ix], err = frame.run(arg)
			if err != nil {
				return nil, err
			}
		}
		return MCall(rx, it.verb, params, []NamedArg{})

	case *SeqExpr:
		var rv interface{}
		for _, expr := range it.exprs {
			result, err := frame.run(expr)
			if err != nil {
				return nil, err
			}
			rv = result
		}
		return rv, nil
	case *ObjectExpr:
		obj := UserObject{frame.scopes, it}
		err := frame.matchBind(it.name, &obj, frame.throw)
		if err != nil {
			return nil, err
		}
		return &obj, nil
	case *MetaContextExpr:
		return &MetaContextObj{}, nil
	}

	return nil, fmt.Errorf("@@eval not implemented for %v", expr)
}

func (ctx *scopeStack) lookup(name string) (interface{}, bool) {
	value, ok := ctx.locals[name]
	if ok {
		return value, ok
	}
	if ctx.parent == nil {
		return nil, false
	}
	return ctx.parent.lookup(name)
}

func (frame *evaluator) withFreshScope(thunk func() (Any, error)) (Any, error) {
	frame.scopes = &scopeStack{make(Scope), frame.scopes}
	value, err := thunk()
	frame.scopes = frame.scopes.parent
	return value, err
}

func (ctx *evaluator) matchBind(patt Pattern, specimen interface{}, ej Any) error {
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
		ctx.scopes.locals[it.name] = specimen
		return nil
	case *ViaPatt:
		ej := ctx.patternFailure
		v, err := ctx.run(it.expr)
		if err != nil {
			return err
		}
		newSpec, err := MCall(v, "run", []Any{specimen, ej}, []NamedArg{})
		if err != nil {
			return err
		}
		// semantics could be clearer that we use the same ejector below.
		return ctx.matchBind(it.patt, newSpec, ej)
	case *ListPatt:
		// Kernel list patterns have no tail
		l, err := unwrapList(specimen)
		if err != nil {
			problem := &StrObj{err.Error()}
			_, err := ctx.throw.Eject(ctx.patternFailure, problem)
			return err
		}
		if len(l) != len(it.items) {
			problem := &StrObj{
				fmt.Sprintf("Failed list pattern (needed %v, got %v)",
					len(it.items), len(l))}
			_, err := ctx.throw.Eject(ctx.patternFailure, problem)
			return err
		}
		for ix, patt := range it.items {
			err := ctx.matchBind(patt, l[ix], ej)
			if err != nil {
				return err
			}
		}
		return nil
	}
	return fmt.Errorf("@@matchBind not implmented for %v", patt)
}

type MetaContextObj struct {
}

func (*MetaContextObj) String() string {
	return "<context of @@...>"
}

func (*MetaContextObj) GetFQNPrefix() (Any, error) {
	log.Printf("WARNING! getFQNPrefix not implemented for meta.context()")
	return &StrObj{"@@prefix"}, nil
}
