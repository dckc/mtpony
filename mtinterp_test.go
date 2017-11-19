package monte

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"path"
)

func ExampleObjectExpr() {
	// Output:
	// object x {
	//    ""
	//   method a(n) {
	//    ""
	//     n.add(x.b(n))
	//   }
	//   method b(n) {
	//    ""
	//     n.subtract(2)
	//   }
	// }
	// <x>
	// x.a(3)
	// 4

	xCall := CallExpr{&NounExpr{"x"}, "b", []Expr{&NounExpr{"n"}},
		[]NamedExpr{}}
	bodyA := CallExpr{&NounExpr{"n"}, "add", []Expr{&xCall},
		[]NamedExpr{}}
	l2 := IntLit{2}
	bodyB := CallExpr{&NounExpr{"n"}, "subtract", []Expr{&l2},
		[]NamedExpr{}}
	nPatt := FinalPatt{"n", nil}
	noDoc := &StrObj{""}
	ma := Method{noDoc, "a", []Pattern{&nPatt}, nil, nil, &bodyA}
	mb := Method{noDoc, "b", []Pattern{&nPatt}, nil, nil, &bodyB}
	o1 := ObjectExpr{noDoc, &FinalPatt{"x", nil}, nil, []Method{ma, mb}}
	l3 := IntLit{3}
	c3 := CallExpr{&NounExpr{"x"}, "a", []Expr{&l3},
		[]NamedExpr{}}

	fmt.Println(&o1)

	emptyScope := make(map[string]interface{})
	result, locals, err := EvalAndBind(&o1, emptyScope)
	if err != nil {
		fmt.Println(err)
	} else {
		fmt.Println(result)
	}

	fmt.Println(&c3)
	result, err = Evaluate(&c3, locals)
	if err != nil {
		fmt.Println(err)
	} else {
		fmt.Println(result)
	}
}

func ExampleMASTDef() {
	// Output:
	// expr: def ITERATIONS :Int := 170
	// result: 170 err: <nil>
	input := bytes.NewReader([]byte(brot1))
	expr, err := Load(input)
	fmt.Printf("expr: %v\n", expr)
	result, err := Evaluate(expr, map[string]interface{}{})
	fmt.Printf("result: %v err: %v\n", result, err)
}

func ExampleModule() {
	input := bytes.NewReader([]byte(brot2))
	expr, err := Load(input)
	if err != nil {
		fmt.Println(err)
		return
	}

	// fmt.Printf("expr: %v\n", expr)
	safeScope := MakeSafeScope()
	module, err := Evaluate(expr, safeScope)
	if err != nil {
		fmt.Println(err)
		return
	}

	// KLUDGE: Using os.Open makes this an integration test, not a unit test.
	package1 := Package{&fileRead{".", os.Open}, safeScope}
	scope1 := map[string]interface{}{
		"brot":     module,
		"package1": &package1}
	runModule := CallExpr{&NounExpr{"brot"}, "run", []Expr{&NounExpr{"package1"}},
		[]NamedExpr{}}
	result, err := Evaluate(&runModule, scope1)
	if err != nil {
		fmt.Println(err)
		return
	}
	fmt.Printf("result: %v err: %v\n", result, err)
	// Output:
	// X
}

type fileRead struct {
	path string
	open func(path string) (*os.File, error)
}

func (rd *fileRead) Open() (io.Reader, error) {
	return rd.open(rd.path)
}

func (rd *fileRead) Join(other ...string) ReadAccess {
	parts := append([]string{rd.path}, other...)
	return &fileRead{path.Join(parts...), rd.open}
}
