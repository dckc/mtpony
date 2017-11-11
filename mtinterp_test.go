package monte

import (
	"bytes"
	"fmt"
)

func ExampleObjectExpr() {
	// Output:
	// object x {
	//   method a(n) {
	//     n.add(x.b(n))
	//   }
	//   method b(n) {
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
	ma := Method{"a", []Pattern{&nPatt}, nil, nil, &bodyA}
	mb := Method{"b", []Pattern{&nPatt}, nil, nil, &bodyB}
	o1 := ObjectExpr{&FinalPatt{"x", nil}, nil, []Method{ma, mb}}
	l3 := IntLit{3}
	c3 := CallExpr{&NounExpr{"x"}, "a", []Expr{&l3},
		[]NamedExpr{}}

	fmt.Println(&o1)

	e := evalCtx{make(map[string]Object), nil}
	result, err := e.run(&o1)
	if err != nil {
		fmt.Println(err)
	} else {
		fmt.Println(result)
	}

	fmt.Println(&c3)
	result, err = e.run(&c3)
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
	interp := evalCtx{map[string]Object{}, nil}
	result, err := interp.run(expr)
	fmt.Printf("result: %v err: %v\n", result, err)
}
