package monte

import "fmt"

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

	xCall := CallExpr{&NounExpr{"x"}, "b", []Expr{&NounExpr{"n"}}}
	bodyA := CallExpr{&NounExpr{"n"}, "add", []Expr{&xCall}}
	l2 := IntLit{2}
	bodyB := CallExpr{&NounExpr{"n"}, "subtract", []Expr{&l2}}
	nPatt := FinalPatt{"n", nil}
	ma := Method{"a", []Pattern{&nPatt}, nil, &bodyA}
	mb := Method{"b", []Pattern{&nPatt}, nil, &bodyB}
	o1 := ObjectExpr{&FinalPatt{"x", nil}, nil, []Method{ma, mb}}
	l3 := IntLit{3}
	c3 := CallExpr{&NounExpr{"x"}, "a", []Expr{&l3}}

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
