package monte

import "fmt"

func ExampleLetRec() {
	l1 := IntExpr{1}
	c1 := CallExpr{&NounExpr{"n"}, "subtract", []Expr{&l1}}
	c2 := CallExpr{&NounExpr{"x"}, "m", []Expr{&c1}}
	body := CallExpr{&l1, "add", []Expr{&c2}}
	p_n := FinalPatt{"n", nil}
	m1 := Method{"m", []Pattern{&p_n}, &body}
	o1 := ObjectExpr{&FinalPatt{"x", nil}, []Method{m1}}
	l3 := IntExpr{3}
	c3 := CallExpr{&o1, "m", []Expr{&l3}}

	// Output:
	// object x {
	//   method m(n) {
	//     1.add(x.m(n.subtract(1)))
	//   }
	// }
	// <x>
	// @@not bound: x

	fmt.Println(&o1)

	e := EvalCtx{make(map[string]Object), []map[string]Object{}}
	{
		result, err := e.run(&o1)
		if err != nil {
			fmt.Println(err)
		} else {
			fmt.Println(result)
		}
	}

	{
		result, err := e.run(&c3)
		if err != nil {
			fmt.Println(err)
		} else {
			fmt.Println(result)
		}
	}
}
