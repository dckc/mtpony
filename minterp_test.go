package monte

import "fmt"

func ExampleLetRec() {
	l1 := IntExpr{1}
	c1 := CallExpr{&NounExpr{"n"}, "subtract", []Expr{&l1}}
	c2 := CallExpr{&NounExpr{"x"}, "m", []Expr{&c1}}
	body := CallExpr{&l1, "add", []Expr{&c2}}
	m1 := Method{"m", []Pattern{&FinalPatt{"n", nil}}, &body}
	ex1 := ObjectExpr{&FinalPatt{"x", nil}, []Method{m1}}

	fmt.Println(&ex1)

	// Output:
	// object x {
	//   method m(n) {
	//     1.add(x.m(n.subtract(1)))
	//   }
	// }
}
