package monte

import (
	"bytes"
	"fmt"
	"log"
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

	emptyScope := make(map[string]interface{})
	result, err := Evaluate(&o1, emptyScope)
	if err != nil {
		fmt.Println(err)
	} else {
		fmt.Println(result)
	}

	fmt.Println(&c3)
	result, err = Evaluate(&c3, emptyScope)
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

type Package struct {
}

func (pkg *Package) Import(petname interface{}) (interface{}, error) {
	log.Printf("import: %v", petname)
	return nil, fmt.Errorf("@@import: sad trombone.")
}

func (pkg *Package) String() string {
	return "<package>"
}

func ExampleModule() {
	input := bytes.NewReader([]byte(brot2))
	expr, err := Load(input)
	if err != nil {
		fmt.Println(err)
		return
	}

	// fmt.Printf("expr: %v\n", expr)
	safeScope := map[string]interface{}{} // TODO
	module, err := Evaluate(expr, safeScope)
	if err != nil {
		fmt.Println(err)
		return
	}

	package1 := Package{&fileRead{".", os.Open}}
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
