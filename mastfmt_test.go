package monte

import (
	"bytes"
	"fmt"
	"strings"
)

func ExampleMAGIC() {
	// Output:
	// expr: <nil> err: EOF
	// expr: <nil> err: unexpected EOF
	// expr: <nil> err: @@expected "Mont\xe0MAST"; got "The Spani"

	inputs := []string{MAGIC, "short", "The Spanish Inquisition"}
	for _, s := range inputs {
		r := strings.NewReader(s)
		expr, err := Load(r)
		fmt.Printf("expr: %v err: %v\n", expr, err)
	}
}

//go:generate go run tools/include_files.go .mast test_data monte

func ExampleLoad() {
	// Output:
	// expr: def ITERATIONS :Int := 170 err: <nil>
	// module obj method verb: run
	// module obj method verb: dependencies
	// module body expr count: 4

	input := []byte(brot1)
	r := bytes.NewReader(input)
	expr, err := Load(r)
	fmt.Printf("expr: %v err: %v\n", expr, err)

	{
		input := []byte(brot2)
		r := bytes.NewReader(input)
		expr, err := Load(r)
		if err != nil {
			fmt.Printf("err: %v\n", err)
		}
		switch mob := expr.(type) {
		case *ObjectExpr:
			for _, method := range mob.methods {
				fmt.Printf("module obj method verb: %v\n", method.verb)
			}
			switch body := mob.methods[0].body.(type) {
			case *SeqExpr:
				fmt.Printf("module body expr count: %v", len(body.exprs))
			}
		}
	}
}
