package monte

import (
	"bytes"
	"fmt"
	"strings"
	"unicode/utf8"
)

// Confirm that '\x00' is OK in golang.
func ExampleChar0() {
	buf := []byte{0}
	r, size := utf8.DecodeRune(buf)
	fmt.Printf("rune: %v size: %v", r, size)
	// Output:
	// rune: 0 size: 1
}

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
	// module obj method verb: run
	// module obj method verb: dependencies
	// module body expr count: 19

	input := []byte(brot)
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
			fmt.Printf("module body expr count: %v\n", len(body.exprs))
		}
	}
}

func ExampleLoad2() {
	// Output:
	// expr: def ITERATIONS :Int := 170 err: <nil>
	input := []byte(brot1)
	r := bytes.NewReader(input)
	expr, err := Load(r)
	fmt.Printf("expr: %v err: %v\n", expr, err)
}

func ExampleLoad3() {
	// Output:
	// module obj method verb: run
	// module obj method verb: dependencies
	// module body expr count: 4
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
			fmt.Printf("module body expr count: %v\n", len(body.exprs))
		}
	}
}
