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

	input := []byte(brot1)
	r := bytes.NewReader(input)
	expr, err := Load(r)
	fmt.Printf("expr: %v err: %v\n", expr, err)
}
