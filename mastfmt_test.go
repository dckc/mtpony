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

//go:generate go run tools/includetxt.go

func ExampleLoad() {
	// Output:
	// expr: def ITERATIONS :Int := 170 err: <nil>

	input := []byte{
		'M', 'o', 'n', 't', 0xe0, 'M', 'A', 'S', 'T', 0x01,
		'N', 0x03, 'I', 'n', 't', 'S', 0x01, 0x10, 0x01, 0x13,
		'P', 'F', 0x0a, 'I', 'T', 'E', 'R', 'A', 'T', 'I', 'O', 'N', 'S', 0x00, 'B', 0x01, 0x00, 0x01, 0x13,
		'L', 'N', 'B', 0x00, 0x00, 0x00, 0x00,
		'L', 'I', 0xd4, 0x02, 'B', 0x01, 0x17, 0x02, 0x1a,
		'D', 0x00, 0x01, 0x02, 'B', 0x01, 0x00, 0x02, 0x1a,
	}
	r := bytes.NewReader(input)
	expr, err := Load(r)
	fmt.Printf("expr: %v err: %v\n", expr, err)
}
