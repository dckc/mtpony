package monte

import (
	"fmt"
	"strings"
)

func ExampleMASTMagic() {
	// Output:
	// expr: 0 err: <nil>
	// expr: <nil> err: unexpected EOF
	// expr: <nil> err: @@bad magic at 0: expected 'M'; got 'u'

	inputs := []string{MAGIC, "short", "unexpected gorp"}
	for _, s := range inputs {
		r := strings.NewReader(s)
		expr, err := load(r)
		fmt.Printf("expr: %v err: %v\n", expr, err)
	}
}
