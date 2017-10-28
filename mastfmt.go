package monte

import (
	"errors"
	"fmt"
	"io"
)

const MAGIC = "Mont\xe0MAST\x00"

func load(input io.Reader) (Expr, error) {
	buf := make([]byte, len(MAGIC))
	if _, err := io.ReadAtLeast(input, buf, len(MAGIC)); err != nil {
		return nil, err
	}
	for ix, b := range []byte(MAGIC) {
		if buf[ix] != byte(b) {
			return nil, errors.New(
				fmt.Sprintf("@@bad magic at %v: expected %q; got %q", ix, byte(b), buf[ix]))
		}
	}
	return &IntExpr{0}, nil
}

type MASTContext struct {
	exprs []Expr
	patts []Pattern
}
