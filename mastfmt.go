package monte

import (
	"bufio"
	"bytes"
	"encoding/binary"
	"fmt"
	"io"
	// "log"
)

// MAGIC marks a Monte AST file.
const MAGIC = "Mont\xe0MAST"

func Load(r io.Reader) (Expr, error) {
	input := bufio.NewReader(r)

	checkMagic := func() error {
		actual := make([]byte, len(MAGIC))
		if _, err := io.ReadAtLeast(input, actual, len(MAGIC)); err != nil {
			return err
		}
		if bytes.Equal(actual, []byte(MAGIC)) {
			return nil
		}
		return fmt.Errorf("@@expected %q; got %q", MAGIC, actual)
	}
	if err := checkMagic(); err != nil {
		return nil, err
	}

	version, err := input.ReadByte()
	if err != nil {
		return nil, err
	}

	ctx := context{[]Expr{}, []Pattern{}}
	return ctx.decode(input, version)
}

type context struct {
	exprs []Expr
	patts []Pattern
}

func (ctx *context) decode(input *bufio.Reader, version byte) (Expr, error) {
	nextStr := func() (string, error) {
		size, err := binary.ReadUvarint(input)
		if err != nil {
			return "", err
		}
		buf := make([]byte, size)
		if _, err := io.ReadAtLeast(input, buf, int(size)); err != nil {
			return "", err
		}
		return string(buf[:]), nil
	}

	// TODO: actually handle spans
	skipSpan := func() error {
		if _, err := input.ReadByte(); err != nil {
			return err
		}
		for part := 0; part < 4; part++ {
			_, err := binary.ReadUvarint(input)
			if err != nil {
				return err
			}
		}
		return nil
	}

	nextExprOpt := func() (Expr, error) {
		ix, err := binary.ReadUvarint(input)
		if err != nil {
			return nil, err
		}
		return ctx.exprs[ix], nil
	}

	pushExpr := func(e Expr) {
		// log.Printf("pushExpr(%v)\n", e)
		ctx.exprs = append(ctx.exprs, e)
	}

	nextPatt := func() (Pattern, error) {
		ix, err := binary.ReadUvarint(input)
		if err != nil {
			return nil, err
		}
		return ctx.patts[ix], nil
	}

	pushPatt := func(p Pattern) {
		// log.Printf("pushPatt(%v)\n", p)
		ctx.patts = append(ctx.patts, p)
	}

	loadLiteral := func() error {
		tag, err := input.ReadByte()
		if err != nil {
			return err
		}

		switch tag {
		case 'N':
			pushExpr(nil)
		case 'I':
			value, err := binary.ReadVarint(input)
			if err != nil {
				return err
			}
			pushExpr(&IntExpr{value})
		default:
			return fmt.Errorf("@@literal tag not implemented: %q", tag)
		}
		err = skipSpan()
		return err
	}

	loadPattern := func() error {
		tag, err := input.ReadByte()
		if err != nil {
			return err
		}

		switch tag {
		case 'I':
			guard, err := nextExprOpt()
			if err != nil {
				return err
			}
			pushPatt(&IgnorePatt{guard})
		case 'F':
			name, err := nextStr()
			if err != nil {
				return err
			}
			guard, err := nextExprOpt()
			if err != nil {
				return err
			}
			pushPatt(&FinalPatt{name, guard})
		default:
			return fmt.Errorf("@@pattern tag not implemented: %q", tag)
		}
		skipSpan()
		return err
	}

	getExprs := func() ([]Expr, error) {
		qty, err := binary.ReadUvarint(input)
		// TODO: assert qty fits in int
		if err != nil {
			return nil, err
		}
		out := make([]Expr, qty)
		for ox := 0; ox < int(qty); ox++ {
			ix, err := binary.ReadUvarint(input)
			if err != nil {
				return nil, err
			}
			out[ox] = ctx.exprs[ix]
		}
		return out, nil
	}
	getNamedArgs := func() ([]Expr, error) {
		qty, err := binary.ReadUvarint(input)
		if err != nil {
			return nil, err
		}
		if qty > 0 {
			panic("@@named args not implemented")
		}
		return []Expr{}, nil
	}

	for true {
		tag, err := input.ReadByte()
		if err == io.EOF {
			last := len(ctx.exprs) - 1
			if last >= 0 {
				return ctx.exprs[last], nil
			}
		}
		if err != nil {
			return nil, err
		}

		switch tag {
		case 'L':
			err := loadLiteral()
			if err != nil {
				return nil, err
			}
		case 'N':
			name, err := nextStr()
			if err != nil {
				return nil, err
			}
			if err = skipSpan(); err != nil {
				return nil, err
			}
			pushExpr(&NounExpr{name})
		case 'D':
			patt, err := nextPatt()
			if err != nil {
				return nil, err
			}
			exit, err := nextExprOpt()
			if err != nil {
				return nil, err
			}
			expr, err := nextExprOpt()
			if err != nil {
				return nil, err
			}
			if err = skipSpan(); err != nil {
				return nil, err
			}
			pushExpr(&DefExpr{patt, exit, expr})
		case 'P':
			if err = loadPattern(); err != nil {
				return nil, err
			}
		case 'C':
			rx, err := nextExprOpt()
			if err != nil {
				return nil, err
			}
			verb, err := nextStr()
			if err != nil {
				return nil, err
			}
			args, err := getExprs()
			if err != nil {
				return nil, err
			}
			if _, err = getNamedArgs(); err != nil {
				return nil, err
			}
			if err = skipSpan(); err != nil {
				return nil, err
			}
			pushExpr(&CallExpr{rx, verb, args})
		default:
			return nil, fmt.Errorf("@@tag not implemented: %q", tag)
		}
	}

	return nil, fmt.Errorf("@@TODO")
}
