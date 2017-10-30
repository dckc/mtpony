package monte

import (
	"bufio"
	"bytes"
	"encoding/binary"
	"errors"
	"fmt"
	"io"
	// "log"
)

const MAGIC = "Mont\xe0MAST"

func load(r io.Reader) (Expr, error) {
	input := bufio.NewReader(r)

	checkMagic := func () error {
		actual := make([]byte, len(MAGIC))
		if _, err := io.ReadAtLeast(input, actual, len(MAGIC)); err != nil {
			return err
		}
		if bytes.Equal(actual, []byte(MAGIC)) {
			return nil
		} else {
			return errors.New(
				fmt.Sprintf("@@expected %q; got %q", MAGIC, actual))
		}
	}
	if err := checkMagic(); err != nil {
		return nil, err
	}

	version, err := input.ReadByte()
	if err != nil {
		return nil, err
	}

	ctx := MASTContext{[]Expr{}, []Pattern{}}
	return ctx.decode(input, version)
}

type MASTContext struct {
	exprs []Expr
	patts []Pattern
}

func (self *MASTContext) decode(input *bufio.Reader, version byte) (Expr, error) {
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
		return self.exprs[ix], nil
	}

	pushExpr := func(e Expr) {
		// log.Printf("pushExpr(%v)\n", e)
		self.exprs = append(self.exprs, e)
	}

	nextPatt := func() (Pattern, error) {
		ix, err := binary.ReadUvarint(input)
		if err != nil {
			return nil, err
		}
		return self.patts[ix], nil
	}

	pushPatt := func(p Pattern) {
		// log.Printf("pushPatt(%v)\n", p)
		self.patts = append(self.patts, p)
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
			return errors.New(fmt.Sprintf("@@literal tag not implemented: %q", tag))
		}
		if err = skipSpan(); err != nil {
			return err
		}
		return nil
	}

	loadPattern := func() error {
		tag, err := input.ReadByte()
		if err != nil {
			return err
		}

		switch tag {
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
			return errors.New(fmt.Sprintf("@@pattern tag not implemented: %q", tag))
		}
		if err = skipSpan(); err != nil {
			return err
		}
		return nil
	}

	for true {
		tag, err := input.ReadByte()
		if err == io.EOF {
			last := len(self.exprs) - 1
			if last >= 0 {
				return self.exprs[last], nil
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
		default:
			return nil, errors.New(fmt.Sprintf("@@tag not implemented: %q", tag))
		}
	}

	return nil, errors.New(fmt.Sprintf("@@TODO"))
}
