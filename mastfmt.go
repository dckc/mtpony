package monte

import (
	"bufio"
	"bytes"
	"encoding/binary"
	"fmt"
	"io"
	"log"
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
		if int(ix) >= len(ctx.exprs) {
			return nil, fmt.Errorf("MAST index %v out of bounds (only %v exprs)",
				ix, len(ctx.exprs))
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
		if int(ix) >= len(ctx.patts) {
			return nil, fmt.Errorf("MAST index %v out of bounds (only %v patterns)",
				ix, len(ctx.patts))
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
			pushExpr(&IntLit{value})
		case 'S':
			value, err := nextStr()
			if err != nil {
				return err
			}
			pushExpr(&StrLit{value})
		case 'D':
			var value float64
			err := binary.Read(input, binary.BigEndian, &value)
			if err != nil {
				return err
			}
			pushExpr(&DoubleLit{value})
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
		case 'L':
			qty, err := binary.ReadUvarint(input)
			if err != nil {
				return err
			}
			items := make([]Pattern, qty)
			for ix := 0; ix < int(qty); ix++ {
				item, err := nextPatt()
				if err != nil {
					return err
				}
				items[ix] = item
			}
			pushPatt(&ListPatt{items})
		case 'A':
			expr, err := nextExprOpt()
			if err != nil {
				return err
			}
			patt, err := nextPatt()
			if err != nil {
				return err
			}
			pushPatt(&ViaPatt{expr, patt})
		case 'V':
			name, err := nextStr()
			if err != nil {
				return err
			}
			guard, err := nextExprOpt()
			if err != nil {
				return err
			}
			pushPatt(&VarPatt{name, guard})
		default:
			return fmt.Errorf("@@pattern tag not implemented: %q", tag)
		}
		err = skipSpan()
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
			out[ox], err = nextExprOpt()
			if err != nil {
				return nil, err
			}
		}
		return out, nil
	}
	getNamedArgs := func() ([]NamedExpr, error) {
		qty, err := binary.ReadUvarint(input)
		if err != nil {
			return nil, err
		}
		items := make([]NamedExpr, qty)
		for ix := 0; ix < int(qty); ix++ {
			items[ix].name, err = nextExprOpt()
			if err != nil {
				return nil, err
			}

			items[ix].value, err = nextExprOpt()
			if err != nil {
				return nil, err
			}
		}
		return items, nil
	}
	getMethods := func() ([]Method, error) {
		exprs, err := getExprs()
		if err != nil {
			return nil, err
		}
		out := make([]Method, len(exprs))
		for mx, expr := range exprs {
			switch m := expr.(type) {
			case *Method:
				out[mx] = *m
			default:
				return nil, fmt.Errorf("expected method; got %v", expr)
			}
		}
		return out, nil
	}

	getPatterns := func() ([]Pattern, error) {
		qty, err := binary.ReadUvarint(input)
		// TODO: assert qty fits in int
		if err != nil {
			return nil, err
		}
		out := make([]Pattern, qty)
		for ox := 0; ox < int(qty); ox++ {
			ix, err := binary.ReadUvarint(input)
			if err != nil {
				return nil, err
			}
			out[ox] = ctx.patts[ix]
		}
		return out, nil
	}
	getNamedPatts := func() ([]NamedPattern, error) {
		qty, err := binary.ReadUvarint(input)
		if err != nil {
			return nil, err
		}
		items := make([]NamedPattern, qty)
		for ix := 0; ix < int(qty); ix++ {
			items[ix].key, err = nextExprOpt()
			if err != nil {
				return nil, err
			}

			items[ix].patt, err = nextPatt()
			if err != nil {
				return nil, err
			}

			items[ix].default_, err = nextExprOpt()
			if err != nil {
				return nil, err
			}
		}
		return items, nil
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
		case 'P':
			if err = loadPattern(); err != nil {
				return nil, err
			}
		case 'L':
			err := loadLiteral()
			if err != nil {
				return nil, err
			}
		case 'B':
			name, err := nextStr()
			if err != nil {
				return nil, err
			}
			if err = skipSpan(); err != nil {
				return nil, err
			}
			pushExpr(&BindingExpr{name})
		case 'N':
			name, err := nextStr()
			if err != nil {
				return nil, err
			}
			if err = skipSpan(); err != nil {
				return nil, err
			}
			pushExpr(&NounExpr{name})
		case 'A':
			target, err := nextStr()
			if err != nil {
				return nil, err
			}
			rhs, err := nextExprOpt()
			if err != nil {
				return nil, err
			}
			if err = skipSpan(); err != nil {
				return nil, err
			}
			pushExpr(&AssignExpr{target, rhs})
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
			namedArgs, err := getNamedArgs()
			if err != nil {
				return nil, err
			}
			if err = skipSpan(); err != nil {
				return nil, err
			}
			pushExpr(&CallExpr{rx, verb, args, namedArgs})
		case 'e':
			patt, err := nextPatt()
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
			pushExpr(&EscapeExpr{patt, expr})
		case 'I':
			cond, err := nextExprOpt()
			if err != nil {
				return nil, err
			}
			cons, err := nextExprOpt()
			if err != nil {
				return nil, err
			}
			alt, err := nextExprOpt()
			if err != nil {
				return nil, err
			}
			if err = skipSpan(); err != nil {
				return nil, err
			}
			pushExpr(&IfExpr{cond, cons, alt})
		case 'S':
			exprs, err := getExprs()
			if err != nil {
				return nil, err
			}
			if err = skipSpan(); err != nil {
				return nil, err
			}
			pushExpr(&SeqExpr{exprs})
		case 'O':
			doc, err := nextStr()
			if err != nil {
				return nil, err
			}
			if doc != "" {
				log.Printf("WARNING! object doc not implemented: %q", doc)
			}
			name, err := nextPatt()
			if err != nil {
				return nil, err
			}
			asExpr, err := nextExprOpt()
			if err != nil {
				return nil, err
			}
			implements, err := getExprs()
			if err != nil {
				return nil, err
			}
			if len(implements) > 0 {
				panic("implements not implemented")
			}
			methods, err := getMethods()
			if err != nil {
				return nil, err
			}
			matchers, err := getExprs()
			if err != nil {
				return nil, err
			}
			if len(matchers) > 0 {
				panic("@@matchers not implemented")
			}
			if err = skipSpan(); err != nil {
				return nil, err
			}
			pushExpr(&ObjectExpr{name, asExpr, methods})

		case 'M':
			doc, err := nextStr()
			if err != nil {
				return nil, err
			}
			if doc != "" {
				log.Printf("WARNING! method doc not implemented: %q", doc)
			}
			verb, err := nextStr()
			if err != nil {
				return nil, err
			}
			params, err := getPatterns()
			if err != nil {
				return nil, err
			}
			namedParams, err := getNamedPatts()
			if err != nil {
				return nil, err
			}
			guard, err := nextExprOpt()
			if err != nil {
				return nil, err
			}
			body, err := nextExprOpt()
			if err != nil {
				return nil, err
			}
			if err = skipSpan(); err != nil {
				return nil, err
			}
			pushExpr(&Method{verb, params, namedParams, guard, body})

		default:
			return nil, fmt.Errorf("@@tag not implemented: %q", tag)
		}
	}

	return nil, fmt.Errorf("@@TODO")
}
