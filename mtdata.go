package monte

import (
	"errors"
	"fmt"
)

type IntObj struct {
	value int64 // TODO: bignum
}

func (it *IntObj) String() string {
	return fmt.Sprintf("%v", it.value)
}

func (it *IntObj) recv(verb string, args []Object, nargs []NamedArg) (reply Object, err error) {
	switch {
	case verb == "add" && len(args) == 1:
		that, err := unwrapInt(args[0])
		if err != nil {
			return nil, err
		}
		return &IntObj{it.value + that}, nil

	case verb == "subtract" && len(args) == 1:
		that, err := unwrapInt(args[0])
		if err != nil {
			return nil, err
		}
		return &IntObj{it.value - that}, nil
	}
	return nil, errors.New("@@refused")
}

func unwrapInt(obj Object) (int64, error) {
	switch it := obj.(type) {
	case *IntObj:
		return it.value, nil
	}
	return 0, errors.New("@@not an int")
}
