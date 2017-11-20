package monte

import (
	"errors"
	"fmt"
)

type Any interface{}

type AnyGuard struct {
}

type NullObj struct {
}

type BoolObj struct {
	value bool
}

type IntObj struct {
	value int64 // TODO: bignum
}
type StrObj struct {
	value string
}

type DoubleGuard struct {
}

func (guard *AnyGuard) String() string {
	return "Any"
}

func (it *NullObj) String() string {
	return "null"
}

func (it *BoolObj) String() string {
	return fmt.Sprintf("%v", it.value) // TODO: test
}

func (it *IntObj) String() string {
	return fmt.Sprintf("%v", it.value)
}

func (it *IntObj) Add(j interface{}) (interface{}, error) {
	that, err := unwrapInt(j)
	if err != nil {
		return nil, err
	}
	return &IntObj{it.value + that}, nil
}

func (it *IntObj) Subtract(j interface{}) (interface{}, error) {
	that, err := unwrapInt(j)
	if err != nil {
		return nil, err
	}
	return &IntObj{it.value - that}, nil
}

func unwrapInt(obj interface{}) (int64, error) {
	switch it := obj.(type) {
	case *IntObj:
		return it.value, nil
	}
	return 0, errors.New("@@not an Int")
}

func (guard *DoubleGuard) String() string {
	return "Double"
}

func (it *StrObj) String() string {
	return fmt.Sprintf("%q", it.value)
}

func (it *StrObj) Add(s interface{}) (interface{}, error) {
	that, err := unwrapStr(s)
	if err != nil {
		return nil, err
	}
	return &StrObj{it.value + that}, nil
}

func wrapStr(s string) *StrObj {
	return &StrObj{s}
}

func unwrapStr(obj interface{}) (string, error) {
	switch it := obj.(type) {
	case *StrObj:
		return it.value, nil
	}
	return "", errors.New("@@not a Str")
}
