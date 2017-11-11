package monte

import (
	"errors"
	"fmt"
)

type IntObj struct {
	value int64 // TODO: bignum
}
type StrObj struct {
	value string
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


func (it *StrObj) String() string {
	return fmt.Sprintf("%q", it.value)
}

func (it *StrObj) add(s []interface{}) (interface{}, error) {
	that, err := unwrapStr(s)
	if err != nil {
		return nil, err
	}
	return &StrObj{it.value + that}, nil
}

func unwrapStr(obj interface{}) (string, error) {
	switch it := obj.(type) {
	case *StrObj:
		return it.value, nil
	}
	return "", errors.New("@@not a Str")
}
