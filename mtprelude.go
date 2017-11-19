package monte

import (
	"fmt"
	"log"
)

type Stub struct {
	name string
}

func (stub *Stub) Run(args ...Any) (Any, error) {
	log.Printf("WARNING! stub: %v", stub.name)
	return &StrObj{stub.name}, nil
}

type MapExtractObj struct {
	throw *Thrower
}
type MapExtractor struct {
	MapExtractObj
	key Any
}

func (obj *MapExtractObj) String() string {
	return "_mapExtract"
}

func (obj *MapExtractObj) Run(key Any) (Any, error) {
	return &MapExtractor{*obj, key}, nil
}

func (fn *MapExtractor) String() string {
	return fmt.Sprintf("_mapExtract(%s)", fn.key)
}

func (fn *MapExtractor) Run(specimen Any, ej Any) (Any, error) {
	switch m := specimen.(type) {
	case *ConstMap:
		item, err := m.Get(fn.key)
		if err != nil {
			problem := &StrObj{err.Error()}
			return fn.throw.Eject(ej, problem)
		}
		rest, _ := m.Without(fn.key)
		value := &ConstList{[]Any{item, rest}}
		return value, nil
	default:
		return MCall(fn.throw, "eject", []Any{"not a Map"}, []NamedArg{})
	}
}
