package monte

import (
	"fmt"
	"strings"
)

type ConstList struct {
	objs []Any
	// TODO: isSettled
}

type ConstMap struct {
	objectMap map[Any]Any
	keys      *ConstList // maintain insertion order
}

type ListMaker struct {
	// ISSUE: cache the empty list?
}

type MapMaker struct {
	// ISSUE: cache the empty map?
}

func (*ListMaker) String() string {
	return "_makeList"
}

func (*ListMaker) Run(args ...Any) (Any, error) {
	return &ConstList{args}, nil
}

func (list *ConstList) String() string {
	return "[" + printObjects(", ", list.objs...) + "]"
}

func printObjects(sep string, objects ...Any) string {
	parts := make([]string, len(objects))
	for ix, object := range objects {
		parts[ix] = fmt.Sprintf("%s", object)
	}
	return strings.Join(parts, sep)
}

func (*MapMaker) String() string {
	return "_makeMap"
}

func (*MapMaker) FromPairs(pairs ...Any) (Any, error) {
	keys := make([]Any, len(pairs))
	m := make(map[Any]Any)
	for ix, pair := range pairs {
		switch keyValue := pair.(type) {
		case *ConstList:
			if len(keyValue.objs) != 2 {
				return nil, fmt.Errorf("bad pair length: %v", keyValue)
			}
			key := keyValue.objs[0]
			value := keyValue.objs[1]
			keys[ix] = key
			m[key] = value
		default:
			return nil, fmt.Errorf("pair must be ConstList: %v", pair)
		}
	}
	return &ConstMap{m, &ConstList{keys}}, nil
}
