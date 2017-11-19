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

func (m *ConstMap) String() string {
	parts := make([]string, len(m.objectMap))
	for ix, key := range m.keys.objs {
		parts[ix] = fmt.Sprintf("%s => %s", key, m.objectMap[key])
	}
	return "[" + strings.Join(parts, ", ") + "]"
}

func (*MapMaker) FromPairs(arg Any) (Any, error) {
	switch pairs := arg.(type) {
	case *ConstList:
		keys := make([]Any, len(pairs.objs))
		m := make(map[Any]Any)
		for ix, pair := range pairs.objs {
			switch keyValue := pair.(type) {
			case *ConstList:
				if len(keyValue.objs) != 2 {
					return nil, fmt.Errorf("bad pair length: %v", keyValue)
				}
				key := keyValue.objs[0]
				value := keyValue.objs[1]
				// log.Printf("map key: %v / value: %v", key, value)
				keys[ix] = key
				// TODO: check settledness
				m[key] = value
			default:
				return nil, fmt.Errorf("pair must be ConstList: %v", pair)
			}
		}
		value := &ConstMap{m, &ConstList{keys}}
		// log.Printf("made map: %v (%v / %v)", value, m, keys)
		return value, nil
	default:
		return nil, fmt.Errorf("arg be ConstList: %v", arg)
	}
}
