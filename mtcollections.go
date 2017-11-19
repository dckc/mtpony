package monte

import (
	"fmt"
	"strings"
)

type ConstList struct {
	objs []Any
	// TODO: isSettled
}

// maintain insertion order
type ConstMap struct {
	keys   *ConstList
	values *ConstList
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

func (list *ConstList) withoutPos(pos int) *ConstList {
	return &ConstList{append(list.objs[0:pos], list.objs[pos+1:]...)}
}

func (*MapMaker) String() string {
	return "_makeMap"
}

func (m *ConstMap) String() string {
	parts := make([]string, len(m.keys.objs))
	for ix, key := range m.keys.objs {
		parts[ix] = fmt.Sprintf("%s => %s", key, m.values.objs[ix])
	}
	return "[" + strings.Join(parts, ", ") + "]"
}

func (*MapMaker) FromPairs(arg Any) (Any, error) {
	switch pairs := arg.(type) {
	case *ConstList:
		keys := make([]Any, len(pairs.objs))
		values := make([]Any, len(pairs.objs))
		for ix, pair := range pairs.objs {
			switch keyValue := pair.(type) {
			case *ConstList:
				if len(keyValue.objs) != 2 {
					return nil, fmt.Errorf("bad pair length: %v", keyValue)
				}
				// TODO: check settledness
				keys[ix] = keyValue.objs[0]
				values[ix] = keyValue.objs[1]
			default:
				return nil, fmt.Errorf("pair must be ConstList: %v", pair)
			}
		}
		value := &ConstMap{&ConstList{keys}, &ConstList{values}}
		// log.Printf("made map: %v (%v / %v)", value, m, keys)
		return value, nil
	default:
		return nil, fmt.Errorf("arg be ConstList: %v", arg)
	}
}

func (m *ConstMap) Get(key Any) (Any, error) {
	pos, err := m.find(key)
	if err != nil {
		return nil, err
	}
	if pos < 0 {
		return nil, fmt.Errorf("key not in map@@")
	}
	return m.values.objs[pos], nil
}

func (m *ConstMap) find(key Any) (int, error) {
	for ix, k := range m.keys.objs {
		same, err := SameEver(k, key)
		if err != nil {
			return -1, err
		}
		if same {
			return ix, nil
		}
	}
	return -1, nil
}

func (m *ConstMap) Without(key Any) (Any, error) {
	pos, err := m.find(key)
	if err != nil {
		return nil, err
	}
	if pos < 0 {
		return m, nil
	}

	keys := m.keys.withoutPos(pos)
	values := m.values.withoutPos(pos)
	return &ConstMap{keys, values}, nil
}
