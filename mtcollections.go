package monte

type Any interface{}

// ListObj
type ConstList struct {
	objs []Any
	// TODO: isSettled
}

type ListMaker struct {
}

func (*ListMaker) Run(args ...Any) (Any, error) {
	return &ConstList{args}, nil
}


