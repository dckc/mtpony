package monte

import (
	"log"
)

type Stub struct {
	name string
}

func (stub *Stub) Run(args ...Any) (Any, error) {
	log.Printf("WARNING! stub: %v", stub.name)
	return &StrObj{stub.name}, nil
}
