package monte

import (
	"io"
	"log"
)

// ReadAccess is an interface to read a file or collection of files.
//
// Open opens the root file.
//
// Join provides access to a subcollection.
type ReadAccess interface {
	Open() (io.Reader, error)
	Join(other ...string) ReadAccess
}

func MakeSafeScope() Scope {
	return Scope{
		"_makeList": &ListMaker{},
	}
}

// Package provides the import method used by Monte modules.
type Package struct {
	modules   ReadAccess
	safeScope Scope
}

func (pkg *Package) Import(petname interface{}) (interface{}, error) {
	log.Printf("import: %v", petname)

	petnameStr, err := unwrapStr(petname)
	if err != nil {
		return nil, err
	}
	modRd, err := pkg.modules.Join(petnameStr + ".mast").Open()
	if err != nil {
		return nil, err
	}
	modAST, err := Load(modRd)
	if err != nil {
		return nil, err
	}
	module, err := Evaluate(modAST, pkg.safeScope)
	if err != nil {
		return nil, err
	}

	deps, err := MCall(module, "dependencies", []interface{}{}, []NamedArg{})
	if err != nil {
		return nil, err
	}
	log.Printf("deps: %v", deps)

	return MCall(module, "run", []interface{}{pkg}, []NamedArg{})
}

func (pkg *Package) String() string {
	return "<package>"
}
