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
	throw := &Thrower{}

	return Scope{
		"null":              &NullObj{},
		"true":              &BoolObj{true},
		"false":             &BoolObj{false},
		"Any":               &AnyGuard{},
		"Double":            &DoubleGuard{},
		"_makeList":         &ListMaker{},
		"_makeMap":          &MapMaker{},
		"throw":             throw,
		"_mapExtract":       &MapExtractObj{throw},
		"_makeProtocolDesc": &Stub{"_makeProtocolDesc"},
		"_makeMessageDesc":  &Stub{"_makeMessageDesc"},
		"_makeParamDesc":    &Stub{"_makeParamDesc"},
	}
}

// Package provides the import method used by Monte modules.
type Package struct {
	modules   ReadAccess
	safeScope Scope
	benches   []Any
}

func (pkg *Package) Import(petname interface{}) (interface{}, error) {
	log.Printf("import: %v", petname)

	petnameStr, err := unwrapStr(petname)
	if err != nil {
		return nil, err
	}

	if petnameStr == "bench" {
		// ISSUE: benchCollector from what module?
		return mkMap(wrapStr("bench"), &benchCollector{pkg}), nil
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

	deps, err := MCall(module, "dependencies", []Any{}, []NamedArg{})
	if err != nil {
		return nil, err
	}
	log.Printf("deps: %v", deps)

	return MCall(module, "run", []Any{pkg}, []NamedArg{})
}

func (pkg *Package) String() string {
	return "<package>"
}

type benchCollector struct {
	pkg *Package
}

func (b *benchCollector) String() string {
	return "<benchCollector>"
}

func (b *benchCollector) Run(aBench Any, name Any) (Any, error) {
	nameStr, err := unwrapStr(name)
	if err != nil {
		return nil, err
	}
	log.Printf("WARNING: not saving bench name %v", nameStr)
	pkg := b.pkg
	if pkg.benches == nil {
		pkg.benches = []Any{aBench}
	} else {
		pkg.benches = append(pkg.benches, aBench)
	}
	return pkg.safeScope["null"], nil
}
