package monte

import (
	"log"
)

func SameEver(first Any, second Any) (bool, error) {
	switch v1 := first.(type) {
	case *StrObj:
		switch v2 := second.(type) {
		case *StrObj:
			return v1.value == v2.value, nil
		default:
			return false, nil
		}
	default:
		log.Printf("WARNING! TODO: SameEver for %v", first)
		return false, nil
	}
}
