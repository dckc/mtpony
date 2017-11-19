import "lib/complex" =~ [=> makeComplex :DeepFrozen]
exports (brotCount)

def ITERATIONS :Int := 170

def brotCount(a) :Int as DeepFrozen:
    var rv := makeComplex(0.0, 0.0)
    var i := ITERATIONS
    while (i > 0 && rv.abs() <= 2):
        rv := makeComplex(rv.real().abs(), rv.imag().abs())
        rv := rv * rv + a
        i -= 1
    # traceln(["Iterations", i, "Seed", a.real(), a.imag()])
    return i

