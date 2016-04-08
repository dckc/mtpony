import "lib/codec/utf8" =~ [=> UTF8 :DeepFrozen]
exports (main)

def mkLiteral(expr) as DeepFrozen:
    def v := expr.getValue()
    def mkLit := (
        if (v =~ n :Void) {
            "Monte.makeNull()"
        } else if (v =~ b :Bool) {
            `Monte.makeBool($b)`
        } else if (v =~ i :Int) {
            `Monte.makeInt("$i")`
        } else if (v =~ ch :Char) {
            `Monte.char(${ch.asInteger()})`
        } else if (v =~ d :Double) {
            `Monte.makeDouble($d)`
        } else if (v =~ s :Str) {
            `Monte.makeStr(${M.toQuote(s)})`
        } else {
            throw(v) })
    return object literal:
        to _printOn(out):
            out.print(mkLit)


def mkCall(expr, scope) as DeepFrozen:
    def list(exprs):
        if (exprs.size() == 0):
            return object emptyList:
                to _printOn(out):
                    out.print("Monte.emptyArgs()")
        def last := scope.asPony(exprs.last())
        def init := list(exprs.slice(0, exprs.size() - 1))
        return object cons:
            to _printOn(out):
                out.print("Monte.argsWith(")
                init._printOn(out)
                out.print(", ")
                last._printOn(out)
                out.print(")")
        
    def map(namedExprs):
        if (namedExprs.size() == 0):
            return object emptyList:
                to _printOn(out):
                    out.print("Monte.emptyNamedArgs()")
        def init := map(namedExprs.slice(0, namedExprs.size() - 1))
        def last := namedExprs.keys().last()
        def [n, v] := [scope.asPony(last), scope.asPony(namedExprs[last])]

        return object cons:
            to _printOn(out):
                out.print(`Monte.namedArgsWith(`)
                init._printOn(out)
                out.print(", ")
                n._printOn(out)
                out.print(", ")
                v._printOn(out)
                out.print(")")

    def rx := scope.asPony(expr.getReceiver())
    def verb := M.toQuote(expr.getVerb())
    def namedArgs := map(expr.getNamedArgs())
    def args := `$verb, ${list(expr.getArgs())}, $namedArgs`
    return object call:
        to _printOn(out):
            out.print(`($rx).call($args)`)


def mkScope(seq) as DeepFrozen:
    def bindings := [].diverge()

    def unify(scope, patt, exit_, expr):
        if (exit_ != null):
            throw("TODO: exit")

        switch (patt.getNodeName()):
            match =="FinalPattern":
                bindings.push(patt.getNoun().getName())
                # TODO: guard
                traceln("FinalPattern; bindings now:", bindings)
                def value := scope.asPony(expr)
                return object defExpr:
                    to _printOn(out):
                        out.print(`_bindings.push(`)
                        value._printOn(out)
                        out.print(")\n")
            match _:
                throw("TODO: non-final patterns")

    return object scope:
        to asPony(expr):
            traceln("expr node:", expr.getNodeName())

            return switch (expr.getNodeName()):
                match =="DefExpr":
                    unify(scope, expr.getPattern(),
                          expr.getExit(), expr.getExpr())
                match =="SeqExpr":
                    def pexprs := [for e in (expr.getExprs()) scope.asPony(e)]
                    object seq:
                        to _printOn(out):
                            out.print("(")
                            for px in (pexprs):
                                px._printOn(out)
                                out.print("\n")
                            out.print(")")

                match =="MethodCallExpr":
                    mkCall(expr, scope)

                match =="NounExpr":
                    def name := expr.getName()
                    def slotIx := bindings.indexOf(name)
                    traceln(`NounExpr: $bindings.indexOf($name) == $slotIx`)
                    if (slotIx < 0):
                        throw("not in scope: " + name)
                    object noun:
                        to _printOn(out):
                            out.print(`_slot($slotIx)  // $name$\n`)

                match =="LiteralExpr":
                    mkLiteral(expr)

def mkCounter() as DeepFrozen:
    var x := 0
    return object counter:
        to next():
            x += 1
            return x

def main(argv,
         => makeFileResource,
         => makeStdOut) as DeepFrozen:
    def [inf, module, outf] := {
        escape usage {
            def [infn, outfn] exit usage := argv
            def `@module.pony` exit usage := outfn
            [makeFileResource(infn), module, makeFileResource(outfn)]
        } catch _ {
            traceln("Usage: any.mt ModuleName.pony", argv)
            return 1
        }
    }

    def out := [].diverge()
    object printer:
        to print(s):
            out.push(s)

    return when (def codeBytes := inf.getContents()) ->
        def via (UTF8.decode) code := codeBytes
        def expr := m__quasiParser.fromStr(code).expand()
        printer.print(`
primitive $module

  fun eval(): MTObject ? =>
    `)
        def s := mkScope(mkCounter())
        s.asPony(expr)._printOn(printer)
        outf <- setContents(UTF8.encode("".join(out), null))
        0
