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


def mkCall(expr, scope, asPony) as DeepFrozen:
    def list(exprs):
        if (exprs.size() == 0):
            return object emptyList:
                to _printOn(out):
                    out.print("Monte.emptyArgs()")
        def last := asPony(exprs.last(), scope)
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
        def [n, v] := [asPony(last, scope), asPony(namedExprs[last], scope)]

        return object cons:
            to _printOn(out):
                out.print(`Monte.namedArgsWith(`)
                init._printOn(out)
                out.print(", ")
                n._printOn(out)
                out.print(", ")
                v._printOn(out)
                out.print(")")

    def rx := asPony(expr.getReceiver(), scope)
    def verb := M.toQuote(expr.getVerb())
    def namedArgs := map(expr.getNamedArgs())
    def args := `$verb, ${list(expr.getArgs())}, $namedArgs`
    return object call:
        to _printOn(out):
            out.print(`($rx).call($args)`)


def asPony(expr, scope) as DeepFrozen:
    traceln("expr node:", expr.getNodeName(), expr)

    return switch (expr.getNodeName()):
        match =="DefExpr":
            scope.unify(expr.getPattern(), expr.getExpr(), expr.getExit())
        match =="AssignExpr":
            trace(expr._uncall())
            def [_, verb, [lhs, rhs, span], _] := expr._uncall()
            def value := asPony(rhs, scope)
            return object assign:
                to _printOn(out):
                    out.print(`${scope[lhs]} = (`)
                    value._printOn(out)
                    out.print(")\n")

        match =="SeqExpr":
            def pexprs := [for e in (expr.getExprs()) asPony(e, scope)]
            object seq:
                to _printOn(out):
                    out.print("(")
                    for px in (pexprs):
                        px._printOn(out)
                        out.print("\n")
                    out.print(")")

        match =="MethodCallExpr":
            mkCall(expr, scope, asPony)

        match =="NounExpr":
            def name := expr.getName()
            return scope[expr]

        match =="LiteralExpr":
            mkLiteral(expr)

def mkScope(outer, seq) as DeepFrozen:
    def slotVar := `slots${seq.next()}`
    def bindings := [].diverge()

    return object scope:
        to unify(patt, expr, exit_):
            if (exit_ != null):
                throw("TODO: exit")

            switch (patt.getNodeName()):
                match n ? (["FinalPattern", "VarPattern"].contains(n)):
                    bindings.push(patt.getNoun().getName())
                    # TODO: guard
                    traceln("FinalPattern; bindings now:", bindings)
                    def value := asPony(expr, scope)
                    return object defExpr:
                        to _printOn(out):
                            out.print(`$slotVar.push(`)
                            value._printOn(out)
                            out.print(")\n")
                match _:
                    throw("TODO: non-final patterns")

        to get(noun):
            def name := noun.getName()
            def slotIx := bindings.indexOf(name)
            if (slotIx < 0):
                traceln(`NounExpr: $bindings.indexOf($name) == $slotIx`)
                return outer[noun]
            return object noun:
                to _printOn(out):
                    out.print(`$slotVar($slotIx)`)
                to lvalue():
                    return `$slotVar($slotIx)`

        to slotDecl():
            return `let $slotVar: Array[MTObject] ref = Array[MTObject]()`


object topScope as DeepFrozen:
    to get(noun):
        def name := noun.getName()
        safeScope[`&&$name`]
        traceln(`safeScope: $name`)
        return object noun:
            to _printOn(out):
                out.print(`safeScope("$name")`)


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
use "collections"

class $module

  fun eval(safeScope: Map[String val, MTObject ref]): MTObject ? =>
    `)
        def s := mkScope(topScope, mkCounter())
        def pexpr := asPony(expr, s)
        printer.print("    " + s.slotDecl() + "\n")
        pexpr._printOn(printer)
        outf <- setContents(UTF8.encode("".join(out), null))
        0
