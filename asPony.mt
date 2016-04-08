import "lib/codec/utf8" =~ [=> UTF8 :DeepFrozen]
exports (main)

def asPony(expr) as DeepFrozen:
    traceln("expr node:", expr.getNodeName())

    def list(exprs):
        if (exprs.size() == 0):
            return object emptyList:
                to _printOn(out):
                    out.print("Monte.emptyArgs()")
        def last := asPony(exprs.last())
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
        def [n, v] := [asPony(last), asPony(namedExprs[last])]
        return object cons:
            to _printOn(out):
                out.print(`Monte.namedArgsWith(`)
                init._printOn(out)
                out.print(", ")
                n._printOn(out)
                out.print(", ")
                v._printOn(out)
                out.print(")")
        
    return switch (expr.getNodeName()):
        match =="MethodCallExpr":
            def rx := asPony(expr.getReceiver())
            def verb := M.toQuote(expr.getVerb())
            def namedArgs := map(expr.getNamedArgs())
            def args := `$verb, ${list(expr.getArgs())}, $namedArgs`
            object call:
                to canFail():
                    return true
                to _printOn(out):
                    if (rx.canFail()):
                        out.print(`
    match ($rx)
    | let obj: MTObject => obj.call($args)
    | let err: MTErr => err
    else
      WrongType // can't happen. assert?
    end
`)
                    else:
                        out.print(`
    ($rx).call($args)
`)

        match =="LiteralExpr":
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
                    `Monte.makeStr(${M.toQuote(s)})`  # TODO: quoting
                } else {
                    throw(v) })
            object literal:
                to canFail():
                    return false
                to _printOn(out):
                    out.print(mkLit)


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

  fun eval(): (MTObject | MTErr) =>
    `)
        asPony(expr)._printOn(printer)
        outf <- setContents(UTF8.encode("".join(out), null))
        0
