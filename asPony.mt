import "lib/tubes" =~ [
    => makeUTF8DecodePump :DeepFrozen,
    => makeUTF8EncodePump :DeepFrozen,
    => makePumpTube :DeepFrozen,
]
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
            def args := list(expr.getArgs())
            def namedArgs := map(expr.getNamedArgs())
            object call:
                to _printOn(out):
                    out.print(`($rx).call($verb, ${args}, $namedArgs)`)

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
                to _printOn(out):
                    out.print(mkLit)

            
def main(argv,
         => makeStdIn,
         => makeStdOut) as DeepFrozen:
    def [name] := ["Module1"] # argv
    def stdin := makeStdIn() <- flowTo(makePumpTube(makeUTF8DecodePump()))
    def stdout := makePumpTube(makeUTF8EncodePump())
    stdout <- flowTo(makeStdOut())
    def expr := m__quasiParser.fromStr("'a' + 1").expand()
    traceln(expr)

    object printer:
        to print(s):
            stdout.receive(s)

    printer.print(`
primitive $name

  fun eval() =>
`)
    asPony(expr)._printOn(printer)
