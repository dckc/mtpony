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
                    out.print("Monte.emptyList()")
        def [first] + rest := exprs
        def f := asPony(first)
        def r := list(rest)
        return object cons:
            to _printOn(out):
                out.print(`Monte._listWith($f, $r)`)
        
    def map(namedExprs):
        if (namedExprs.size() == 0):
            return object emptyList:
                to _printOn(out):
                    out.print("Monte.emptyMap()")
        def [first] + _ := namedExprs.keys()
        def [n, v] := [asPony(first), asPony(namedExprs[n])]
        def r := map(namedExprs.slice(1))
        return object cons:
            to _printOn(out):
                out.print(`Monte._mapWith($n, $v, $r)`)
        
    return switch (expr.getNodeName()):
        match =="MethodCallExpr":
            def rx := asPony(expr.getReceiver())
            def verb := M.toQuote(expr.getVerb())
            def args := list(expr.getArgs())
            def namedArgs := map(expr.getNamedArgs())
            object call:
                to _printOn(out):
                    out.print(`call($rx, $verb, ${args}, $namedArgs)`)

        match =="LiteralExpr":
            def v := expr.getValue()
            def mkLit := (
                if (v =~ n :Void) {
                    "_theNull()"
                } else if (v =~ b :Bool) {
                    `_makeBool($b)`
                } else if (v =~ i :Int) {
                    `_makeInt("$i")`
                } else if (v =~ ch :Char) {
                    `_makeChar(${ch.asInteger()})`
                } else if (v =~ d :Double) {
                    `_makeInt($d)`
                } else if (v =~ s :Str) {
                    `_makeStr(${M.toQuote(s)})`  # TODO: quoting
                } else {
                    throw(v) })
            object literal:
                to _printOn(out):
                    out.print(mkLit)

            
def main(argv,
         => makeStdIn,
         => makeStdOut) as DeepFrozen:
    def stdin := makeStdIn() <- flowTo(makePumpTube(makeUTF8DecodePump()))
    def stdout := makePumpTube(makeUTF8EncodePump())
    stdout <- flowTo(makeStdOut())
    def expr := m__quasiParser.fromStr("'a' + 1").expand()
    traceln(expr)

    object printer:
        to print(s):
            stdout.receive(s)

    asPony(expr)._printOn(printer)
