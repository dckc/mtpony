import "lib/codec/utf8" =~ [=> UTF8 :DeepFrozen]
exports (main)


def mkLiteral(expr) as DeepFrozen:
    def mkLit := switch (expr.getValue()) {
        # these are nouns, not literals:
        # match n :Void   { "null" }
        # match b :Bool   { ["true", "false"].pick(b) }
        match i :Int    { `(mt:Int $i)` }
        match ch :Char  { `(mt:Char ${ch.asInteger()})` }
        match d :Double { `(mt:Double $d)` }
        match s :Str    { `(mt:Str ${M.toQuote(s)})` }
    }

    return object literal:
        to _printOn(out):
            out.print(mkLit)


def mkCall(expr, scope, asScheme) as DeepFrozen:
    def xlate := fn e { asScheme(e, scope) }
    def rx := xlate(expr.getReceiver())
    def verb := expr.getVerb()  # TODO: scheme quote symbol
    def args := [for e in (expr.getArgs()) xlate(e)]
    def nargs := [for k => v in (expr.getNamedArgs()) k => xlate(v)]
    return object call:
        to _printOn(out):
            out.print("(send ")
            rx._printOn(out)
            out.print(` $verb `)
            for sexp in (args):
                sexp._printOn(out)
                out.print(" ")
            for k => v in (nargs):
                out.print(`#:$k `)
                v._printOn(out)
                out.print(" ")
            out.print(")")

def asScheme(expr, scope) as DeepFrozen:
    traceln("expr node:", expr.getNodeName(), expr)

    return switch (expr.getNodeName()):
        match =="DefExpr":
            def sexpr := asScheme(expr.getExpr(), scope)
            scope.unify(expr.getPattern(), sexpr, expr.getExit())
        match =="AssignExpr":
            trace(expr._uncall())
            def [_, verb, [lhs, rhs, span], _] := expr._uncall()
            def value := asScheme(rhs, scope)
            return object assign:
                to _printOn(out):
                    out.print(`(set! $lhs `)
                    value._printOn(out)
                    out.print(")\n")

        match =="SeqExpr":
            def pexprs := [for e in (expr.getExprs()) asScheme(e, scope)]
            object seq:
                to _printOn(out):
                    out.print("(begin ")
                    for px in (pexprs):
                        px._printOn(out)
                        out.print("\n")
                    out.print(")")

        match =="MethodCallExpr":
            mkCall(expr, scope, asScheme)

        match =="NounExpr":
            def name := expr.getName()
            return scope[expr]

        match =="LiteralExpr":
            mkLiteral(expr)


def mkScope(outer, seq) as DeepFrozen:
    def slotVar := `slots${seq.next()}`
    def bindings := [].diverge()

    return object scope:
        to unify(patt, sexpr, exit_):
            if (exit_ != null):
                throw("TODO: exit")

            switch (patt.getNodeName()):
                match n ? (["FinalPattern", "VarPattern"].contains(n)):
                    def name := patt.getNoun().getName()
                    bindings.push(name)
                    # TODO: guard
                    traceln("FinalPattern; bindings now:", bindings)
                    return object defExpr:
                        to _printOn(out):
                            out.print(`(set! $name `)
                            sexpr._printOn(out)
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
                    out.print(name)
                to lvalue():
                    throw("@@")

        to slotDecl():
            return "\n".join([for name in (bindings)
                              `(define $name (void))`])

object topScope as DeepFrozen:
    to get(noun):
        def name := noun.getName()
        safeScope[`&&$name`]
        traceln(`safeScope: $name`)
        return object noun:
            to _printOn(out):
                # TODO: identifier quoting
                out.print(name)


def mkCounter() as DeepFrozen:
    var x := 0
    return object counter:
        to next():
            x += 1
            return x


def parseModule(code) as DeepFrozen:
    var body := code.trim()
    def imported := [].diverge()
    var exported := []
    # TODO: use lexer to get proper handling of spaces, ;, ::
    while (body.trim() =~ `import @matchbind$\n@rest`):
        def mbExpr := m__quasiParser.fromStr(matchbind)
        def [_, =="run", [mb2, ==false], _] := mbExpr._uncall()
        imported.push(mb2)
        body := rest.trim()
    if (body =~ `exports(@names)$\n@rest`):
       exported := [for name in (names.split(",")) name.trim()]
       body := rest.trim()
    return [imported.snapshot(), exported, body]


def compile(codeBytes, printer, modName) as DeepFrozen:
    return when (codeBytes) ->
        def via (UTF8.decode) code := codeBytes

        def [imported, exported, body] := parseModule(code)
        traceln("module:", imported, exported)

        def s := mkScope(topScope, mkCounter())
        def expr := m__quasiParser.fromStr(body).expand()
        def sexpr := asScheme(expr, s)

        printer.print(`#lang racket

(require "safeScope.rkt")
(require (prefix-in mt: "monte-runtime.rkt"))

`)
        for ident in (exported):
            printer.print(`(provide $ident)$\n`)
        printer.print("\n")
        printer.print(s.slotDecl())
        printer.print("\n")
        sexpr._printOn(printer)


def main(argv, => makeFileResource) as DeepFrozen:
    def [inf, outf, modName] := {
        escape usage {
            def [infn, outfn] exit usage := argv
            def `@modName.rkt` exit usage := outfn
            [makeFileResource(infn), makeFileResource(outfn), modName]
        } catch _ {
            traceln("Usage: any.mt modName.rkt", argv)
            return 1
        }
    }

    def out := [].diverge()
    object printer:
        to print(s):
            out.push(s)

    when (compile(inf.getContents(), printer, modName)) ->
        outf <- setContents(UTF8.encode("".join(out), null))
        0
