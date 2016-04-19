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


def mkMethod(expr, scope, asScheme) as DeepFrozen:
    # TODO: def doc := expr.getDocstring()
    def verb := expr.getVerb()
    traceln("mkMethod:", verb)
    def mScope := scope.subScope()
    def patts := expr.getPatterns()
    def args := [for ix => patt in (patts) `_arg$ix` => patt]
    def bargs := [for arg => patt in (args)
                  mScope.unify(patt, arg, null)]
    # TODO: def namedPatts := expr.getNamedPatterns()
    # TODO: def guard := expr.getResultGuard()
    def body := asScheme(expr.getBody(), mScope)
    return object defmethod:
        to _printOn(out):
            out.print(`  (define/public ($verb ${" ".join(args.getKeys())})$\n`)
            out.print(mScope.slotDecl())
            for b in bargs:
                b._printOn(out)
            body._printOn(out)
            out.print(")\n")

def mkObject(expr, scope, asScheme) as DeepFrozen:
    # TODO: def doc := expr.getDocstring()
    def name := expr.getName()
    def asExpr := expr.getAsExpr()
    def auditors := expr.getAuditors()
    def script := expr.getScript()
    def methods := [for m in (script.getMethods())
                    mkMethod(m, scope, asScheme)]
    if (script.getMatchers().size() > 0):
        throw("TODO: matchers")
    return object odef:
        to _printOn(out):
            traceln("odef:" name)
            if (asExpr != null):
                out.print(`; TODO: asExpr $asExpr$\n`)
            if (auditors.size() > 0):
                out.print(`;TODO: auditors $auditors$\n`)
            out.print(`(define $name$\n  (new (class root% (super-new)$\n`)
            for m in (methods):
                m._printOn(out)
            out.print(")))\n")

def asScheme(expr, scope) as DeepFrozen:
    traceln("expr node:", expr.getNodeName())

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

        match =="ObjectExpr":
            mkObject(expr, scope, asScheme)

        match =="SeqExpr":
            def pexprs := [for e in (expr.getExprs()) asScheme(e, scope)]
            object seq:
                to _printOn(out):
                    out.print("\n(begin ")
                    for px in (pexprs):
                        px._printOn(out)
                        out.print("\n")
                    out.print(")")

        match =="EscapeExpr":
            def eScope := scope.subScope()
            def ej := eScope.temp("(make-ejector)")
            def u := eScope.unify(expr.getEjectorPattern(), ej, null)
            def body := asScheme(expr.getBody(), eScope)
            # TODO: def catchPatt := expr.getCatchPattern()
            # TODO: def catchExpr := expr.getCatchBody()
            return object esc:
                to _printOn(out):
                    out.print("\n(begin\n")
                    out.print(eScope.slotDecl())
                    u._printOn(out)
                    out.print("(with-handlers []\n")  # TODO
                    body._printOn(out)
                    out.print(") )\n")

        match =="MethodCallExpr":
            mkCall(expr, scope, asScheme)

        match =="NounExpr":
            def name := expr.getName()
            return scope[expr]

        match =="LiteralExpr":
            mkLiteral(expr)


def mkScope(outer, seq) as DeepFrozen:
    def slotVar := `slots${seq.next()}`
    def bindings := [].asMap().diverge()

    return object scope:
        # TODO: named arg for exit_
        to unify(patt, sexpr, exit_):
            traceln("unify:", patt.getNodeName())
            if (exit_ != null):
                throw("TODO: exit")

            return switch (patt.getNodeName()):
                match n ? (["FinalPattern", "VarPattern"].contains(n)):
                    def name := patt.getNoun().getName()
                    bindings[name] := "(void)"
                    # TODO: guard
                    traceln("FinalPattern; bindings now:", bindings)
                    return object defExpr:
                        to _printOn(out):
                            out.print(`(let ((_fixme `)
                            sexpr._printOn(out)
                            out.print(`)) (set! $name _fixme) _fixme)$\n`)

                match =="ListPattern":
                    def l := scope.temp(sexpr)
                    def items := [for ix => p in (patt.getPatterns())
                                  scope.unify(p, `(send $l get $ix)`, exit_)]
                    object listpat:
                        to _printOn(out):
                            for i in (items):
                                i._printOn(out)

                match =="ViaPattern":
                    def f := asScheme(patt.getExpr(), scope)
                    object funcall:
                        to _printOn(out):
                            out.print(`(send ($f) run $sexpr $exit_)`)
                    scope.unify(patt.getPattern(), funcall, exit_)

                match _:
                    throw(`TODO: pattern: $patt`)

        to temp(sexpr):
            def t := `_tmp${seq.next()}`
            bindings[t] := sexpr
            return t

        to get(noun):
            def name := noun.getName()
            def slotIx := bindings.getKeys().indexOf(name)
            traceln(`NounExpr: ${bindings.getKeys()}.indexOf($name) == $slotIx`)
            if (slotIx < 0):
                return outer[noun]
            return object noun:
                to _printOn(out):
                    out.print(name)
                to lvalue():
                    throw("@@")

        to slotDecl():
            return "\n".join([for name => v in (bindings)
                              `(define $name $v)`]) + "\n\n"

        to subScope():
            return mkScope(scope, seq)


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
    while (body.trim() =~ `import @matchbind]@rest`):
        def mbExpr := m__quasiParser.fromStr(matchbind + "]")
        def [_, =="run", [mb2, ==false], _] := mbExpr._uncall()
        imported.push(mb2)
        body := rest.trim()
    if (body =~ `exports@spc(@names)$\n@rest`):
       exported := [for name in (names.split(",")) name.trim()]
       body := rest.trim()
    return [imported.snapshot(), exported, body + "\n"]


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
