#lang racket

(require "compiling-with-exceptions.rkt")

(require "kernel.rkt")
(require "doctest-suite.rkt")
(require "monte-runtime.rkt")
(require "safeScope.rkt")

(define scope/c (listof pair?))
(define/contract empty-scope scope/c '())
(define monte-value/c (lambda (x) (is-a? x receiver<%>)))

(define/contract evaluator%
  (class/c
   (init [scopes (listof scope/c)])
   [run (->m monte-expr/c monte-value/c)]
   [match-bind (->m monte-patt/c monte-value/c void)]
   )
  (class object%
    (init scopes)
    (define _scopes scopes)
    ;;@@(define/contract _locals scope/c empty-scope)
    (define _locals empty-scope)

    (super-new)

    (define (scope-lookup name)
      (let ([name-val (or (assoc name _locals) (assoc name (first _scopes)))])
        (if name-val (second name-val) (error 'not-bound))))

    (define/public (run expr)

      (define (recur e) (send this run e))
      (define/return (in-fresh-scope thunk)
        (set! _scopes (cons _locals _scopes))
        (set! _locals empty-scope)
        (try-finally
         (return (thunk))
         (begin
           (set! _locals (first _scopes))
           (set! _scopes (rest _scopes)))))

      (pretty-print expr) ;; @@

      (match expr

        ;; sequencing: Lit, Seq
        [`(literal ,(and i (? integer? _))) (new Int% [value i])]
        [`(literal ,(and s (? string? _))) (new Str% [value s])]

        [`(sequence ,@exprs)
         (let ([result mt-null])
           (for ([expr exprs])
             (set! result (recur expr)))
           result)]

        ;; read-only store (Noun, Binding)
        ;; TODO: binding
        [(? symbol? noun) (scope-lookup noun)]

        [`(def ,lhs ,rhs)
         (let ([val (recur rhs)])
           (send this match-bind lhs val)
           val)]

        ;; read-write store (unguarded patts, Def)...
        ;; TODO
        
        ;; scope introduction (Hide, If)
        ;; TODO: in-fresh-scope
        [`(hide ,expr) (in-fresh-scope (lambda () (recur expr)))]
        
        [`(if ,test ,consequent ,alt)
         (let* ([test-val (recur test)]
                [branch (cond
                          [(eq? test-val mt-true) consequent]
                          [(eq? test-val mt-false) alt]
                          [else (raise-argument-error 'if-test "Bool" test-val)])])
           (in-fresh-scope (lambda () (recur branch))))]

        ;; call contexts and closure (Obj, Call, guards)
        ;; TODO: Obj, guards
        [`(send ,obj-expr ,verb ,args ...) ;; @@TODO: named args
         (let ([obj (recur obj-expr)]
               [method-name (string->symbol verb)]
               [params (map recur args)])
           (apply dynamic-send obj method-name params))]

        ;; continuations (Try, Finally, Escape).
        ;; TODO


        [else (error "@@not-implemented: ~A" expr)]
        )
      )
    (define/public (match-bind patt val)  ;; TODO: guard
      (match patt
        [`(final ,name)
         (set! _locals `((,name ,val) . ,_locals)) ;; TODO: slots, bindings
         ]
        )
      )
    )
  )

(for ([test-case monte-suite])
  (printf "\n\n~A-~A: ~A\n" (doctest-section test-case) (doctest-lineno test-case) (doctest-source test-case))
  (let ([context (new evaluator% [scopes (list safeScope)])]
        [expr (doctest-AST test-case)])
    (pretty-print (send context run expr))))
