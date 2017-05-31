#lang racket

(require "kernel.rkt")
(require "doctest-suite.rkt")
(require "monte-runtime.rkt")
(require "safeScope.rkt")

(define scope/c (listof pair?))
(define monte-value/c (lambda (x) (is-a? x root%)))

(define/contract evaluator%
  (class/c
   (init [scopes (listof scope/c)])
   [run (->m monte-expr/c monte-value/c)]
   [match-bind (->m monte-patt/c monte-value/c void)]
  )
  (class object%
    (init scopes)
    (define _scopes scopes)
    (define _locals '()) ;; @@define/contract scope/c

    (super-new)
    (define/public (run expr)

      (define (recur e) (send this run e))
      (define (scope-lookup name)
        (let ([name-val (or (assoc name _locals) (assoc name (first _scopes)))])
          (if name-val (second name-val) (error 'not-bound))))

      (pretty-print expr) ;; @@

      (match expr
        [`(literal ,(and i (? integer? _))) (new Int% [value i])]
        [`(literal ,(and s (? string? _))) (new Str% [value s])]

        [(? symbol? noun) (scope-lookup noun)]

        [`(def ,lhs ,rhs)
         (let ([val (recur rhs)])
           (send this match-bind lhs val)
           val)]

        [`(send ,obj-expr ,verb ,args ...) ;; @@ named args
         (let ([obj (recur obj-expr)]
               [method-name (string->symbol verb)]
               [params (map recur args)])
           (apply dynamic-send obj method-name params))]

        [`(if ,test ,consequent ,alt)
         (let ([test-val (recur test)])
           (cond
             [(eq? test-val mt-true) (recur consequent)]
             [(eq? test-val mt-false) (recur alt)]
             [else (raise-argument-error 'if-test "Bool" test-val)]))]

        [`(sequence ,@exprs)
         (let ([result mt-null])
           (for ([expr exprs])
             (set! result (recur expr)))
           result)]
        [else (error 'not-implemented)]
        )
      )
    (define/public (match-bind patt val)  ;; TODO: guard
      (match patt
        (`(final ,name)
         (set! _locals `((,name ,val) . ,_locals)) ;; TODO: slots, bindings
         )
        )
      )
    )
  )

(for ([test-case monte-suite])
  (printf "\n\n~A-~A: ~A\n" (doctest-section test-case) (doctest-lineno test-case) (doctest-source test-case))
  (let ([context (new evaluator% [scopes (list safeScope)])]
        [expr (doctest-AST test-case)])
    (pretty-print (send context run expr))))
