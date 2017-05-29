#lang racket

(require "kernel.rkt")
(require "doctest-suite.rkt")
(require "monte-runtime.rkt")
(require "safeScope.rkt")

(define/contract (mt-eval expr scopes)
  (monte-expr/c (listof pair?) . -> . any/c)

  (define (recur e) (mt-eval e scopes))

  (pretty-print expr)
  
  (match expr
    [(list literal (and i (? integer? _))) (new Int% [value i])]
    [(list literal (and s (? string? _))) (new Str% [value s])]

    [(? symbol? noun) (second (assoc noun (first scopes)))]
    
    [`(send ,obj-expr ,verb ,args ...) ;; @@ named args
     (let ([obj (recur obj-expr)]
           [method-name (string->symbol verb)]
           [params (map recur args)])
       (apply dynamic-send obj method-name params))]
    
    [(list if test consequent alt)
     (let ([test-val (recur test)])
       (cond
         [(eq? test-val mt-true) (recur consequent)]
         [(eq? test-val mt-false) (recur alt)]
         [else (raise-argument-error 'if-test "Bool" test-val)]))]
    )
  )

(for ([test-case monte-suite])
     (printf "\n\n~A-~A: ~A\n" (doctest-section test-case) (doctest-lineno test-case) (doctest-source test-case))
     (pretty-print (mt-eval (doctest-AST test-case) (list safeScope))))

                 
