#lang racket

(require redex)
;(require redex/reduction-semantics)

(require "kernel.rkt")

(define-extended-language Monte-values Monte
  (Value ::= (datum Datum))
  (Datum ::= integer string #t #f)  ;; Char Double null
  )

(define monte-step
  (reduction-relation
   Monte-values #:domain Expr
   (--> (literal any_val) (datum any_val) literal-eval)
   (--> (if (datum #t) Expr_1 Expr_2) Expr_1 if-t)
   (--> (if (datum #f) Expr_1 Expr_2) Expr_2 if-f)
   ;; else error

   (--> (send variable_rx "lessThan" (integer_x integer_y) ()) #t
        (side-condition (and (< (term integer_x) (term integer_y))
                             ;; _foo is special syntax in redex. TODO: make a metafunction?
                             (equal? "_comparer" (symbol->string (term variable_rx)))))
        compare-int-lt)
   ))

(define-extended-language Monte-call-by-value Monte-values
  (E ::= hole
     (send E Verb (Expr ...) ((string . => . Expr) ...))
     (send Value Verb (Value ... E Expr ...) ((string . => . Expr) ...))
     (if E Expr Expr))
)

(define -->n
    (context-closure monte-step Monte-call-by-value E))

(traces -->n (term (literal "abc")))

(define-term block-expr-54
  (if (send _comparer "lessThan" ((literal 2) (literal 3)))
      "expected"
      "unexpected"))

(traces -->n (term block-expr-54))
