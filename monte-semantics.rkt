#lang racket

(require redex)
;(require redex/reduction-semantics)

(require "kernel.rkt")

(define monte-step
  (reduction-relation
   Monte #:domain Expr
   (--> (literal any_val) (primitive any_val) literal-eval)
   (--> (if (primitive #t) Expr_1 Expr_2) Expr_1 if-t)
   (--> (if (primitive #f) Expr_1 Expr_2) Expr_2 if-f)
   ;; else error

   (--> (send variable_rx "lessThan" (literal integer_x) (literal integer_y)) (primitive #t)
        (side-condition (and (< (term integer_x) (term integer_y))
                             ;; _foo is special syntax in redex. TODO: make a metafunction?
                             (equal? "_comparer" (symbol->string (term variable_rx)))))
        compare-int-lt)
   ))

(define-extended-language Monte-call-by-value Monte
  (E ::= hole
     (send E Verb (Expr ...) ((string . => . Expr) ...))
     (send Value Verb (Value ... E Expr ...) ((string . => . Expr) ...))
     (if E Expr Expr))
)

(define -->n
    (context-closure monte-step Monte-call-by-value E))

(traces -->n (term (literal "abc")))

(define-term block-expr-54
  (if (send _comparer "lessThan" (literal 2) (literal 3))
      (literal "expected")
      (literal "unexpected")))

(traces -->n (term block-expr-54))
