#lang racket

(require redex)
;(require redex/reduction-semantics)

(require "kernel.rkt")

(define-extended-language Monte-closure Monte
  (Closure ::= ((Noun Value) ...))
)

(define-judgement-form monte-eval
  (eval I I I O)
  #:contract (eval Expr Closure : Value)
 
  [(eval (literal any_val) Closure : (value any_val))]

  [(eval Expr_test Closure : (primitive #t))
   (eval Expr_cons Closure : V)
   -----------------------------------------
   (eval (if Expr_test Expr_cons Expr) V)]

  [(eval Expr_test Closure : (primitive #f))
   (eval Expr_alt Closure : V)
   -----------------------------------------
   (eval (if Expr_test Expr Expr_alt) V)]

   (--> (send variable_rx "lessThan" (literal integer_x) (literal integer_y)) (value #t)
        (side-condition (and (< (term integer_x) (term integer_y))
                             ;; _foo is special syntax in redex. TODO: make a metafunction?
                             (equal? "_comparer" (symbol->string (term variable_rx)))))
        compare-int-lt)
   ))

(define -->v
    (context-closure monte-step Monte-call-by-value E))

; (traces -->n (term (literal "abc")))

(define-term block-expr-54
  (if (send _comparer "lessThan" (literal 2) (literal 3))
      (literal "expected")
      (literal "unexpected")))

; (traces -->n (term block-expr-54))

(test-->> -->n (term block-expr-54) (term (value "expected")))
