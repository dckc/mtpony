#lang racket

;; (require redex) ;; gui stuff
(require redex/reduction-semantics)

(provide Monte
         (contract-out
          [monte-expr/c (any/c . -> . boolean?)]
          [monte-method/c (any/c . -> . boolean?)]))

(define (monte-expr/c x)
  (redex-match? Monte Expr x))
(define (monte-method/c x)
  (redex-match? Monte Method x))

(define-language Monte
  (Expr ::= Literal Noun MethodCall If Object Sequence Def Hide Catch Finally Escape)
  (Literal ::= Int Str Double Char)
  (Int ::= number)
  (Char ::= (side-condition (name ch any) (term (char? ch))))
  (Str ::= string)
  (Double ::= real) ;;@@(side-condition (name x real) (flonum? x)))
  (Noun ::= variable)
  (MethodCall ::= (send Expr Verb (Expr ...) ((Expr . => . Expr) ...)))
  (If ::= (if Expr Expr Expr))
  (Sequence ::= (sequence (Expr ...)))
  (Hide ::= (hide Expr))
  (Catch ::= (try Expr #:catch Patt Expr))
  (Finally ::= (try Expr #:finally Expr))
  (Escape ::= (escape Patt Expr #:catch Patt Expr)
          (escape Patt Expr))
  (Def ::= (def Patt #:exit Expr Expr)
    (def Patt Expr))
  (Object ::= (object Final ;; or Ignore
                      (Method ...)) ;; (Matcher ...)
              )

  (Verb ::= string)
  (Method ::=
          (to Verb (Patt ...) Expr)
          (to Verb (Patt ...) #:guard Expr Expr)
          )
  
  (Patt ::= Ignore Final Via)
  (Ignore ::= (ignore #:guard Expr)
             (ignore))
  (Final ::= (final (variable-except _) #:guard Expr)
             (final (variable-except _)))
  (Via ::= (via Expr Patt))
  (Value ::= Int) ;;  Char Str Double Bool UserObj ...

  (Env ::= ((Noun Value) ...))
)
