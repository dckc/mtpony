#lang racket

;; (require redex) ;; gui stuff
(require redex/reduction-semantics)

(provide Monte
         (contract-out
          [monte-expr/c (any/c . -> . boolean?)]
          [monte-patt/c (any/c . -> . boolean?)]
          [monte-method/c (any/c . -> . boolean?)]
          [monte-matcher/c (any/c . -> . boolean?)]))

(define (monte-expr/c x)
  (redex-match? Monte Expr x))
(define (monte-patt/c x)
  (redex-match? Monte Patt x))
(define (monte-method/c x)
  (redex-match? Monte Method x))
(define (monte-matcher/c x)
  (redex-match? Monte Matcher x))

(define-language Monte
  (Expr ::= Literal Noun MethodCall control-flow Def Assign Binding Object)
  (control-flow ::= Hide Sequence If Catch Finally Escape)
  (Literal ::= (literal Lit))
  (Lit ::=
       integer
       string
       (side-condition (name ch any) (term (char? ch)))
       (side-condition (name x any) (term (flonum? x))) )
  (Noun ::= variable)
  (MethodCall ::= (send Expr Verb Expr ... (Expr . => . Expr) ...))
  (If ::= (if Expr Expr Expr)
      (if Expr Expr))
  (Sequence ::= (sequence Expr ...))
  (Hide ::= (hide Expr))
  (Catch ::= (try Expr #:catch Patt Expr))
  (Finally ::= (try Expr #:finally Expr))
  (Escape ::= (escape Patt Expr #:catch Patt Expr)
          (escape Patt Expr))
  (Def ::= (def Patt #:exit Expr Expr)
    (def Patt Expr))
  (Assign ::= (set! variable Expr))
  (Binding ::= (binding variable))
  (Object ::=
          (object ObjectName #:doc string Method ... Matcher ...)
          (object ObjectName Method ... Matcher ...)
          )
  (ObjectName ::= FinalPatt
              IgnorePatt)
  (Verb ::= string)
  (Method ::=
          (method Verb (Patt ... (Expr . => . Patt) ...) #:guard Expr Expr)
          (method Verb (Patt ... (Expr . => . Patt) ...) Expr)
          )
  (Matcher ::= (match Patt Expr))

  (Patt ::= IgnorePatt FinalPatt ViaPatt VarPatt BindingPatt ListPatt)
  (IgnorePatt ::= (ignore #:guard Expr)
             (ignore))
  (FinalPatt ::= (final variable #:guard Expr)
             (final variable))
  (VarPatt ::= (var variable #:guard Expr)
       (var variable))
  (ViaPatt ::= (via Expr Patt))
  (BindingPatt ::= (&& variable))
  (ListPatt ::= (list Patt ...))
)

(test-equal
 (redex-match? Monte Patt
               '(&& broken_8))
 #t)
