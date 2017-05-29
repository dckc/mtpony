#lang racket

(require "monte-runtime.rkt")

(provide traceln safeScope)

(define _makeList
  (new (class root%
         (super-new)
         (define/public (run . args)
           (new List% [members args]) ) )) )

(define _comparer
  (new (class root%
         (super-new)
         (define/public (lessThan left right)
           (send (send left op__cmp right) belowZero) ))))

(define traceln
  (new (class root%
         (super-new)
         (define/public (run . args)
           (printf "TRACE ~v\n" args) ) )) )

(define safeScope
  `((null ,mt-null)
    (true ,mt-true)
    (false ,mt-false)
    (_comparer ,_comparer)
    (_makeList ,_makeList)))
