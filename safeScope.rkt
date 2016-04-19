#lang racket

(require "monte-runtime.rkt")

(provide _makeList null true false traceln)

(define _makeList
  (new (class root%
         (super-new)
         (define/public (run . args)
           (new List% [members args]) ) )) )

(define null (new Null%))
(define true (new Bool% [value #t]))
(define false (new Bool% [value #f]))

(define traceln
  (new (class root%
         (super-new)
         (define/public (run . args)
           (printf "TRACE ~v\n" args) ) )) )
