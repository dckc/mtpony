#lang racket

(provide root% Bool% Null% List%)
(provide (contract-out [Int (-> integer? any)]
                       [Char (-> integer? any)]  ; TODO: 32bit int
                       [Str (-> string? any)]
                       [Double (-> inexact? any)]))

(define root%
  (class object%
    (super-new)
    ) )

(define Null%
  (class root%
     (super-new)
    ) )

(define Bool%
  (class root%
    (init value)
    (super-new)
    ) )

(define Int%
  (class root%
    (init value)
    (define i value)
    (super-new)
    (define/public (add other)
      (new Int% [value (+ i (send other int-value))]) )
    (define/public (int-value)
      i)
    ) )

(define (Int i)
  (new Int% [value i]))

(define Char%
  (class root%
    (init value)
    (define codepoint value)
    (super-new)
    (define/public (int-value)
      codepoint)
    ) )

(define (Char i)
  (new Char% [value i]))

(define Double%
  (class root%
    (init value)
    (define d value)
    (super-new)
    (define/public (add other)
      (new Double% [value (+ d (send other double-value))]) )
    (define/public (double-value)
      d)
    ) )

(define (Double d)
  (new Double% [value d]))

(define Str%
  (class root%
    (init value)
    (super-new)
    ) )

(define (Str s)
  (new Str% [value s]))

(define List%
  (class root%
    (init members)  ; TODO: contract on members
    (super-new) ) )
