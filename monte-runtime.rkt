#lang racket

(provide root% true false null)
(provide _makeList _makeInt _loop _validateFor _makeOrderedSpace)
(provide traceln)

(define root%
  (class object%
    (super-new)
    ) )

(define Null%
  (class root%
    (super-new)
    ) )
(define null (new Null%))

(define Bool%
  (class root%
    (init value)
    (super-new)
    ) )
(define true (new Bool% [value #t]))
(define false (new Bool% [value #t]))

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

(define-syntax def/fn
  (syntax-rules ()
    [(def/fn (name arg ...) body)
     (define name
       (new
        (class root%
          (super-new)
          (define/public (run arg ...)
            body))))]
    
    [(def/fn (name . args) body)
     (define name
       (new
        (class root%
          (super-new)
          (define/public (run . args)
            body))))]
    ))

(def/fn (_makeInt numeral)
  (let* ((s (send numeral string-value))
         (i (string->number s)))
    (new Int% [value i])))

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
    (define s value)
    (super-new)
    (define/public (string-value)
      s)
    ) )

(define (Str s)
  (new Str% [value s]))

(define _last last)

(define List%
  (class root%
    (init members)  ; TODO: contract on members
    (define xs members)
    (super-new)
    (define/public (items) xs)
    (define/public (last) (_last xs))
    ) )

(def/fn (_makeList . items)
  (new List% [members items]))

(def/fn (traceln . items)
  (printf "TRACE: ~a\n" items))

(def/fn (_loop iterable body)
  (let ((iterator (send iterable _makeIterator))
        (ej 'todo-ejector))
    (define (loop)
      (let* ((argList (send iterator next ej))
             (args (send argList items)))
        (send body run . args)
        (loop)))
    (loop)))

(def/fn (_validateFor flag)
  (if (not (send flag boolean-value))
      (raise "Failed to validate loop!")
      null))

; note: defined in mast/prelude/region.mt
(define _makeOrderedSpace
  (new
   (class root%
     (super-new)
     (define/public (op__thru lo hi)
       (new
        (class root%
          (super-new)
          (define/public (_makeIterator)
            (new
             (class root%
               (define pos lo)
               (super-new)
               (define/public (next ej)
                 (if (> pos hi) (ej)
                     (let ((out pos))
                       (set! pos (send pos next))
                       (_makeList 'todo pos))))
               )))))))))
