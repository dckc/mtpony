#lang racket

(provide root% mt-null mt-true mt-false)
(provide _makeList _makeInt _loop _validateFor _makeOrderedSpace)
(provide List% Int% Str%)

(define root%
  (class object%
    (super-new)
    ) )

(define Null%
  (class root%
    (super-new)
    ) )
(define mt-null (new Null%))

(define Bool%
  (class root%
    (init value)
    (super-new)
    ) )
; ISSUE: (object-name)
; ... (make-struct-tye-property

(define mt-true (new Bool% [value #t]))
(define mt-false (new Bool% [value #f]))


(define Int%
  (class root%
    (init value)
    (define i value)
    (super-new)

    (define (wrap v) (new Int% [value v]))
    (define (unwrap o) (send o int-value))
    ;; ISSUE: this shouldn't be a public method. Make and unwrap function.
    (define/public (int-value) i)

    (define/public (add other)
      (new Int% [value (+ i (unwrap other))]))

    (define/public (belowZero)
      (if (< i 0) mt-true mt-false))

    (define/public (op__cmp other)
      (let ([j (unwrap other)])
        (cond
          [(< i j) (wrap -1)]
          [(> i j) (wrap 1)]
          [else (wrap 0)])))
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
