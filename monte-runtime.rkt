#lang racket

(provide receiver<%> mt-null mt-true mt-false)
(provide _makeList _makeInt _loop _validateFor _makeOrderedSpace)
(provide List% Int% Str%)

(define receiver<%>
  (interface ()
    ;; implies printable<%>?
    ;; TODO: recv
    ))


(define print-only<%>
  (interface ()
    custom-print
    ))
(define simple-print
  (mixin (print-only<%>) (printable<%>)
    (inherit custom-print)
    (super-new)
    (define/public (custom-write port)
      (custom-print port 0))
    (define/public (custom-display port)
      (custom-print port 0))))

;; TODO: trait miranda?

(define Null%
  (simple-print
   (class* object% (print-only<%> receiver<%>)
     (super-new)
     (define/public (custom-print port quasi-depth)
       (print "null" port))
     ) ))
(define mt-null (new Null%))

(define Bool%
  (simple-print
   (class* object% (print-only<%> receiver<%>)
     (init value)
     (define _value value) ;; boring
     (super-new)
     (define/public (custom-print port quasi-depth)
       (print _value port))
     ) ))
; ISSUE: (object-name)
; ... (make-struct-tye-property

(define mt-true (new Bool% [value #t]))
(define mt-false (new Bool% [value #f]))


(define Int%
  (simple-print
   (class* object% (print-only<%> receiver<%>)

     (init value)
     (define i value)
     (super-new)

     (define/public (custom-print port quasi-depth)
       (print i port))
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
     ) ) )

(define-syntax def/fn
  (syntax-rules ()
    [(def/fn (name arg ...) body)
     (define name
       (new
        (class* object% (receiver<%>)
          (super-new)
          (define/public (run arg ...)
            body))))]
    
    [(def/fn (name . args) body)
     (define name
       (new
        (class* object% (receiver<%>)
          (super-new)
          (define/public (run . args)
            body))))]
    ))

(def/fn (_makeInt numeral)
  (let* ((s (send numeral string-value))
         (i (string->number s)))
    (new Int% [value i])))

(define Char%
  (simple-print
   (class* object% (print-only<%> receiver<%>)
     (init value)
     (define codepoint value)  ;; @@ISSUE: codepoint or char?
     (super-new)
     (define/public (int-value)
       codepoint)
     (define/public (custom-print port quasi-depth)
       (print codepoint port))
     ) ))

(define (Char i)
  (new Char% [value i]))

(define Double%
  (simple-print
   (class* object% (print-only<%> receiver<%>)
     (init value)
     (define d value)
     (super-new)
     (define/public (add other)
       (new Double% [value (+ d (send other double-value))]) )
     (define/public (double-value)
       d)
     (define/public (custom-print port quasi-depth)
       (print d port))
     ) ))

(define (Double d)
  (new Double% [value d]))

(define Str%
  (simple-print
   (class* object% (print-only<%> receiver<%>)
     (init value)
     (define s value)
     (super-new)
     (define/public (string-value)
       s)
     (define/public (custom-print port quasi-depth)
       (print s port))
     ) ))

(define (Str s)
  (new Str% [value s]))

(define _last last)

(define List%
  (class* object% (receiver<%>)
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
   (class* object% (receiver<%>)
     (super-new)
     (define/public (op__thru lo hi)
       (new
        (class* object% (receiver<%>)
          (super-new)
          (define/public (_makeIterator)
            (new
             (class* object% (receiver<%>)
               (define pos lo)
               (super-new)
               (define/public (next ej)
                 (if (> pos hi) (ej)
                     (let ((out pos))
                       (set! pos (send pos next))
                       (_makeList 'todo pos))))
               )))))))))
