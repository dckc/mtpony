#lang racket

(provide decode-mast check-magic decode-literal decode-expr)

(define (decode-mast port)
  (check-magic port)
  (let-values ([(exprs patts) (decode-expr port '() '())])
    (printf "@@exprs: ~a patts: ~a\n" exprs patts)
    (last exprs) ) )

(define magic #"Mont\340MAST\0")

(define (check-magic port)
  (let ((actual (read-bytes (bytes-length magic) port)))
    (if (equal? magic actual) #t
        (error `(bad-magic ,actual)) ) ) )

(define (tag b)
  (integer->char b))
(define (must-read-byte port)
  (let ((b (read-byte port)))
    (if (eof-object? b) (error 'early-eof) b) ) )

(define (decode-expr port exprs patts)
  (define (go-e e)
    (printf "@@ go-e: ~a\n" e)
    (decode-expr port (append exprs (list e)) patts))
  (let ((b (read-byte port)))
    (if (eof-object? b) (values exprs patts)
        (case (tag b)
          [(#\L) (go-e (decode-literal port))]
          ) ) ) )

(define (decode-literal port)
  (case (tag (must-read-byte port))
    [(#\C) `(lit ,(read-char port))]
    [(#\D) (error 'TODO-double)]
    [(#\I) `(lit ,(zz (decode-int port)))]
    [(#\N) no-expr]
    [(#\S) `(lit ,(decode-str port))]
     ) )

(define no-expr (cons '() '()))

(define (decode-int port)
  (define (go shift i)
    (let* ((b (must-read-byte port))
           (stop (= 0 (bitwise-and b 128)))
           (bi (bitwise-ior i (arithmetic-shift (bitwise-and b 127) shift))) )
      (if stop bi (go (+ shift 7) bi)) ) )
  (go 0 0) )

(define (zz i)
  (let ((mag (arithmetic-shift i -1)))
    (if (= 1 (bitwise-and i 1))
        (bitwise-xor mag -1)
        mag ) ) )

(define (decode-str port)
  (let* ((size (decode-int port))
         (bs (read-bytes size port)))
    (bytes->string/utf-8 bs) ) )
