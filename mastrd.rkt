#lang racket

(require racket/pretty)

(provide decode-mast check-magic decode-literal decode-expr)

(define (decode-mast port)
  (check-magic port)
  (let* ([exprs-patts (decode-expr port '() '())]
         [exprs (first exprs-patts)]
         [patts (second exprs-patts)])
    (pretty-print patts)
    (pretty-print exprs)
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
    (printf "@@(go-e ~v)\n" e)
    (decode-expr port (append exprs (list e)) patts) )
  (define (go-p p)
    (printf "@@(go-p ~v)\n" p)
    (decode-expr port exprs (append patts (list p))) )

  (define (next-expr)
    (list-ref exprs (decode-int port)) )
  (define (next-exprs)
    (for/list [(ix (in-range (decode-int port)))] (next-expr)) )
  (define (next-named-exprs)
    (for/list [(ix (in-range (decode-int port)))]
      `(named-arg ,(next-expr) ,(next-expr)) ) )
  (define (next-patt)
    (list-ref patts (decode-int port)) )
  (define (next-patts)
    (for/list [(ix (in-range (decode-int port)))] (next-patt)) )
  (define (next-named-patts)
    (for/list [(ix (in-range (decode-int port)))]
      `(,(next-expr) => ,(next-expr) := ,(next-expr)) ) )

  (define (decode-pattern)
    (let ((ptag (tag (must-read-byte port))))
      (case ptag
        [(#\A) (let ((expr (next-expr))
                     (patt (next-patt)))
                 `(via ,expr ,patt))]
        [(#\F) (let ((name (decode-str port))
                     (guard (next-expr)))
                 `(final ,name #:guard ,guard) )]
        [(#\I) (let ((guard (next-expr)))
                 `(_ #:guard ,guard))]
        [(#\L) `(list ,(next-patts))]
        [(#\V) (let ((name (decode-str port))
                     (guard (next-expr)))
                 `(var ,name #:guard ,guard) )]
        [else (error (format "pattern tag??? ~v" ptag))]
        ) ) )

  (let ((b (read-byte port)))
    (if (eof-object? b) (list exprs patts)
        (case (tag b)
          [(#\A) (let ((target (decode-str port)) ; symbol?
                       (expr (next-expr)))
                   (go-e `(assign ,target ,expr)))]
          [(#\C) (go-e (let ((target (next-expr))
                             (verb (decode-str port)) ; symbol?
                             (args (next-exprs))
                             (kwargs (next-named-exprs)))
                         ; splice args and use #:x for kwargs
                         `(send ,target ,verb ,args ,kwargs) )) ]
          [(#\D) (let ((patt (next-patt))
                       (exit (next-expr))
                       (expr (next-expr)))
                   (go-e `(def ,patt #:exit ,exit ,expr)))]
          [(#\L) (go-e (decode-literal port))]
          [(#\M) (let ((doc (decode-str port))
                       (verb (decode-str port))
                       (patts (next-patts))
                       (namedPatts (next-named-patts))
                       (guard (next-expr))
                       (block (next-expr)))
                   (go-e `(define/method ,verb ,patts ,namedPatts
                            #:guard ,guard
                            #:doc ,doc
                            ,block)) )]
          [(#\O) (let ((doc (decode-str port))
                       (patt (next-patt))
                       (asExpr (next-expr))
                       (implements (next-exprs))
                       (methods (next-exprs))
                       (matchers (next-exprs)))
                   (go-e `(define/object ,patt
                            #:as ,asExpr
                            #:implements ,implements
                            #:doc ,doc
                            ,methods
                            #:matchers ,matchers)))]
          [(#\N) (go-e `(noun ,(decode-str port)))]
          [(#\P) (go-p (decode-pattern))]
          [(#\S) (go-e `(begin ,(next-exprs)))]
          [else (error (format "expr tag??? ~v ~v" b (integer->char b)))]
          ) ) ) )

(define (decode-literal port)
  (case (tag (must-read-byte port))
    [(#\C) `(lit ,(read-char port))]
    [(#\D) (error 'TODO-double)]
    [(#\I) `(lit ,(zz (decode-int port)))]
    [(#\N) 'not-present]
    [(#\S) `(lit ,(decode-str port))]
    [else (error 'bad-literal-tag)]
     ) )

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