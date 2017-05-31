#lang racket

(require racket/pretty)
(require math/flonum)
(require redex/reduction-semantics)

(require "kernel.rkt")

(provide (contract-out
          [decode-mast (port? . -> . monte-expr/c)]
          [magic bytes?]))

(define (decode-mast port)
  (check-magic port)
  (let* ([with-spans (version-has-spans (must-read-byte port))]
          [exprs-patts (decode-expr port with-spans '() '())]
         [exprs (first exprs-patts)]
         [patts (second exprs-patts)])
    ;(pretty-print patts)
    ;(pretty-print exprs)
    (last exprs) ) )

(define magic #"Mont\340MAST")

(define (version-has-spans version)
  (case version
    ((0) #f)
    ((1) #t)
    (else (error 'unkown-version))))

(define (check-magic port)
  (let ((actual (read-bytes (bytes-length magic) port)))
    (if (equal? magic actual) #t
        (error `(bad-magic ,actual)) ) ) )

(define (tag b)
  (integer->char b))
(define (must-read-byte port)
  (let ((b (read-byte port)))
    (if (eof-object? b) (error 'early-eof) b) ) )

(define (decode-expr port with-spans exprs patts)
  (define/contract (go-e e)
    ((or/c monte-expr/c monte-method/c monte-matcher/c) . -> . list?)
    ;(printf "(go-e ~v)\n" e)
    (decode-expr port with-spans (append exprs (list e)) patts) )
  (define (go-p p)
    ;(printf "(go-p ~v)\n" p)
    (decode-expr port with-spans exprs (append patts (list p))) )

  (define (next-span)
    (if with-spans
        (let ([span-tag (tag (must-read-byte port))]
              [start-line (decode-int port)]
              [start-col (decode-int port)]
              [end-line (decode-int port)]
              [end-col (decode-int port)])
          (case span-tag
            ((#\S #\B) #t)
            (else (error 'invalid-span-tag))
            ))
        #f))

  (define (next-expr)
    (list-ref exprs (decode-int port)) )
  (define (next-exprs)
    (for/list [(ix (in-range (decode-int port)))] (next-expr)) )
  (define (next-named-exprs)
    (apply append
           (for/list [(ix (in-range (decode-int port)))]
             `(,(next-expr) . => . ,(next-expr)) ) ) )
  (define (next-patt)
    (list-ref patts (decode-int port)) )
  (define (next-patts)
    (for/list [(ix (in-range (decode-int port)))] (next-patt)) )
  (define (next-named-patts)
    (for/list [(ix (in-range (decode-int port)))]
      (let* ((name (next-expr))
             (patt (next-patt))
             (default (next-expr)))
        `(=> ,name ,patt ,@(opt "default" default))
        )))

  (define (opt kw node)
    (if (eq? node 'not-present) '() `(,(string->keyword kw) ,node)))
  (define (opt-rep kw nodes)
    (if (equal? nodes '()) '() `(,(string->keyword kw) ,@nodes)))
  (define (opt-doc node)
    (if (equal? node "") '() `(#:doc ,node)))

  (define (decode-pattern)
    (let ((ptag (tag (must-read-byte port))))
      (case ptag
        [(#\A) (let ([expr (next-expr)]
                     [patt (next-patt)]
                     [span (next-span)])
                 `(via ,expr ,patt))]
        [(#\B) (let ([name (string->symbol (decode-str port))]
                     [span (next-span)])
                 `(&& ,name))]
        [(#\F) (let ([name (string->symbol (decode-str port))]
                     [guard (next-expr)]
                     [span (next-span)])
                 `(final ,name ,@(opt "guard" guard)) )]
        [(#\I) (let ([guard (next-expr)]
                     [span (next-span)])
                 `(ignore ,@(opt "guard" guard)) )]
        [(#\L) (let ([patts (next-patts)]
                     [span (next-span)])
                 `(list ,@patts))]
        [(#\V) (let ([name (string->symbol (decode-str port))]
                     [guard (next-expr)]
                     [span (next-span)])
                 `(var ,name ,@(opt "guard" guard)) )]
        [else (error (format "pattern tag??? ~v" ptag))]
        ) ) )

  (let ((b (read-byte port)))
    (if (eof-object? b) (list exprs patts)
        (case (tag b)
          [(#\A) (let ([target (string->symbol (decode-str port))]
                       [expr (next-expr)]
                       [span (next-span)])
                   (go-e `(set! ,target ,expr)))]
          [(#\B) (let ([name (string->symbol (decode-str port))]
                       [span (next-span)])
                   (go-e `(binding ,name)))]
          [(#\C) (go-e (let ([target (next-expr)]
                             [verb (decode-str port)]
                             [args (next-exprs)]
                             [kwargs (next-named-exprs)]
                             [span (next-span)])
                         `(send ,target ,verb ,@args ,@kwargs) )) ]
          [(#\D) (let ([patt (next-patt)]
                       [exit (next-expr)]
                       [expr (next-expr)]
                       [span (next-span)])
                   (go-e `(def ,patt ,@(opt "exit" exit) ,expr)))]
          [(#\E) (let ([escPatt (next-patt)]
                       [escExpr (next-expr)]
                       [catchPatt (next-patt)]
                       [catchExpr (next-expr)]
                       [span (next-span)])
                   (go-e `(escape ,escPatt ,escExpr
                                  #:catch ,catchPatt ,catchExpr)))]
          [(#\e) (let ([escPatt (next-patt)]
                       [escExpr (next-expr)]
                       [span (next-span)])
                   (go-e `(escape ,escPatt ,escExpr)))]
          [(#\F) (let ([try (next-expr)]
                       [finally (next-expr)]
                       [span (next-span)])
                   (go-e `(try ,try #:finally ,finally)))]
          [(#\Y) (let ([try (next-expr)]
                       [catchPatt (next-patt)]
                       [catchExpr (next-expr)]
                       [span (next-span)])
                   (go-e `(try ,try
                               #:catch ,catchPatt ,catchExpr)))]
          [(#\H) (let ([e (next-expr)]
                       [span (next-span)])
                   (go-e `(hide ,e)))]
          [(#\I) (let ([check (next-expr)]
                       [yes (next-expr)]
                       [no (next-expr)]
                       [span (next-span)])
                   (go-e (if (eq? no 'not-present)
                             `(if ,check ,yes)  ;; @@test for this case?
                             `(if ,check ,yes ,no))))]
          [(#\L) (let ([lit (decode-literal port)]
                       [span (next-span)])
                   (go-e lit))]
          [(#\M) (let ([doc (decode-str port)]
                       [verb (decode-str port)]
                       [patts (next-patts)]
                       [namedPatts (next-named-patts)]
                       [guard (next-expr)]
                       [block (next-expr)]
                       [span (next-span)])
                   (go-e `(method ,verb (,@patts ,@namedPatts)
                            ,@(opt "guard" guard)
                            ,@(opt-doc doc)
                            ,block)) )]
          [(#\O) (let ([doc (decode-str port)]
                       [patt (next-patt)]
                       [asExpr (next-expr)]
                       [implements (next-exprs)]
                       [methods (next-exprs)]
                       [matchers (next-exprs)]
                       [span (next-span)])
                   (go-e `(object ,patt
                            ,@(opt "as" asExpr)
                            ,@(opt-rep "implements" implements)
                            ,@(opt-doc doc)
                            ,@methods
                            ,@matchers
                            )))]
          [(#\N) (let ([name (decode-str port)]
                       [span (next-span)])
                   (go-e (string->symbol name)))]
          [(#\P) (go-p (decode-pattern))]
          [(#\R) (let ([patt (next-patt)]
                       [block (next-expr)]
                       [span (next-span)])
                   (go-e `(match
                           ,patt
                           ,block)) )]
          [(#\S) (let ([exprs (next-exprs)]
                       [span (next-span)])
                   (go-e `(sequence ,@exprs)))]
          [else (error (format "expr tag??? ~v ~v" b (integer->char b)))]
          ) ) ) )

(define (decode-literal port)
  (case (tag (must-read-byte port))
    [(#\C) `(literal ,(read-char port))]
    [(#\D) `(literal ,(floating-point-bytes->real (read-bytes 8 port) #t))]
    [(#\I) `(literal ,(zz (decode-int port)))]
    [(#\N) 'not-present]
    [(#\S) `(literal ,(decode-str port))]
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
