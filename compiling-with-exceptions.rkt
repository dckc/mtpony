#lang racket

(require racket/mpair)
(provide (all-defined-out))

;; Control constructs.

(define-syntax (define/return stx)
  (syntax-case stx ()
    [(_ f-params body ...)
     ; =>
     (with-syntax ([return (datum->syntax #'f-params 'return)])
     #'(define f-params 
         (call/ec
          (λ (return) body ...))))]))


(define-syntax (λ/return stx)
  (syntax-case stx ()
    [(_ params body ...)
     ; =>
     (with-syntax ([return (datum->syntax #'params 'return)])
       #'(lambda params (call/ec (λ (return) body ...))))]))
   


(define-syntax (while stx)
  (syntax-case stx ()
    [(_ cond body else)
     ; =>
     (with-syntax ([break    (datum->syntax #'body 'break)]
                   [continue (datum->syntax #'body 'continue)])
       #'(call/ec (λ (break)
                    (letrec ([loop (λ ()
                                     (when cond
                                       (call/ec (λ (continue)
                                                  body))
                                       (loop)))])
                                                  
                      (loop)
                      else))))]
    
    [(_ cond body)
     ; =>
     #'(while cond body (void))]))



;; Standard escape continuations:
(define (continue) (error "cannot continue from this context"))
(define (return) (error "cannot return from this context"))
(define (break) (error "cannot break from this context"))


;; Exceptions.
(define $current-handler (λ (ex) ("no handler installed")))

(define (current-handler) $current-handler)
(define (set-current-handler! handler) (set! $current-handler handler))

(define-syntax (try stx)
  (syntax-case stx ()
    [(_ body handler)
     ; =>
     (with-syntax ([return   (datum->syntax #'body 'return)]
                   [continue (datum->syntax #'body 'continue)]
                   [break    (datum->syntax #'body 'break)])
       #'(let ([$old (current-handler)])
           (let* ([return   (λ args 
                              (set-current-handler! $old)
                              (apply return args))]
                  [continue (λ ()
                              (set-current-handler! $old)
                              (continue))]
                  [break    (λ ()
                              (set-current-handler! $old)
                              (break))])
                (call/ec 
                 (λ (ec)
                   (set-current-handler! 
                    (λ (ex)
                      (set-current-handler! $old)
                      (ec (handler ex))))
                   (let ([rv body])
                     (set-current-handler! $old)
                     rv))))))]    
     
    
    [(_ body handler finally)
     ; =>
     (with-syntax ([return   (datum->syntax #'body 'return)]
                   [continue (datum->syntax #'body 'continue)]
                   [break    (datum->syntax #'body 'break)])
       #'(let* ([$val (void)]
                [$fin (λ () $val)]
                [$old (current-handler)])
           (call/ec
            (λ (fin)
              (let* ([return   (λ args
                                 (set! $fin (λ () (apply return args)))
                                 (fin))]
                     [continue (λ ()
                                 (set! $fin continue)
                                 (fin))]
                     [break    (λ ()
                                 (set! $fin break)
                                 (fin))])
                (call/ec 
                 (λ (ec)
                   (set-current-handler! 
                    (λ (ex)
                      ; if the handler
                      ; throws an exception,
                      ; re-throw it after
                      ; the finally block:
                      (set-current-handler! 
                       (λ (ex*)
                         (set! $fin (λ () (throw ex*))) 
                         (ec #f)))
                      (ec (let ([rv (handler ex)])
                            (set! $fin (λ () rv))))))
                   (let ([rv body])
                     (set! $fin (λ () rv))
                     (fin)))))))
           (set-current-handler! $old)
           (set! $val finally)
           ($fin)))]))
    

(define-syntax (try-finally stx)
  (syntax-case stx ()
    [(_ body finally)
     ; =>
     (with-syntax ([return   (datum->syntax #'body 'return)]
                   [continue (datum->syntax #'body 'continue)]
                   [break    (datum->syntax #'body 'break)])
       #'(let* ([$val (void)]
                [$fin (λ () $val)]
                [$old (current-handler)])
           (call/ec
            (λ (fin)
              (let* ([return   (λ args
                                 (set! $fin (λ () (apply return args)))
                                 (fin))]
                     [continue (λ ()
                                 (set! $fin continue)
                                 (fin))]
                     [break    (λ ()
                                 (set! $fin break)
                                 (fin))])
                (call/ec 
                 (λ (ec)
                   (set-current-handler! 
                    (λ (ex)
                      ; re-throw after finally:
                      (set! $fin (λ () (throw ex)))
                      (fin)))
                   (let ([rv body])
                     (set! $fin (λ () rv))
                     (fin)))))))
           (set-current-handler! $old)
           (set! $val finally)
           ($fin)))]))
    
                    
(define (throw ex)
  ((current-handler) ex))
