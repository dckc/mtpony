#lang racket

(require "compiling-with-exceptions.rkt")


;; Test cases:


(define/return (max a b)
  (when (> a b)
    (return a))
  
  (return b))
     
(max 20 40)



(try 
 (throw 10)
 (λ (ex)
   (printf "caught: ~s~n" ex))
 (printf "finally~n"))


(define/return (g)
  (try 
   (return 20)
   (λ (ex)
     (return 50)
     (printf "caught: ~s~n" ex))
   (printf "finally in g~n")))

(g)




(define/return (f)
  (try 
   (throw 10)
   (λ (ex)
     (return 50)
     (printf "caught: ~s~n" ex))
   (printf "finally in f~n")))

(f)




(define/return (h)
  (try
   (begin
     (while #t
            (try 
             (break)
             (λ (ex) (printf "wrong exception!~n"))))
     (throw 10))
   (λ (ex) (printf "right exception!~n"))
   (printf "and, finally.~n")))

(h)  



(define/return (s)
  (try
   (begin
     (while #t
            (try 
             (return 200)
             (λ (ex) (printf "wrong exception!~n"))
             (printf "first finally~n")))
     (throw 10))
   (λ (ex) (printf "wrong exception!~n"))
   (printf "second finally~n")))

(s)  


(try
 (try-finally
  (throw 100)
  (printf "me first~n"))
 (λ (ex)
   (printf "me second: ~s~n" ex)))
  
