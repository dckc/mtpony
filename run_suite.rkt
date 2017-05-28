#lang racket

(require json)
(require file/sha1)

(require "mastrd.rkt")

(struct doctest (source section lineno want MAST))
(define (hash->doctest obj)
  (doctest
       (hash-ref obj 'source)
       (hash-ref obj 'section)
       (hash-ref obj 'lineno)
       (hash-ref obj 'want)
       (hex-string->bytes (hash-ref obj 'MAST))))

(define monte-suite
  (call-with-input-file "test/doctest_wm.json"
    (lambda (t)
      (map hash->doctest (read-json t)))))

(define (run test-case)
  (call-with-input-bytes
   (doctest-MAST test-case)
   (lambda (mast-port)
     (printf "~A~A: " (doctest-section test-case) (doctest-lineno test-case))
     (pretty-print (decode-mast mast-port)))))


(call-with-input-file "test/doctest_wm.json"
  (lambda (input)
    (for ([test-hash (read-json input)])
      (run (hash->doctest test-hash)))))
