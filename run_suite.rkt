#lang racket

(require file/sha1)
(require json-parsing)

(require "mastrd.rkt")

(define (run-test test-case)
  (let* ([section (hash-ref test-case 'section)]
         [lineno (hash-ref test-case 'lineno)]
         [mast-hex (hash-ref test-case 'MAST)]
         [mast-bytes (hex-string->bytes mast-hex)])
    (call-with-input-bytes
     mast-bytes
     (lambda (mast-port)
       (printf "~A~A: " section lineno)
       (pretty-print (decode-mast mast-port))
       ))))

(call-with-input-file "test/doctest_wm.json"
  (lambda (t)
    (let ((suite (json->sjson t)))
      (for ([test-case suite]) (run-test test-case)))))

