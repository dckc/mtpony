#lang racket

(require json)
(require file/sha1)

(require "doctest-suite.rkt")
(require "mastrd.rkt")

(for ([test-case monte-suite])
     (printf "\n\n~A-~A: ~A\n" (doctest-section test-case) (doctest-lineno test-case) (doctest-source test-case))
     (pretty-print (doctest-AST test-case)))

