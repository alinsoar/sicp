#lang racket

(require "sicp.rkt")
(GETMOD 3 50)

(define (expand num den radix)
  "expanding the FLOATING POINT DIVISION of NUM by DEN in base RADIX"
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

(module+ test
  (define test
    (lambda (num den radix)
      (d (~a num "/" den " in radix " radix "=") (/ (+ 0. num) den) )
      (print-row-n (expand num den radix)
                   100)))
  (test 1 7 10)
  (test 3 8 10)
  (test 9 8 10)
  (test 1 5 2)
  'done)

