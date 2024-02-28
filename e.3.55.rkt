#lang racket

(require "sicp.rkt")
(GETMOD 3 50)

(define partial-sums
  (lambda (s)
    (cons-stream (stream-car s)
                 (add-streams (partial-sums s)
                              (stream-cdr s)))))

(module+ test

  (d "--")
  (print-row-n (partial-sums integers)
               100)

  (d "--")
  (print-row-n (partial-sums ones)
               100)
  
  'done)

(module+ export
  (provide partial-sums))
