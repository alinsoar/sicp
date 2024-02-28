#lang racket

(require "sicp.rkt")
(GETMOD 3 50)
(GETMOD 3 59)
(GETMOD 3 60)

(define (invert-unit-series s)
  (cons-stream 1
               (mul-series (scale-stream (stream-cdr s) -1)
                           (invert-unit-series s))))

(module+ test
  (define (test s)
    (print-row-n (invert-unit-series s) 20)
    (d)
    (print-row-n (mul-series (invert-unit-series s)
                             s)
                 20)
    "---")
  (test ones)
  (test integers)
  ;; SINE does not work here, as the 1st coefficient is not 1.
  (test cosine-series)
  'done)



(module+ export
    (provide invert-unit-series
             ))
