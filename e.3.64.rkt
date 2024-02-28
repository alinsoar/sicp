#lang racket

(require "sicp.rkt")
(GETMOD 3 50)
(GETMOD 1 06)
(GETMOD 3 63)

(define (stream-limit s tolerance)
  (let ((first (stream-car s))
        (s (stream-cdr s)))
    (if (< (abs (- first (stream-car s)))
           tolerance)
        (stream-car s)
        (stream-limit s tolerance))))

(module+ test
  (define (test s tolerance)
    (d (stream-limit s tolerance))
    "---")
  (test (sqrt-stream 2) .01)
  (test (sqrt-stream 2) 1e-10)
  'done)


