#lang racket

(require "sicp.rkt")
(GETMOD 3 50)
(GETMOD 1 06)

(define (sqrt-improve guess x)
  (d ":" guess x)
  (average guess (/ x guess)))

(define (sqrt-stream0 x)
  "guesses is a stream"
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

;; EXERCISE 3.63
(define (sqrt-stream1 x)
  "sqrt-stream is a function that returns a new stream at each
iteration."
  (cons-stream 1.0
               (stream-map (lambda (guess)
                             (sqrt-improve guess x))
                           ;; recompute the sqrt stream again
                           (sqrt-stream1 x))))

(module+ test
  (define (test s)
    (print-column-n s 10)
    "---")
  (test (sqrt-stream0 2))
  (test (sqrt-stream1 2))
  'done)


(module+ export
  (provide (rename-out (sqrt-stream0 sqrt-stream))))
