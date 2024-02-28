#lang racket

(require "sicp.rkt")
(GETMOD 3 50)
(GETMOD 3 66 without pairs)

(define (pairs s t)
  (define make-pair
    (lambda (a b) (list a b)))
  (cons-stream
   (make-pair (stream-car s) (stream-car t))
   (interleave
    (interleave (stream-map (lambda (x) (make-pair (stream-car s) x))
                            (stream-cdr t))
                (stream-map (lambda (x) (make-pair x (stream-car t)))
                            (stream-cdr s)))
    (pairs (stream-cdr s) (stream-cdr t)))))

(module+ test
  (define (test s t)
    (print-row-n (pairs s t) 20)
    "---")
  (test integers integers)
  'done)


