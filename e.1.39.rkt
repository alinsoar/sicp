#lang racket

(require (submod "e.1.37.rkt" export))

(define (tan-cf x k)
  (cont-frac-iter
   (lambda (k) (if (zero? k) x (- (* x x))))
   (lambda (k) (+ 1 (* 2 k)))
   k))

(module+ test
 (-
  (tan-cf 10 20)
  (tan 10)))

