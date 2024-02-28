#lang racket

(require (submod "e.1.37.rkt" export))

(define d
  (lambda (k)
    (if (zero? (remainder (- k 1) 3))
        (* 2.0 (+ 1 (quotient k 3)))
        1.0)))

(define n
  (lambda (k) 1.0))

(module* test #f
  ;; the interactive-recursive-process version does not work for large `k`
  (- (- (exp 1) 2)
     (cont-frac-iter-rec-co
      n
      d
      0
      1111
      (lambda (x) x)))

  (- (- (exp 1) 2)
     (cont-frac-iter
      n
      d
      11))

  ;; the recursive-process version does not work for large `k`
  (- (- (exp 1) 2)
     (cont-frac-rec
      n
      d
      0
      11)))

