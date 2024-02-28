#lang racket


(require racket/trace)

(define square (lambda (x) (* x x)))

(define (f g)
  (g 2))

(f square)

(f (lambda (z) (* z (+ z 1))))

(trace f)

;;; it tries to apply 2 on 2
(f f)





