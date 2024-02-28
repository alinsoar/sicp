#lang racket

(require racket/trace)

(define (inc x) (+ x 1))

(define (dec x) (- x 1))

;; recursive
(define (xp a b)
  (if (= a 0)
      b
      (inc (xp (dec a) b))))

;; iterative
(define (yp a b)
  (if (= a 0)
      b
      (yp (dec a) (inc b))))

(trace xp)
(trace yp)

(module+ test
  (xp 10 10)

  (yp 10 10))

(module+ export (provide inc dec))
