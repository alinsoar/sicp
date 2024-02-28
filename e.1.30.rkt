#lang racket



(define (f x) (+ 1(* x x)))
(define (next a) (+ 1 a))

;;; recursive
(define (sum term a b next)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) b next))))

(sum f 1 100 next)

;;; iterative
(define (sum-iter term a b next)
  (define (iter a res)
    (if (> a b)
        res
        (iter (next a) (+ res (term a)))))
  (iter a 0))

(sum-iter f 1 100 next)




