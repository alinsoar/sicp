#lang racket


(define (term x) x)
(define (next i) (+ i 1))

;;; recursive process
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

;;; iterative process
(define (accumulate-iter combiner null-value term a next b)
  (define (iter a res)
    (if (> a b)
        res
        (iter (next a) (combiner (term a) res))))
  (iter a null-value))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (sum-iter term a next b)
  (accumulate-iter + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (product-iter term a next b)
  (accumulate-iter * 1 term a next b))

(define (factorial n)
  (product term 1 next n))

(define (factorial-iter n)
  (product-iter term 1 next n))

(factorial 10)

(factorial-iter 10)

(sum term 1 next 100)

(sum-iter term 1 next 100)

