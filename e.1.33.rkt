#lang racket

(require (submod "e.1.23.rkt" export))
(require "sicp.rkt")

;;; recursive process
(define (filtered-accumulate0 p > combiner null-value term a next b)
  (cond ((> a b) null-value)
        ((p a)
         (combiner (term a)
                   (filtered-accumulate0 p > combiner null-value term (next a) next b)))
        (else
         (filtered-accumulate0 p > combiner null-value term (next a) next b))))

;;; iterative process
(define (filtered-accumulate1 p > combiner null-value term a next b)
  (define (iter a res)
    (cond ((> a b) res)
          ((p a)
           (iter (next a) (combiner res (term a))))
          (else
           (iter (next a) res))))
  (iter a null-value))

;;; iterative-recursive process
(define (filtered-accumulate p > combiner null-value term a next b)
  (define (iter a co)
    (cond ((> a b) (co null-value))
          ((p a) (iter (next a)
                       (lambda (x)
                         (co (combiner x (term a))))))
          (else
           (iter (next a)
                 (lambda (x)
                   (co x))))))
  (iter a (lambda (x) x)))

(define (sum-squared-primes f/a)
  (lambda (a b)
    (define (term i) (* i i))
    (define (next i) (+ i 1))
    (f/a prime? > + 0 term a next b)))

(define (product-rel-prime-int f/a)
  (lambda (n)
    (define (rel-prime? x)
      (define (gcd a b) (if (= b 0) a (gcd b (remainder a b))))
      (equal? 1 (gcd x n)))
    (define (next i) (+ i 1))
    (define (term x) x)
    (f/a rel-prime? > * 1 term 1 next (- n 1))))

(module+ test
  ((sum-squared-primes filtered-accumulate0) 3 10)
  ((sum-squared-primes filtered-accumulate1) 3 10)
  (d)
  ((sum-squared-primes filtered-accumulate) 3 10)
  ((sum-squared-primes filtered-accumulate) 3 10)
  (d)
  ((product-rel-prime-int filtered-accumulate0) 10)
  ((product-rel-prime-int filtered-accumulate1) 10))

(module+ export
  (provide filtered-accumulate))
