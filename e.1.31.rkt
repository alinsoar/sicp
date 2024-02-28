#lang racket


(define (f x)
  (define (square x) (* x x))
  (define (inc x) (+ x 1))
  (define (dec x) (- x 1))
  (/ (square x) (* (inc x) (dec x))))

(define (next k) (+ k 2))

;;; iterative
(define (product-iter term a next b)
  (define (iter a res)
    (if (> a b)
        res
        (iter (next a) (* res (term a)))))
  (iter a 1))

;;; recursive
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (pi n)
  (define pi/4 (* (/ 2 3) (product term 4. next (* 2 n))))
  (* 4 pi/4))

(pi 1000000)

(define (pi-iter n)
  (define pi/4-iter (* (/ 2 3) (product-iter term 4. next (* 2 n))))
  (* 4 pi/4-iter))

(pi-iter 1000000)
