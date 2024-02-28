#lang racket


(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (simpson-integral f a b n)
  "integrate f from a to b, dividing the interval [a,b] in n pieces"
  (define h (/ (- b a) n))
  (define (next i) (+ i 2))
  (define (term k) (f (+ a (* k h))))
  (/
   (* h (+ (* 4 (sum term 1 next (- n 1) ) )
           (* 2 (sum term 2 next (- n 2) ) )
           (term 0)
           (term n)))
   3))

(define (cube x) (* x x x))
(simpson-integral cube 0. 1 100)
(simpson-integral cube 0. 1 1000)

(define (ct x) 12)
(simpson-integral ct 0 10 100)

(define (identity x) x)
(simpson-integral identity 0 10 100)

(define (linear x) (lambda (y) (* x y)))
(simpson-integral (linear 1) 0 10 100)
(simpson-integral (linear 2) 0 10 100)


(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2)) add-dx b)
     dx))

;;; this is not so accurate as Simson's method -- .00002 error
(integral cube 0 1 0.01)

;; Simpson's integration formula
;; http://en.wikipedia.org/wiki/Simpson's_rule

