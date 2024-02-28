#lang racket

(define (square x) (* x x))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.000001))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt-iter guess x)
  (display guess)
  (newline)
  ;; the recursion never stops -- the second argument is evaluated all
  ;; the time by the interpreter, whatever good-enough? returns.
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x )))

(module+ test
  (new-if (= 2 3) 0 5)
  (new-if (= 1 1) 0 5)
  '(sqrt-iter 1 16)                     ; todo with sandbox
  'ok)

(module+ export
  (provide square
           average
           ))

