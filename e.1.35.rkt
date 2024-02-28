#lang racket



(define theta (/ (+ 1 (sqrt 5)) 2))

(define tolerance 0.00000000001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (test)
  (- theta
     (fixed-point
      (lambda (x) (+ 1 (/ 1 x)))
      1.0)))

(fixed-point
 (lambda (x) (+ 1 (/ 1 x)))
 1.0)

(test)

;;; theta is root of theta^2=theta+1, so fixed point of
;;; theta->1+1/theta


