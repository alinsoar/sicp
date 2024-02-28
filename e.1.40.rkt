#lang racket


;;; when the tolerance is small, for some big numbers it stucks,
;;; reason of fp arithmetic
(define tolerance 1e-7)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

;;; sometimes, when the delta is too small, it may stuck
(define dx 1e-10)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  (define (cube x) (* x x x))
  (define (square x) (* x x))
  (define (identity x) x)
  (lambda (x)
    (+ (cube x) (* a (square x)) (* b (identity x)) c)))

(define (test x)
  (- x
     (let ((w (newtons-method (cubic 0 0 (- x)) 1)))
       (* w w w ))))

(test 15000)

(newtons-method (cubic 1 1 1) 1)        ; -1+1-1+1 = 0
