#lang racket


(define theta (/ (+ 1 (sqrt 5)) 2))

(define tolerance 0.00000000001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2) (< (abs (- v1 v2)) tolerance))
  (define (try guess counter)
    (display (format "~a\t~a\n" counter guess))
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next (+ 1 counter)))))
  (try first-guess 0))

(define f (lambda (x) (/ (log 1000) (log x))))
(define average (lambda (x y) (/ (+ x y) 2)))

;;; values between 0 and 1 will generate negative logarithms after
;;; recurring
(fixed-point f 2)

(fixed-point (lambda (x) (average x (f x))) 2)


;;; x^x = 1000
;;; x*log(x) = log(1000)
;;; x -> log(1000)/log(x) -- first form
;;; x -> (log(1000)/log(x) + x)/2 -- second form
