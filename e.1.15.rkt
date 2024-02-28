#lang racket


(require racket/math)

(define tolerance 1e-2)

(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (display angle)
  (newline)
  (if (not (> (abs angle) tolerance))
       angle
       (p (sine (/ angle 3.0)))))

;; complexity = log_3(n/precision)-1 steps

;; (sine (/ pi 2))


;;; log_ab(x) = m
;;; (ab)^m=x
;;; m*log_k(ab)=log_k(k)
;;; m = log_k(x) / log_k(ab)
;;; log_ab(x) = log_k(x)/log_k(ab)
;;; a=3, b=1
;;; log_3(x) = ln(x)/ln(3)
(define (complexity x)
  (/ (log (/ x tolerance))
     (log 3)))

(define (test x)
  (define (msg)
    (display "will stop in ")
    (display (ceiling (complexity x)))
    (display " steps")
    (newline))
  (msg)
  (sine x))

(test 200)


