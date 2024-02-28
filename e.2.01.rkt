#lang racket


(define (gcd a b) (if (= b 0) a (gcd b (remainder a b))))

(define (numer x) (car x))

(define (denum x) (cdr x))

(define (print-rat x)
  (display (car x))
  (display "/")
  (display (cdr x)))

(define (make-rat a b)
  (let ((g (abs (gcd a b))))
    (let ((a1 (/ (abs a) g))
          (b1 (/ (abs b) g)))
      (cons (if (> (* a b) 0) a1 (- a1))
            b1))))

(define rat (make-rat 5 -15))

(print-rat rat)

(define rat (make-rat 5 15))

(print-rat rat)

(define rat (make-rat -5 -15))

(print-rat rat)

