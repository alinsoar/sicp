#lang racket


(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))


(define z (cons 1 2))
(car z)
(cdr z)

;;; car--1
(((lambda (x y)
    (lambda (m) (m x y)))
  1 2)
 (lambda (p q) p))

;;; cdr--1
(((lambda (x y)
    (lambda (m) (m x y)))
  1 2)
 (lambda (p q) q))

;;; car--2
((lambda (m) (m 1 2))
 (lambda (p q) p))

;;; cdr--2
((lambda (m) (m 1 2))
 (lambda (p q) q))

;;; car--3
((lambda (p q) p) 1 2)

;;; cdr--3
((lambda (p q) q) 1 2)


