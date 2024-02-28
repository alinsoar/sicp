#lang racket


(define (expt a b)
  (define (iter i co)
    (if (zero? i)
        (co 1)
        (iter (- i 1)
              (lambda (x)
                (co (* x a))))))
  (iter b (lambda (x) x)))

(define (exponent a b)
  "exponent of b in decomposition of a"
  (define (iter w co)
    (if (zero? (remainder w b))
        (iter (/ w b) (lambda (x) (co (+ x 1))))
        (co 1)))
  (iter a (lambda (x) x)))

(define (cons a b) (* (expt 2 a) (expt 3 b)))
(define (car z) (exponent z 2))
(define (cdr z) (exponent z 3))

(define z (cons 11 12))

(car z)

(cdr z)




