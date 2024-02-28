#lang racket


(define same-parity
  (lambda w                         ;syntactic sugar for (function . w)
    (define (check-par p w)
      (if (null? w)
          '()
          (if (= p (remainder (car w) 2))
              (cons (car w) (check-par p (cdr w)))
              (check-par p (cdr w)))))
    (check-par (remainder (car w) 2) w)))

(define (same-parity w . r)
  (define (eq-w-parity? x)
    (if (even? w) (even? x) (odd? x)))
  (define (iter l co)
    (cond ((null? l) (co '()))
          ((eq-w-parity? (car l))
           (iter (cdr l)
                 (lambda (x)
                   (co (cons (car l) x)))))
          (else (iter (cdr l)
                      (lambda (x)
                        (co x))))))
  (iter (cons w r) (lambda (x) x)))

(same-parity 1 3 4 5 6 5 7 6 7 8)

(same-parity 2 3 4 5 6 7 8)








