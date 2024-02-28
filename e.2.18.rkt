#lang racket

(define (rev0 l)
  (cond ((null? l) l)
        (else (append (rev0 (cdr l)) (cons (car l) '())))))

(define (rev1 l)
  (define (iter l co)
    (if (null? l)
        (co '())
        (iter (cdr l)
              (lambda (x)
                (cons (car l)
                      (co x))))))
  (iter l (lambda (x) x)))

(module+ test
  (rev0 '(1 2 3 4))
  (rev1 '(1 2 3 4)))

(module+ export
  (provide rev0)
  (provide rev1))

