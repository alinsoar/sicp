#lang racket

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

(define (adjoint-set x set)
  (define (iter s co)
    (cond ((null? s) (co (list x)))
          ((= x (car s)) (co s))
          ((< x (car s)) (co (cons x s)))
          (else (iter (cdr s)
                      (lambda (x)
                        (co (cons (car s) x)))))))
  (iter set (lambda (x) x)))

(module+ test
  (adjoint-set 0 '(1 2 3))
  (adjoint-set 1 '(0 2 3))
  (adjoint-set 1 '())
  (adjoint-set 1 '(1))
  (adjoint-set 1 '(0 1 2)))
