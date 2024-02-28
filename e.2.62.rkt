#lang racket

(define (union-set set1 set2)
  (define (iter s1 s2 co)
    (cond ((null? s1) (co s2))
          ((null? s2) (co s1))
          ((= (car s1) (car s2))
           (iter (cdr s1) (cdr s2)
                 (lambda (x)
                   (co (cons (car s1) x)))))
          ((< (car s1) (car s2))
           (iter (cdr s1) s2
                 (lambda (x)
                   (co (cons (car s1) x)))))
          (else (iter s1 (cdr s2)
                      (lambda (x)
                        (co (cons (car s2) x)))))))
  (iter set1 set2 (lambda (x) x)))

(module+ test
  (union-set '() '())
  (union-set '() '(1))
  (union-set '(1) '())
  (union-set '(1) '(1))
  (union-set '(1) '(2))
  (union-set '(1 2) '(1 5))
  (union-set '(1 2) '(3 5))
  (union-set '(3 5) '(1 2))
  )


