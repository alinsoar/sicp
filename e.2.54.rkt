#lang racket

;;; alpha * alpha -> boolean
(define (equal0? l1 l2)
  (cond ((and (symbol? l1) (symbol? l2)) ; both symbols
         (eq? l1 l2))
        ((or (symbol? l1) (symbol? l2)) ; 1 symbol, 1 list
         false)
        ((null? l1) (null? l2))         ; both empty list
        ((or (null? l1) (null? l2))     ; only 1 empty list
         false)
        (else
         (and (equal0? (car l1) (car l2))
              (equal0? (cdr l1) (cdr l2))))))

(define (equal1? l1 l2)
  (define (iter l1 l2 co)
    (cond ((and (symbol? l1) (symbol? l2))
           (co (eq? l1 l2)))
          ((or (symbol? l1) (symbol? l2))
           (co false))
          ((null? l1) (co (null? l2)))
          ((or (null? l1) (null? l2))
           (co false))
          (else
           (iter (car l1) (car l2)
                 (lambda (x)
                   (iter (cdr l1) (cdr l2)
                         (lambda (y)
                           (co (and x y)))))))))
  (iter l1 l2 (lambda (x) x)))

(define (equal2? l1 l2)
  (define (iter l1 l2 co)
    (cond ((and (symbol? l1) (symbol? l2))
           (co (eq? l1 l2)))
          ((or (symbol? l1) (symbol? l2))
           (co false))
          ((and (number? l1) (number? l2))
           (co (= l1 l2)))
          ((or (number? l1) (number? l2))
           (co false))
          ((null? l1) (co (null? l2)))
          ((or (null? l1) (null? l2))
           (co false))
          (else
           (iter (car l1) (car l2)
                 (lambda (x)
                   (iter (cdr l1) (cdr l2)
                         (lambda (y)
                           (co (and x y)))))))))
  (iter l1 l2 (lambda (x) x)))
(module+ test

  (equal0? '(this is a list) '(this is a list))

  (equal0? '(this is a list) '(this (is a) list))

  (equal0? '(this is (a list) abc) '(this is (a list) abc))

  (equal0? '(a) '(a))

  (equal0? '((a)((x)y)q)'((a)((x)y)q))

  (equal0? '(a) '())
  
  (equal1? '(this is a list) '(this is a list))

  (equal1? '(this is a list) '(this (is a) list))

  (equal1? '(this is (a list) abc) '(this is (a list) abc))

  (equal1? '(a) '(a))

  (equal1? '((a)((x)y)q)'((a)((x)y)q))

  (equal1? '(a) '()))

(module+ export
  (provide equal0?
           equal1?
           equal2?))
