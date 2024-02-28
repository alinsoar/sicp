#lang racket

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set0 set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set0 (cdr set1) set2)))
        (else (intersection-set0 (cdr set1) set2))))

(define (intersection-set set1 set2)
  (define (iter s1 co)
    (cond ((null? s1) (co '()))
          ((element-of-set? (car s1) set2)
           (iter (cdr s1)
                 (lambda (x) (co (cons (car s1) x)))))
          (else (iter (cdr s1)
                 (lambda (x) (co x))))))
  (iter set1 (lambda (x) x)))

(define (union-set0 set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set? (car set1) set2)
         (union-set0 (cdr set1) set2))
        (else
         (union-set0 (cdr set1) (cons (car set1) set2)))))

(define (union-set set1 set2)
  (define (iter s1 co)
    (cond ((null? s1) (co set2))
          ((element-of-set? (car s1) set2)
           (iter (cdr s1)
                 (lambda (x) (co x))))
          (else (iter (cdr s1)
                      (lambda (x)
                        (co (cons (car s1) x)))))))
  (iter set1 (lambda (x) x)))

(module+ test
  
  (intersection-set0 '(1 2 3) '())
  (intersection-set0 '(1 2 3) '(2))
  (intersection-set0 '(1 2 3) '(1 3 5 4))
  (intersection-set0 '(1 2 3) '(5 4))
  (intersection-set '(1 2 3) '())
  (intersection-set '(1 2 3) '(2))
  (intersection-set '(1 2 3) '(1 3 5 4))
  (intersection-set '(1 2 3) '(5 4))

  (union-set0 '(1 2 3) '(4 5 6 2 7 3))
  (union-set0 '(8 1 2 3) '(4 5 6 1 2 7 3))
  (union-set0 '(1) '(2))
  (union-set '(1 2 3) '(4 5 6 2 7 3))
  (union-set '(8 1 2 3) '(4 5 6 1 2 7 3))
  (union-set '(1) '(2)))

(module+ export
  (provide union-set))
