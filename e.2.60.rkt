#lang racket


;;; duplicate-elements set is slower than the non-dup version, because
;;; it scans a longer list, that may have duplicates
(define (element-of-set? x set)
  (define (iter s co)
    (cond ((null? s) (co false))
          ((equal? (car s) x)
           (co true))
          (else (iter (cdr s)
                      (lambda (x)
                        (co x))))))
  (iter set (lambda (x) x)))

(element-of-set? 'x '(1 2 3 4 x 5 6))
(element-of-set? 'y '(1 2 3 4 x 5 6))

;;; the current duplicate-elements set is faster than the non-dup
;;; version, but this one uses more memory.
(define (adjoin-set x set) (cons x set))

;;; duplicate-elements set is faster than the non-dup version, but
;;; this one uses more memory. The code is the same as append's.
(define (union-set set1 set2)
  (define (iter s co)
    (if (null? s)
        (co set2)
        (iter (cdr s)
              (lambda (x)
                (co (cons (car s) x))))))
  (iter set1 (lambda (x) x)))


(union-set '(1 2 3) '(1 2 3))
(union-set '() '(1 2 3))
(union-set '() '())
(union-set '(2) '())
(union-set '(2) '(2))

;;; duplicate-elements set has the same code as non-dup version, but
;;; it is slower, because it must scan lists with duplicates.
(define (intersection-set set1 set2)
  (define (iter s co)
    (cond ((null? s) (co '()))
          ((element-of-set? (car s) set2)
           (iter (cdr s)
                 (lambda (x)
                   (co (cons (car s) x)))))
          (else (iter (cdr s)
                      (lambda (x)
                        (co x))))))
  (iter set1 (lambda (x) x)))

(intersection-set '() '())
(intersection-set '() '(1))
(intersection-set '(1) '())
(intersection-set '(1) '(1))
(intersection-set '(0 ) '(1 2))
(intersection-set '(0 2 3) '(5 1 2 9))







