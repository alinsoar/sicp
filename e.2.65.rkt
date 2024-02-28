#lang racket

(require (submod "e.2.63.rkt" export))
(require (submod "e.2.64.rkt" export))

(define (union-set t1 t2)
  (define (iter l1 l2 co)
    (cond ((null? l1) (co l2))
          ((null? l2) (co l1))
          ((= (car l1) (car l2))
           (iter (cdr l1) (cdr l2)
                 (lambda (x)
                   (co (cons (car l1) x)))))
          ((< (car l1) (car l2))
           (iter (cdr l1) l2
                 (lambda (x)
                   (co (cons (car l1) x)))))
          (else (iter l1 (cdr l2)
                      (lambda (x)
                        (co (cons (car l2) x)))))))
  (list->tree (iter (tree->list-2 t1)
                    (tree->list-2 t2)
                    (lambda (x) x))))

(module+ test
  (tree->list-2
   (union-set (list->tree '(1 2 3 4 5 6))
              (list->tree '(0 1 2 4 5 6 7 8 9)))))

(define (intersection-set t1 t2)
  (define (iter l1 l2 co)
    (cond ((null? l1) (co '()))
          ((null? l2) (co '()))
          ((= (car l1) (car l2))
           (iter (cdr l1) (cdr l2)
                 (lambda (x)
                   (co (cons (car l1) x)))))
          ((< (car l1) (car l2))
           (iter (cdr l1) l2
                 (lambda (x) (co x))))
          (else (iter l1 (cdr l2)
                      (lambda (x)
                        (co x))))))
  (list->tree (iter (tree->list-2 t1)
                    (tree->list-2 t2)
                    (lambda (x) x))))

(module+ test
  (tree->list-2
   (intersection-set (list->tree '(0 2 3 7 ))
                     (list->tree '(1 2 3 4 5 6 7)))))

