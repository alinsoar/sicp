#lang racket


(define square (lambda (x) (* x x)))

(define (square-tree l)
  (cond ((null? l) '())
        ((pair? (car l))
         (cons (square-tree (car l))
               (square-tree (cdr l))))
        (else (cons (square (car l))
                    (square-tree (cdr l))))))

(define (square-tree l)
  (map (lambda (x)
         (if (pair? x)
             (square-tree x)
             (square x)))
       l))

(define (square-tree l)
  (define (iter l co)
    (cond ((null? l) (co '()))
          ((pair? (car l))
           (iter (car l)
                 (lambda (x)
                   (iter (cdr l)
                         (lambda (y)
                           (co (cons x y)))))))
          (else (iter (cdr l)
                      (lambda (x)
                        (co
                         (cons (square (car l)) x)))))))
  (iter l (lambda (x) x)))

(square-tree '(1 2 3 9))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))


