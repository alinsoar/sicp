#lang racket


(define square (lambda (x) (* x x)))

(define (tree-map f l)
  (cond ((null? l) '())
        ((pair? (car l))
         (cons (tree-map f (car l))
               (tree-map f (cdr l))))
        (else (cons (f (car l))
                    (tree-map f (cdr l))))))

(define (tree-map f l)
  (map (lambda (x)
         (if (pair? x)
             (tree-map f x)
             (f x)))
       l))

(define (tree-map f l)
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
                        (co (cons (f (car l))
                                  x)))))))
  (iter l (lambda (x) x)))

(define (square-tree tree) (tree-map square tree))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))


