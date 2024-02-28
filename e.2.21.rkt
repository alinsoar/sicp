#lang racket


(define (square-list items)
  (if (null? items)
      '()
      (cons (* (car items) (car items))
            (square-list (cdr items)))))

(define (square-list2 items)
  (map (lambda (x) (* x x)) items))

(define (square-list3 items)
  (define (iter l co)
    (if (null? l)
        (co '())
        (iter (cdr l)
              (lambda (x)
                (co (cons (* (car l) (car l))
                          x))))))
  (iter items (lambda (x) x)))

(square-list '(1 2 3 4 5))

(square-list2 '(1 2 3 4 5))

(square-list3 '(1 2 3 4 5))





