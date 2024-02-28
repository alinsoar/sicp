#lang racket


(define (fringe l)
  (define (iter l co)
    (cond ((null? l) (co '()))
          ((pair? (car l))
           (iter (cdr l)
                 (lambda (x)
                   (co (append (fringe (car l))
                               x)))))
          (else (iter (cdr l)
                      (lambda (x)
                        (co (cons (car l) x)))))))
  (iter l (lambda (x) x)))

(define (fringe l)
  (define (iter l res)
    (cond ((null? l) res)
          ((pair? (car l))
           (iter (cdr l)
                 (append res
                         (fringe (car l)))))
          (else (iter (cdr l)
                      (append res
                              (list (car l)))))))
  (iter l '()))

(define (fringe l)
  (cond ((null? l) '())
        ((pair? (car l))
         (append (fringe (car l))
                 (fringe (cdr l))))
        (else (cons (car l)
                    (fringe (cdr l))))))

(define x (list (list 1 2) (list 3 4)))

(fringe x)

(fringe (list x x))




