#lang racket

(define (last-pair l)
  (cond ((null? l) l)
        ((null? (cdr l)) l)
        (else (last-pair (cdr l)))))

(define (last-pair2 l)
  (define (iter l break)
    (if (null? (cdr l))
        (break l)
        (iter (cdr l) break)))
  (if (null? l)
      '()
      (iter l (lambda (x) x))))

(module+ test

 (last-pair '())
 (last-pair2 '())
 (last-pair '(1 2 3 4))
 (last-pair2 '(1 2 3 4))
 )




