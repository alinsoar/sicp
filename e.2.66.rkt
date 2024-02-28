#lang racket

(require (submod "e.2.63.rkt" export))
(require (submod "e.2.64.rkt" export))

(define (lookup0 tree key)
  (let ((e (entry tree))
        (l (left-branch tree))
        (r (right-branch tree)))
    (cond ((< key e)
           (and (not (null? l))
                (lookup0 l key)))
          ((> key e)
           (and (not (null? r))
                (lookup0 r key)))
          (else true))))


(define (lookup1 tree key)
  (define (iter t co)
    (cond ((null? t) (co false))
          ((< key (car t))
           (iter (left-branch t)
                 (lambda (x) (co x))))
          ((> key (car t))
           (iter (right-branch t)
                 (lambda (x) (co x))))
          (else (co true))))
  (iter tree (lambda (x) x)))


(module+ test
  (lookup1 (list->tree '(0 1 2 3 4 5 6 7 8 9))
           -1)

  (lookup1 (list->tree '(0 1 2 3 4 5 6 7 8 9))
           5)
  (lookup0 (list->tree '(0 1 2 3 4 5 6 7 8 9))
           -1)

  (lookup0 (list->tree '(0 1 2 3 4 5 6 7 8 9))
           5))


