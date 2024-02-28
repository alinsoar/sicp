#lang racket

(require "sicp.rkt")
(GETMOD 4 1 without eval apply)
(GETMOD 4 27)

(define CONS
  '(define (cons x y)
     (lambda (m) (m x y))))

(define CAR
  '(define (car z)
     (z (lambda (p q) p))))

(define CDR
  '(define (cdr z)
     (z (lambda (p q) q))))

(define LIST-REF
  '(define (list-ref items n)
     (if (= n 0)
         (car items)
         (list-ref (cdr items) (- n 1)))))

(define MAP
  '(define (map proc items)
     (if (null? items)
         '()
         (cons (proc (car items))
               (map proc (cdr items))))))

(define SCALE-LIST
  '(define (scale-list items factor)
     (map (lambda (x) (* x factor))
          items)))

(define ADD-LISTS
  '(define (add-lists list1 list2)
     (cond ((null? list1) list2)
           ((null? list2) list1)
           (else (cons (+ (car list1) (car list2))
                       (add-lists (cdr list1) (cdr list2)))))))

(define ONES
  '(define ones (cons 1 ones)))

(define INTEGERS
  '(define integers (cons 1 (add-lists ones integers))))

(define INTEGRAL
  '(define (integral integrand initial-value dt)
     (define int
       (cons initial-value
             (add-lists (scale-list integrand dt)
                        int)))
     int))

(define SOLVE
  '(define (solve f y0 dt)
     (define y (integral dy y0 dt))
     (define dy (map f y))
     y))

(module+ test
  (define (test-lazy-eval e) (actual-value e the-global-environment))
  (map
   test-lazy-eval
   (list CONS CAR CDR
         LIST-REF MAP SCALE-LIST ADD-LISTS
         ONES INTEGERS
         INTEGRAL SOLVE))
  (test-lazy-eval '(list-ref integers 17))
  (test-lazy-eval '(list-ref (solve (lambda (x) x) 1 .0005) 2000))
  (test-lazy-eval '(car (cons 'a (cons 'b 'c))))
  (test-lazy-eval '(car ones))
  (test-lazy-eval '(car (cdr ones)))
  (test-lazy-eval '(print-environment 'env))
  'done)

(module+ export
  (provide actual-value
           the-global-environment
           CONS CAR CDR
           ONES INTEGERS
           ADD-LISTS SCALE-LIST MAP))

