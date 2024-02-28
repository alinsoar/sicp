#lang racket

(require "sicp.rkt")
(GETMOD 4 35)

(module+ test
  "===================="
  (amb-test
   '((define (distinct? l)
       (define (iter l)
         (cond ((null? l) true)
               ((member? (car l) (cdr l)) false)
               (else
                (iter (cdr l)))))
       (iter l))
     (define (map f l)
       (if (null? l)
           '()
           (cons (f (car l))
                 (map f (cdr l)))))
     (define (all l)
       (if (null? l)
           true
           (if (car l)
               (all (cdr l))
               false)))
     (define rows
       (lambda ()
         (amb 1 2 3 4 5 6 7 8)))
     (define (check2 q q0 col col0)
       (if (not (= q0 q))
           (not (= (- col0 col) (abs (- q q0))))
           false))
     (define (length l)
       (if (null? l)
           0
           (+ 1
              (length (cdr l)))))
     (define (check q prevs)
       (define col0 (length prevs))
       (define (iter col prevs)
         (if (null? prevs)
             true
             (if (check2 q (car prevs) col col0)
                 (iter (+ 1 col)
                       (cdr prevs)))))
       (iter 0 prevs))
     (define (queens)
       (let ((q1 (rows)))
         (let ((q2 (rows)))
           (require (check q2 (list q1)))
           (let ((q3 (rows)))
             (require (check q3 (list q1 q2)))
             (let ((q4 (rows)))
               (require (check q4 (list q1 q2 q3)))
               (let ((q5 (rows)))
                 (require (check q5 (list q1 q2 q3 q4)))
                 (let ((q6 (rows)))
                   (require (check q6 (list q1 q2 q3 q4 q5)))
                   (let ((q7 (rows)))
                     (require (check q7 (list q1 q2 q3 q4 q5 q6)))
                     (let ((q8 (rows)))
                       (require (check q8 (list q1 q2 q3 q4 q5 q6 q7)))
                       (list q1 q2 q3 q4 q5 q6 q7 q8))))))))))))
  (amb-test '((queens)
              try-again try-again try-again try-again try-again ; 
              try-again try-again try-again try-again try-again ; 10
              try-again try-again try-again try-again try-again ;
              try-again try-again try-again try-again try-again ; 20
              try-again try-again try-again try-again try-again
              try-again try-again try-again try-again try-again ; 30
              try-again try-again try-again try-again try-again
              try-again try-again try-again try-again try-again ; 40
              try-again try-again try-again try-again try-again
              try-again try-again try-again try-again try-again ; 50
              try-again try-again try-again try-again try-again
              try-again try-again try-again try-again try-again ; 60
              try-again try-again try-again try-again try-again
              try-again try-again try-again try-again try-again ; 70
              try-again try-again try-again try-again try-again
              try-again try-again try-again try-again try-again ; 80
              try-again try-again try-again try-again try-again
              try-again try-again try-again try-again try-again ; 90
              try-again
              try-again                 ; 92 solution
              try-again
              ))
  'done)
