#lang racket

(require "sicp.rkt")
(GETMOD 4 35)

(module+ test
  "===================="
  (amb-test
   '((define (an-integer-between a b)
       (if (<= a b)
           (amb a (an-integer-between (+ 1 a) b))
           (amb)))
     (define (an-integer-starting-from2 a)
       (define (iter n m)
         (if (<= n m)
             (amb (cons n m) (iter (+ n 1) (- m 1)))
             (an-integer-starting-from2 (+ 1 a))))
       (iter 1 (- a 1)))
     (define (a-pythagorean-triple-starting-from low)
       (let ((ij (an-integer-starting-from2 low)))
         (let ((i (car ij))
               (j (cdr ij)))
           (let ((k (an-integer-between (+ 1 j) (+ 1 (* i i) (* j j)))))
             (require (= (+ (* i i) (* j j)) (* k k)))
             (list i j k)))))))
  
  (amb-test '((a-pythagorean-triple-starting-from 10)
              try-again
              try-again
              try-again
              try-again
              try-again
              try-again
              try-again
              try-again
              try-again
              try-again
              try-again
              try-again
              try-again
              try-again
              try-again
              try-again
              try-again
              try-again
              try-again
              try-again
              try-again
              try-again
              try-again
              try-again
              try-again
              try-again
              try-again
              try-again
              try-again
              try-again
              try-again
              try-again
              try-again
              try-again
              try-again
              try-again
              try-again
              try-again
              try-again
              try-again
              try-again
              try-again
              try-again
              ))
  'done)


