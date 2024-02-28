#lang racket

(require "sicp.rkt")
(GETMOD 4 35)

(module+ test
  "===================="
  (amb-test
   '((define (member? a l)
       (define (iter l)
         (cond ((null? l) false)
               ((= a (car l)) true)
               (else
                (iter (cdr l)))))
       (iter l))
     (define (distinct? l)
       (define (iter l)
         (cond ((null? l) true)
               ((member? (car l) (cdr l)) false)
               (else
                (iter (cdr l)))))
       (iter l))
     (define (multiple-dwelling)
       (let ((baker (amb 1 2 3 4 5))
             (cooper (amb 1 2 3 4 5))
             (fletcher (amb 1 2 3 4 5))
             (miller (amb 1 2 3 4 5))
             (smith (amb 1 2 3 4 5)))
         (require (distinct? (list baker cooper fletcher miller smith)))
         (require (not (= baker 5)))
         (require (not (= cooper 1)))
         (require (not (= fletcher 5)))
         (require (not (= fletcher 1)))
         (require (> miller cooper))
         (require (not (= (abs (- smith fletcher)) 1)))
         (require (not (= (abs (- fletcher cooper)) 1)))
         (list (list 'baker baker)
               (list 'cooper cooper)
               (list 'fletcher fletcher)
               (list 'miller miller)
               (list 'smith smith))))
     (define (multiple-dwelling-modified)
       (let ((baker (amb 1 2 3 4 5))
             (cooper (amb 1 2 3 4 5))
             (fletcher (amb 1 2 3 4 5))
             (miller (amb 1 2 3 4 5))
             (smith (amb 1 2 3 4 5)))
         (require (distinct? (list baker cooper fletcher miller smith)))
         (require (not (= baker 5)))
         (require (not (= cooper 1)))
         (require (not (= fletcher 5)))
         (require (not (= fletcher 1)))
         (require (> miller cooper))
         ;; (require (not (= (abs (- smith fletcher)) 1)))
         (require (not (= (abs (- fletcher cooper)) 1)))
         (list (list 'baker baker)
               (list 'cooper cooper)
               (list 'fletcher fletcher)
               (list 'miller miller)
               (list 'smith smith))))
     ))
  (amb-test '((multiple-dwelling)
              try-again
              'MODIFIED-PUZZLE
              (multiple-dwelling-modified)
              try-again
              try-again
              try-again
              try-again
              try-again
              try-again
              ))
  'done)


