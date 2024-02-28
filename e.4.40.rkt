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
     (define counter 0)
     (define (multiple-dwelling)
       (let ((baker (amb 1 2 3 4 5))
             (cooper (amb 1 2 3 4 5))
             (fletcher (amb 1 2 3 4 5))
             (miller (amb 1 2 3 4 5))
             (smith (amb 1 2 3 4 5)))
         (p-set! counter (+ 1 counter))
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
               (list 'smith smith)
               counter)))
     (define counter1 0)
     (define counter2 0)
     (define counter3 0)
     (define counter4 0)
     (define counter5 0)
     (define (multiple-dwelling-much-better)
       (let ((baker (amb 1 2 3 4 5)))
         (p-set! counter1 (+ 1 counter1)) ; K1
         (require (not (= baker 5)))
         (let ((cooper (amb 1 2 3 4 5)))
           (p-set! counter2 (+ 1 counter2)) ; K2
           (require (not (= cooper 1)))
           (let ((fletcher (amb 1 2 3 4 5)))
             (p-set! counter3 (+ 1 counter3)) ; K3
             (require (not (= (abs (- fletcher cooper)) 1)))
             (require (not (= fletcher 5)))
             (require (not (= fletcher 1)))
             (let ((miller (amb 1 2 3 4 5)))
               (p-set! counter4 (+ 1 counter4)) ; K4
               (require (> miller cooper))
               (let ((smith (amb 1 2 3 4 5)))
                 (p-set! counter5 (+ 1 counter5)) ;K5
                 (require (distinct? (list baker cooper fletcher miller smith)))
                 (require (not (= (abs (- smith fletcher)) 1)))
                 (list (list 'baker baker)
                       (list 'cooper cooper)
                       (list 'fletcher fletcher)
                       (list 'miller miller)
                       (list 'smith smith)
                       counter1
                       counter2
                       counter3
                       counter4
                       counter5
                       "SUM:"
                       (+ counter1 counter2 counter3 counter4 counter5))))))))))
  (amb-test '((multiple-dwelling)
              try-again
              try-again
              (multiple-dwelling-much-better)

              ))
  'done)


