#lang racket

(require "sicp.rkt")
(GETMOD 4 35)

(module+ test
  "===================="
  (amb-test
   '((define count 0)
     (define (an-element-of items)
       (require (not (null? items)))
       (amb (car items) (an-element-of (cdr items))))
     ))


  
  (amb-test '(
              (define (prime-sum-pair list1 list2)
                (let ((a (an-element-of list1))
                      (b (an-element-of list2)))
                  (require (prime? (+ a b)))
                  (list a b)))

              (define (an-element-of items)
                (require (not (null? items)))
                (amb (car items) (an-element-of (cdr items))))
              ))

  
  (amb-test '(
              (let ((pairs '()))
                (if-fail (let ((p (prime-sum-pair '(1 3 5 8) '(20 35 110))))
                           (p-set! pairs (cons p pairs))
                           (amb))
                         pairs))))
  'done)


