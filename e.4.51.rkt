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
  
  (amb-test '("-------------------- TEST 1 -- PERMANENT SET"
              (let ((x (an-element-of '(a b c)))
                    (y (an-element-of '(a b c))))
                (p-set! count (+ count 1))
                (require (not (eq? x y)))
                (list x y count))
              try-again
              try-again
              try-again
              try-again
              try-again
              try-again
              ))
  (amb-test '("-------------------- TEST 2 -- SET"
              (set! count 0)
              (let ((x (an-element-of '(a b c)))
                    (y (an-element-of '(a b c))))
                (set! count (+ count 1))
                (require (not (eq? x y)))
                (list x y count))
              try-again
              try-again
              try-again
              try-again
              try-again
              try-again
              ))
  'done)


