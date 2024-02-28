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
              (if-fail (let ((x (an-element-of '(1 3 5))))
                         (require (even? x))
                         x)
                       'all-odd)
              
              (if-fail (let ((x (an-element-of '(1 3 5 8))))
                         (require (even? x))
                         x)
                       'all-odd)
              ))

  'done)


