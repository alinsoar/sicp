#lang racket

(require "sicp.rkt")
(GETMOD 4 32)

(module+ test
  (define (test-lazy-eval e) (actual-value e the-global-environment))
  (map
   test-lazy-eval
   (list CONS CAR CDR))
  
  (with-handlers (((lambda (v) (exn? v)) (lambda (v) (exn-message v))))
    (test-lazy-eval '(car '(a b c))))
  
  (test-lazy-eval '(car (cons 'a (cons 'b 'c))))
  'done)



