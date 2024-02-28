#lang racket

(require "sicp.rkt")
(GETMOD 3 33)

(define (squarer a b)
  (multiplier a a b))

(module+ test
  (define a (make-connector 'a))
  (define b (make-connector 'b))
  (void (probe a)
        (probe b))
  (squarer a b)
  (set-value! a 10 'user)
  (forget-value! a 'user)
  (set-value! a 20 'user)
  (forget-value! a 'user)
  ;; the change of b does not propagate towards a
  (set-value! b 400 'user)
  )

