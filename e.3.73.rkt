#lang racket

(require "sicp.rkt")
(GETMOD 3 50)

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

(define (RC R C dt)
  (lambda (i v0)
    (add-streams (scale-stream i R)
                 (integral (scale-stream i (/ 1 C))
                           v0
                           dt))))

(module+ test
  (define s ((RC 5 1 .5)
             ones
             1))

  "output voltages"
  (print-row-n s 20)
  
  'done)

