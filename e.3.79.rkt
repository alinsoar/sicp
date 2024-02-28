#lang racket

(require "sicp.rkt")
(GETMOD 3 50)
(GETMOD 3 77)

(define (solve-generic-2nd f y0 dy0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)

(module+ test
  (define (test K)
    (d (stream-ref (solve-generic-2nd (lambda (x y) x)
                                      1 1
                                      (/ 1.0 K))
                   K))
    (d (exp 1))
    (d))

  (test 100)
  (test 1000)
  (test 10000)
  (test 100000)
  'done)


