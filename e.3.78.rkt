#lang racket

(require "sicp.rkt")
(GETMOD 3 50)
(GETMOD 3 77)

(define (solve-2nd a b dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (add-streams (scale-stream dy a)
                           (scale-stream y b)))
  y)

(module+ test
  (define (test K)
    (d (stream-ref (solve-2nd 0 -1
                              (/ 1.0 K)
                              1 1)
                   K))
    (d (+ (cos 1) (sin 1)))
    (d))

  (test 100)
  (test 1000)
  (test 10000)
  (test 100000)
  'done)

