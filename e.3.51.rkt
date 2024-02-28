#lang racket

(require "sicp.rkt")
(GETMOD 3 50)

(module+ test
  (define (test active-delay?)
    (set-active-memo-proc! active-delay?)
    (define x (stream-map show (stream-enumerate-interval 0 10)))
    (d)
    (d x)
    (d)
    (display-line (stream-ref x 5))
    (d)
    (display-line (stream-ref x 7))
    (d))
  (test true)
  (d "---")
  (test false)
  'done)

