#lang racket

(require "sicp.rkt")
(GETMOD 3 50)

(module+ test

  (define test
    (lambda (active-delay?)
      (set-active-memo-proc! active-delay?)
      (define sum 0)

      (define (accum x)
        (set! sum (+ x sum))
        sum)

      (define seq (stream-map accum (stream-enumerate-interval 1 20)))
      (define y (stream-filter even? seq))
      (define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                               seq))

      (d "--" sum)
      (d "--")
      (d "REF:" (stream-ref y 7))
      (d "-- Z ;" (display-stream-row z))
      (d "-- SEQ ;" (display-stream-row seq))
      (d "-- Y ;" (display-stream-row y))
      (d "--")
      (d sum)
      'done))

  (test true)
  "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% REF is different without memoization"
  (test false)
  
  'done)

