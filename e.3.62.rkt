#lang racket

(require "sicp.rkt")
(GETMOD 3 50)
(GETMOD 3 59)
(GETMOD 3 60)
(GETMOD 3 61)

(define (div-series num den)
  (let ((coeff-denom (+ 0.0 (stream-car den))))
    (if (= 0 coeff-denom)
        (error "denom has 0 constant coeff.")
        (mul-series (scale-stream num (/ 1.0 coeff-denom))
                    (invert-unit-series (scale-stream den (/ 1.0 coeff-denom)))))))

(module+ test
  (define (test s1 s2)
    (print-row-n (div-series s1 s2) 20)
    (d)
    "---")
  (test ones ones)
  (test ones integers)
  (test integers ones)
  (test integers integers)
  (test sine-series cosine-series)
  'done)


