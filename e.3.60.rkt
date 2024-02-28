#lang racket

(require "sicp.rkt")
(GETMOD 3 50)
(GETMOD 3 59)

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1)
                  (stream-car s2))
               (add-streams (scale-stream (stream-cdr s2)
                                          (stream-car s1))
                            (mul-series (stream-cdr s1)
                                        s2))))

(module+ test
  (define (test s1 s2 )
    (print-row-n (mul-series s1 s2) 20)
    "---")
  (test ones ones)
  (print-row-n
   (add-streams (mul-series sine-series sine-series)
                (mul-series cosine-series cosine-series))
   20)
  
  'done)

(module+ export
  (provide mul-series))
