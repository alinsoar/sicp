#lang racket

(require "sicp.rkt")
(GETMOD 3 50)
(GETMOD 3 74)

(define (make-zero-crossings input-stream last-value last-average)
  (let ((current (stream-car input-stream)))
    (let ((avpt (/ (+ current last-value) 2)))
      (cons-stream (sign-change-detector avpt last-average)
                   (make-zero-crossings (stream-cdr input-stream)
                                        current
                                        avpt)))))

(module+ test

  "--- SIGNALS"
  (print-row-n sense-data 30)
  "--"
  (print-row-n zero-crossings 30)

  "--- ZERO CROSSING VALUES"
  (print-row-n (make-zero-crossings sense-data 0 0) 30)

  'done)



