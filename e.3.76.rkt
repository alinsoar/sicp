#lang racket

(require "sicp.rkt")
(GETMOD 3 50)
(GETMOD 1 6)
(GETMOD 3 74)

(define smooth
  (lambda (s)
    (stream-map
     (lambda (p) (apply average p))
     (pair-streams s (stream-cdr s)))))

(define (make-zero-crossings input-stream)
  (let ((avg (smooth input-stream)))
    (stream-map (lambda (p) (apply sign-change-detector p))
                (pair-streams (stream-cdr avg) avg))))

(module+ test

  "--- SIGNALS"
  (print-row-n sense-data 30)
  "--"

  "--- ZERO CROSSINGS"
  (print-row-n (make-zero-crossings sense-data) 30)

  "--- SMOOTH"
  (print-row-n (smooth sense-data) 30)
  
  'done)



