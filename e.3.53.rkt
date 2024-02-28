#lang racket

(require "sicp.rkt")
(GETMOD 3 50)

(module+ test
  (define s (cons-stream 1 (add-streams s s)))
  (d s)
  (d (stream-cdr s))
  (d (stream-cdr (stream-cdr s)))
  (d (stream-cdr (stream-cdr (stream-cdr s))))
  (d (stream-cdr (stream-cdr (stream-cdr (stream-cdr s)))))
  (d (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr s))))))
  (print-row-n s 10)
  'done)

