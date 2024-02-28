#lang racket

(require "sicp.rkt")
(GETMOD 3 50 without add-streams stream-map)

(define (add-streams s1 s2)
  (d (stream-car s1) "+" (stream-car s2))
  (if (stream-null? s1)
      the-empty-stream
      (cons-stream (+ (stream-car s1)
                      (stream-car s2))
                   (add-streams (stream-cdr s1)
                                (stream-cdr s2)))))

(module+ test
  (define (test active-memo-proc?)
    (set-active-memo-proc! active-memo-proc?)
    (define fibs (cons-stream 0
                              (cons-stream 1
                                           (add-streams (stream-cdr fibs)
                                                        fibs))))
    (print-row-n fibs 6))
  (test true)
  "**************"
  (test false)
  'done)

