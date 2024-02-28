#lang racket

(require "sicp.rkt")
(GETMOD 3 50)
(GETMOD 3 66 without pairs)

(define (pairs s t)
  (and (> 100 (stream-car s))
       (> 100 (stream-car t))
       (o (stream-car s) "/" (stream-car t) ","))
  ;; INTERLEAVE is a procedure, and does not delay the PAIRS
  ;; parameter, which is also a procedure
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x))
               t)
   (pairs (stream-cdr s) (stream-cdr t))))

(module+ test
  (require rackunit)
  (require racket/sandbox)
  (define (test s t)
    (print-row-n (pairs s t) 20)
    "---")

  (infinite-loop (lambda ()
                   (test integers integers))
                 "infinite loop"
                 .0001
                 "exp")
  'done)

