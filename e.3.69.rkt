#lang racket

(require "sicp.rkt")
(GETMOD 3 50)
(GETMOD 3 66)
(GETMOD 1 6)

(define (interleave3 a b c) (interleave a (interleave b c)))

(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-map (lambda (p) (list (stream-car s) (car p) (cadr p)))
                (pairs (stream-cdr t)
                       (stream-cdr u)))
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define pytagorean-triples
  (stream-filter (lambda (p)
                   (= (+ (square (car p))
                         (square (cadr p)))
                      (square (caddr p))))
                (triples integers integers integers)))

(module+ test
  (define (test s t u)
    (print-row-n (triples s t u) 100)
    "---")
  (test integers integers integers)
  (print-row-n pytagorean-triples 4)
  'done)

