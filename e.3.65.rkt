#lang racket

(require "sicp.rkt")
(GETMOD 3 50)
(GETMOD 3 55)
(GETMOD 1 6)

(define log-summands
  (lambda ()
    (define (iter n)
      (cons-stream (/ 1.0 n)
                   (stream-map - (iter (+ n 1)))))
    (iter 1)))

(define log-stream (partial-sums (log-summands)))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(module+ test
  (define (test s)
    (print-row-n s 100)
    "---")
  "LOG-STREAM"
  (test log-stream)
  "EULER-TRANSFORM LOG-STREAM"
  (test (euler-transform log-stream))
  "ACCELERATED EULER-TRANSFORM LOG-STREAM"
  (test (accelerated-sequence euler-transform log-stream))
  'done)


