#lang racket

(require "sicp.rkt")
(GETMOD 3 50)

(define sign-change-detector
  (lambda (current prev)
    (define (sign x) (if (>= x 0) 1 -1))
    (cond ((or (eq? current 'unknown)
               (eq? prev 'unknown)) '0)
          ((= (sign prev) (sign current)) 0)
          (else (sign current)))))

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

(define (make-zero-crossings input-stream last-value)
  (cons-stream
   (sign-change-detector (stream-car input-stream) last-value)
   (make-zero-crossings (stream-cdr input-stream)
                        (stream-car input-stream))))

(define (make-sense-data)
  (begin
    (define (reduce op init l)
      (if (null? l)
          init
          (op (car l) (reduce op init (cdr l)))))

    (define unknown-stream (cons-stream 0 unknown-stream))
    
    (define S
      (reduce (lambda (x y) (cons-stream x y))
              unknown-stream
              (list 1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4)))

    S))

(define sense-data (make-sense-data))

(define zero-crossings
  (stream-map
   sign-change-detector
   sense-data
   (stream-cdr sense-data)))

(module+ test

  "--- SIGNALS"
  (print-row-n sense-data 30)

  "--- ZERO CROSSING VALUES"
  (print-row-n (make-zero-crossings sense-data 0) 30)
  "--- ZERO CROSSINGS"
  (print-row-n zero-crossings 30)
  'done)

(module+ export
  (provide sense-data
           zero-crossings
           sign-change-detector))
