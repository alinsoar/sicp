#lang racket

(require "sicp.rkt")
(GETMOD 3 50)

(define (integral0 delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt)
                                int))))
  int)

(define (solve0 f y0 dt)
  (define y (integral0 (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(define (integral delayed-integrand initial-value dt)
  (cons-stream initial-value
               (if (stream-null? delayed-integrand)
                   the-empty-stream
                   ;; FORCE must appear inside CONS-STREAM, for the DY
                   ;; to be insert in environment in the meantime.
                   (let ((integrand (force delayed-integrand)))
                    (integral (delay (stream-cdr integrand))
                              (+ (* dt (stream-car integrand))
                                 initial-value)
                              dt)))))

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(module+ test
  (define (test K)
    (let ((r0 (stream-ref (solve (lambda (y) y) 1 (/ 1.0 K)) K))
          (r1 (stream-ref (solve0 (lambda (y) y) 1 (/ 1.0 K)) K)))
      (d r0)
      (d r1)
      (d (- (exp 1) r0))
      (d (- (exp 1) r1)))
    (d))

  (test 100)
  (test 1000)
  (test 10000)
  (test 100000)
  (test 1000000)
  
  'done)

(module+ export
  (provide integral))
