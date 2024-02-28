#lang racket

(require (submod "e.2.07.rkt" export))

(define (negate-interval z)
  (make-interval (- (upper-bound z))
                 (- (lower-bound z))))

(define (sub-interval x y)
  (add-interval x (negate-interval y)))

(module+ test

  (define z0 (make-interval 1 2))
  (sub-interval z0 z0)

  (define z (make-interval 1 1))
  (sub-interval z0 z0)

  (define z1 (make-interval 10 20))
  (define z2 (make-interval 1 2))
  (sub-interval z1 z2)

  (negate-interval (make-interval 1 -2)))

(module+ export
  (provide sub-interval))
