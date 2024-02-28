#lang racket

(require (submod "e.2.07.rkt" export))
(require (submod "e.2.08.rkt" export))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(module+ test
  (define (test-interval-width op x y)
    (display
     (format "~a\t~a\t~a\t~a\t~a\t~a\n"
             (f (width (op x y)) 5 1)
             (f (width x) 5 1)
             (f (width y) 10 6)
             (str-interval x 5 3)
             (str-interval y 5 3)
             (str-interval (op x y) 5 3))))

  ;; width of sum is sum of widths
  (test-interval-width
   add-interval
   (make-interval -1. 2.)
   (make-interval -1. 2.))

  ;; width of difference is sum of widths
  (test-interval-width
   sub-interval
   (make-interval -1 2)
   (make-interval -1 2))

  ;; the width of the multiplied interval is constant all the time,
  ;; however the second interval varies.
  (define (t-mul i e)
    (if (<= i 0)
        'ok
        (begin
          (test-interval-width
           mul-interval
           (make-interval -1. 2.)
           (make-interval i 2.))
          (t-mul (- i e) e))))

  (t-mul 1 .001)

  ;; the width of the interval x/y remains constant in time, however
  ;; the denominator y varies.
  (define (t-div i e)
    (if (<= i 0)
        'ok
        (begin
          (test-interval-width
           div-interval
           (make-interval 0. 1.)
           (make-interval 1 (+ 1 i)))
          (t-div (- i e) e))))

  (t-div 1 .001))

