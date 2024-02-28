#lang racket

(require (submod "e.2.07.rkt" export))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

;;; width is the same as before
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent center percentage-tolerance)
  (make-interval (- center (/ (* center percentage-tolerance) 100))
                 (+ center (/ (* center percentage-tolerance) 100))))

(define (percent z)
  (/ (* 100 (width z)) (center z)))

(define (str-center-width-interval z k p)
  (format "[~a | ~a ~a]"
          (str-interval z k p)
          (f (center z) k p) (f (width z) k p)))

(define (str-center-percent-interval z k p)
  (format "[~a | ~a ~a]"
          (str-interval z k p)
          (f (center z) k p) (f (percent z) k p)))

(module+ test

  (str-center-percent-interval (make-center-percent 6.8 10) 10 4)

  (str-center-width-interval (make-center-width 6.8 2) 10 3)

  (str-center-width-interval (make-center-width 6.8 2) 1 3)

  (define (test-percent c p)
    (- (percent (make-center-percent c p))
       p))

  (test-percent 200 2.33)

  (test-percent 6.8 10))

(module+ export
  (provide percent
           make-center-percent
           str-center-percent-interval
           center ))
