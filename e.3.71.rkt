#lang racket

(require "sicp.rkt")
(GETMOD 3 50)
(GETMOD 3 70)

(module+ test
  (d "--")

  (define (3** x) (* x x x))
  (define (f p) (+ (3** (car p)) (3** (cadr p))))
  
  (define S (weighted-pairs integers
                            integers
                            f))

  (define pair-next (pair-streams S
                                  (stream-cdr S)))

  (define ramanujan-numbers
    (stream-map
     (lambda (p) (format "~a = ~a^3+~a^3 = ~a^3+~a^3"
                         (f (car p))
                         (caar p)
                         (cadar p)
                         (caadr p)
                         (cadadr p)))
     (stream-filter
      (lambda (x)
        (= (f (car x))
           (f (cadr x))))
      pair-next)))
  
  (d "--")
  (print-row-n S 100)
  (print-row-n pair-next 100)
  "-- Ramanujan numbers --"
  (print-column-n ramanujan-numbers 50)
  
  'done)

(module+ export
  (provide weighted-pairs))
