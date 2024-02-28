#lang racket

(require "sicp.rkt")
(GETMOD 3 50)
(GETMOD 3 66)

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< (weight s1car) (weight s2car))
                  (cons-stream s1car (merge-weighted (stream-cdr s1)
                                                     s2
                                                     weight)))
                 ((> (weight s1car) (weight s2car))
                  (cons-stream s2car (merge-weighted s1
                                                     (stream-cdr s2)
                                                     weight)))
                 (else
                  (cons-stream s1car
                               (cons-stream s2car
                                            (merge-weighted (stream-cdr s1)
                                                            (stream-cdr s2)
                                                            weight)))))))))

(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
    weight)))

(module+ test
  (d "--")

  (define (filter-not-235 s)
    (stream-filter (lambda (x)
                     (not (or (= (remainder x 2) 0)
                              (= (remainder x 3) 0)
                              (= (remainder x 5) 0))))
                   s))
  
  (define S0 (weighted-pairs integers
                            integers
                            (lambda (p) (+ (car p) (cadr p)))))
  (define S1 (weighted-pairs (filter-not-235 integers)
                             (filter-not-235 integers)
                             (lambda (p)
                               (+ (* (car p) 2)
                                  (* (cadr p) 3)
                                  (* (cadr p) (car p) 5)))))
  (d "--")
  (print-row-n S0 100)
  (d "--")
  (print-row-n S1 100)
  'done)

(module+ export
  (provide weighted-pairs))
