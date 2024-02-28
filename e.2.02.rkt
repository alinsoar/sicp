#lang racket

;;; define Point

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (add-point p1 p2)
  (make-point (+ (x-point p1) (x-point p2))
              (+ (y-point p1) (y-point p2))))

(define (average-point p)
  (make-point (/ (x-point p) 2)
              (/ (y-point p) 2)))

(define (print-point p ln)
  (or ln (newline))
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;;; define Segment

(define (midpoint-segment s)
  (average-point
   (add-point (start-segment s)
              (end-segment s))))

(define (print-segment s)
  (print-point (car s) #false)
  (display ":")
  (print-point (cdr s) #true))

(module+ test
;;; Test

  (define p1 (make-point -1 -1))
  (define p2 (make-point 2 2))
  (define s (make-segment p1 p2))

  (print-segment s)
  (print-point (midpoint-segment s) #false) )

(module+ export
  (provide make-point
           x-point
           y-point
           print-point) )
