#lang racket

(define make-vect
  (lambda (x y)
    (lambda (message)
      (cond ((eq? message 'x) x)
            ((eq? message 'y) y)
            (else false)))))

(define xcor-vect (lambda (v) (v 'x)))
(define ycor-vect (lambda (v) (v 'y)))

(define add-vect
  (lambda (v w)
    (make-vect (+ (xcor-vect v) (xcor-vect w))
                 (+ (ycor-vect v) (ycor-vect w)))))

(define sub-vect
  (lambda (v w)
    (make-vect (- (xcor-vect v) (xcor-vect w))
                 (- (ycor-vect v) (ycor-vect w)))))

(define scale-vect
  (lambda (c v)
    (make-vect (* c (xcor-vect v))
                 (* c (ycor-vect v)))))

(module+ test

  (define v (make-vect 1 2))
  (define w (make-vect 3 4))

  (xcor-vect v)
  (ycor-vect v)

  (xcor-vect (add-vect v w))
  (ycor-vect (add-vect v w))

  (xcor-vect (sub-vect v w))
  (ycor-vect (sub-vect v w))

  (xcor-vect (scale-vect 3 w))
  (ycor-vect (scale-vect 3 w)))

(module+ export
  (provide add-vect
           sub-vect
           scale-vect
           xcor-vect
           ycor-vect
           make-vect))
