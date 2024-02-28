#lang racket

(define (make-interval a b) (cons a b))
(define (lower-bound z) (min (car z) (cdr z)))
(define (upper-bound z) (max (car z) (cdr z)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (f n k p)
    (~r n
        #:precision p
        #:notation 'positional
        #:pad-string "_"
        #:min-width k))

(define (str-interval z k p)
    (format "[~a ~a]"
            (f (lower-bound z) k p)
            (f (upper-bound z) k p)))

(module+ test
  (define z (make-interval 2 1))
  (str-interval z 3 3))

(module+ export
  (provide make-interval
           lower-bound
           upper-bound
           add-interval
           mul-interval
           div-interval
           f
           str-interval))
