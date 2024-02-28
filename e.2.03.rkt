#lang racket
(require (submod "e.2.02.rkt" export))

(define-signature encoding^ (make-rectangle
                             rectangle-left-down
                             rectangle-right-top
                             rectangle-length
                             rectangle-height) )

(define-signature abstraction^ ( print-area-perim-rect ) )

(define-unit abstraction@
  (import encoding^)
  (export abstraction^)
  ;; functions used by all encodings of rectangles
  (define (perimeter-rectangle r)
    (* 2 (+ (rectangle-length r)
            (rectangle-height r))))
  (define (area-rectangle r)
    (* (rectangle-length r)
       (rectangle-height r)))
  (define (print-rectangle r)
    (print-point (rectangle-left-down r) #false)
    (display ":")
    (print-point (rectangle-right-top r) #true))
  (define (print-area-perim-rect r)
    (print-rectangle r)
    (printf (format "\t=> perimeter=~a  area=~a"
                    (perimeter-rectangle r)
                    (area-rectangle r)))))

(define-unit first-encoding@
  (import)
  (export encoding^)
  ;; 2nd encoding -- (p length height)
  (define (make-rectangle p l h) (cons p (cons l h)))
  (define (rectangle-left-down r) (car r))
  (define (rectangle-right-top r) (make-point (+ (x-point (car r)) (cadr r))
                                              (+ (y-point (car r)) (cddr r))))
  (define (rectangle-length r) (cadr r))
  (define (rectangle-height r) (cddr r)))

(define-unit second-encoding@
  (import)
  (export encoding^)
  ;; 1st encoding -- (p1 p2)
  (define (make-rectangle p1 p2) (cons p1 p2))
  (define (rectangle-left-down r) (car r))
  (define (rectangle-right-top r) (cdr r))
  (define (rectangle-length r) (- (x-point (cdr r)) (x-point (car r))))
  (define (rectangle-height r) (- (y-point (cdr r)) (y-point (car r)))))

(module+ test
  (define p1 (make-point -2 -2))
  (define p2 (make-point 4 4))
  (define (test1)
    (define-values/invoke-unit/infer first-encoding@)
    (define-values/invoke-unit/infer abstraction@)
    (define r1 (make-rectangle p1
                               (- (x-point p2) (x-point p1))
                               (- (y-point p2) (y-point p1))))
    (print-area-perim-rect r1))
  (define (test2)
    (define-values/invoke-unit/infer second-encoding@)
    (define-values/invoke-unit/infer abstraction@)
    (define r1 (make-rectangle p1 p2))
    (print-area-perim-rect r1))
  (test1)
  (test2))

