#lang racket

(require "sicp.rkt")

;; +---------------------------------------------------+
;; |                   -----------             (x2 y2) |
;; |                --/           \--                  |
;; |               /                  \                |
;; |            -/                     \-              |
;; |           /                         \             |
;; |           |                         |             |
;; |          /      compute area of      \            |
;; |          |        this region        |            |
;; |          \                           /            |
;; |           |                         |             |
;; |           \                         /             |
;; |            -\                     /-              |
;; |              -\                 /-                |
;; |                --\           /--                  |
;; | (x1 y1)           -----------        rectangle    |
;; +---------------------------------------------------+

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (+ low (random (- high low))))

(define (estimate-integral predicate x1 y1 x2 y2 trials)
  (let ((area (* (- y2 y1) (- x2 x1) ) )
        (p (monte-carlo trials (predicate x1 y1 x2 y2) )))
    (* p (+ .0 area))) )

(define (inside-circle? x1 y1 x2 y2)
  (define (inside-circle center-x center-y r2)
    (lambda ()
      "each call picks a point inside the rectangle [(x1 y1) (x2 y2)]
and checks whether this point is inside the circle centered at (___)
and radius ___"
      (let ((x (random-in-range x1 x2))
            (y (random-in-range y1 y2)))
        (let ((dx (- x center-x))
              (dy (- y center-y)))
          (<= (+ (* dx dx) (* dy dy)) r2) ) ) ) )
  (let ((radius (/ (min (- x2 x1) (- y2 y1)) 2)))
    (inside-circle (+ x1 radius) (+ y1 radius)
                   (* radius radius) ) ) )

(define estimate-pi
  (lambda (R trials)
    ;; AREA = PI * R^2 ; hence PI = AREA/R^2
    (/ (estimate-integral inside-circle? 0 0 (* R 2) (* R 2) trials)
       (* R R))))

(module+ test
  (estimate-pi 1 1000000)
  (estimate-pi 10 1000000)
  (estimate-pi 100 1000000)
  (estimate-pi 1000 1000000)
  (estimate-pi 10000 1000000)
  (estimate-pi 100000 1000000)
  (estimate-pi 1000000 1000000)
  (estimate-pi 10000000 1000000)
  (estimate-pi 100000000 1000000)
  (estimate-integral inside-circle? 10 10 10000 100 100000))
