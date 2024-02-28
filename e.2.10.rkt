#lang racket

(require (except-in (submod "e.2.07.rkt" export)
                    div-interval))
(require (submod "e.2.08.rkt" export))

(define (div-interval x y)
  (if (positive? (* (upper-bound y) (lower-bound y)))
      (mul-interval x 
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))
      (error "dividing by interval spanning 0")))

(module+ test
  (require rackunit)
  (test-case "division with interval containing 0"
   (check-exn exn:fail?
              (lambda ()
               (div-interval (make-interval 0 1)
                             (make-interval -1 2))))
   'ok)

  (div-interval (make-interval 0 1)
                (make-interval 1 2)))
