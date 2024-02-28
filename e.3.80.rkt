#lang racket

(require "sicp.rkt")
(GETMOD 3 50)
(GETMOD 3 77)

(define (RLC R L C dt)
  (lambda (vc0 il0)
    (define vc (integral (delay dvc) vc0 dt))
    (define il (integral (delay dil) il0 dt))
    (define dvc (scale-stream il (/ -1 C)))
    
    (define dil (add-streams (scale-stream vc (/ 1 L))
                             (scale-stream il (/ (- R) L))))
    (pair-streams vc il)))

(module+ test
  (print-column-n ((RLC 1.0 1.0 0.2 0.1)
                   10.0 0.0)
                  20)
  'done)


