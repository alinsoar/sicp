#lang racket

(define (make-accumulator0 initial-count)
  (let ((count initial-count))
    (lambda (x)
      (set! count (+ count x))
      count)))

;; simpler -- not need for the environment of LET

(define (make-accumulator1 count)
  (lambda (x)
    (set! count (+ count x))
    count))

(module+ test
  (define a (make-accumulator0 5))

  (a 10)
  (a 10)
  (a 10)
  
  (define b (make-accumulator1 5))

  (b 10)
  (b 10)
  (b 10)

  )



