#lang racket

(require (submod "e.2.46.rkt" export))

(define make-segment
  (lambda (start-point end-point)
    (lambda (message)
      (cond ((eq? message 'start) start-point)
            ((eq? message 'end) end-point)
            (else false)))))

(define start-segment (lambda (s) (s 'start)))
(define end-segment (lambda (s) (s 'end)))

(define (segment? s) (and (pair? s)
                          (= (length s) 2)))

(module+ test
  (define s (make-segment (make-vect 2 3)
                          (make-vect 5 4)))

  (xcor-vect (start-segment s))
  (ycor-vect (start-segment s))

  (xcor-vect (end-segment s))
  (ycor-vect (end-segment s)))

(module+ export
  (provide make-segment
           start-segment
           end-segment ))




