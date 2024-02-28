#lang racket

(require "sicp.rkt")
(GETMOD 3 33)

(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (format "Error! square less than 0 -- SQUARER"
                    (get-value b))
            (set-value! a
                        (sqrt (get-value b))
                        me))
        (if (has-value? a)
            (set-value! b
                        (* (get-value a) (get-value a))
                        me)
            'ignore)))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value)
    )
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          ((eq? request 'type)
           (format "SQUARER ~a ~a"
                   (a 'get-name)
                   (b 'get-name)))
          (else
           (error "Unknown request -- SQUARER" request))))
  (connect a me)
  (connect b me)
  me)

(module+ test
  (define a (make-connector 'input))
  (define b (make-connector 'output))
  (void (probe a)
        (probe b))
  (squarer a b)
  (define (print-status-and-forget x)
    (print-connector a)
    (print-connector b)
    (forget-value! x 'user)
    (print-connector a)
    (print-connector b)
    (d "--"))
  (set-value! a 10 'user)
  (print-status-and-forget a)
  (set-value! a 20 'user)
  (print-status-and-forget b)
  (set-value! b 100 'user)
  (print-status-and-forget a)
  (set-value! b 100 'user)
  (print-status-and-forget b))



