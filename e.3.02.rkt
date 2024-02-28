#lang racket

(define 1+ add1)

(define (make-monitored f)
  (let ((counter 0))
    (define (how-many-calls)
      counter)
    (define (reset-count)
      (set! counter 0))
    (define (dispatch msg)
      (cond ((eq? msg 'how-many-calls?) (how-many-calls))
	    ((eq? msg 'reset-count) (reset-count))
	    (else (begin (set! counter (1+ counter))
			 (f msg)))))
    dispatch))

(module+ test

  (define t (make-monitored 1+))
  (t 100)
  (t 'how-many-calls?)

  "--"
  (define s (make-monitored sqrt))
  (s 100)
  (s 'how-many-calls?)
  (s 100)
  (s 'how-many-calls?)
  (s 'reset-count)
  (s 100)
  (s 'how-many-calls?)
  )

(module+ export
  (provide 1+))




