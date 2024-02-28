#lang racket

(require "sicp.rkt")
(GETMOD 3 39)

(module+ test

  (define default-x-value 10)

  (define (process-test5)
    (define heuristics default-heuristics)
    (define expected-values 5)
    (define process-list
     (make-process-list
      (lambda ()
        (id '_)
        (id (set! x (* (id x) (id x)))))
      (lambda ()
        (id '_)
        (id (set! x (* (id x) (id x) (id x)))))))

    (define id~
      (lambda (m v)
        (let ((DEBUG #f)) (and DEBUG (d "." m x v)))
        ((ID (heuristics m)) v)))
    (define (id v) (id~ '_ v))
    (define x default-x-value)
    (define s (make-serializer))
    (define (get-value) x)
    (define (reset) (set! x default-x-value))

    (make-process-group heuristics expected-values process-list reset get-value))

  (define (process-test6)
    (define heuristics default-heuristics)
    (define expected-values 1)
    (define expr+1
      (lambda ()
        (id '_)
        ((s (lambda () (id (set! x (* (id x) (id x)))))))))
    (define expr**2
      (lambda ()
        (id '_)
        ((s (lambda () (id (set! x (* (id x) (id x) (id x)))))))))


    (define id~
      (lambda (m v)
        (let ((DEBUG #f)) (and DEBUG (d "." m x v)))
        ((ID (heuristics m)) v)))
    (define (id v) (id~ '_ v))
    (define x default-x-value)
    (define s (make-serializer))
    (define (get-value) x)
    (define (reset) (set! x default-x-value))

    (define p1 (make-process expr+1 'proc1))
    (define p2 (make-process expr**2 'proc2))

    (make-process-group heuristics expected-values (list p1 p2) reset get-value))

  "to see how the values are obtained, SET DEBUG ON in id~"
  "---"
  (parallel-execute (process-test5))
  "---"
  (parallel-execute (process-test6))

  'done
  )
