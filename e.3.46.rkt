#lang racket

(require "sicp.rkt")
(GETMOD 3 39 without make-serializer make-mutex)
(require scheme/mpair)
(require compatibility/mlist)

(define heuristics default-heuristics)
(define id~ (lambda (m v) ((ID (heuristics m)) v)))
(define (id v) (id~ '_ v))

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define (make-mutex)
  (let ((cell (mlist false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (and (test-and-set! cell)
                  (the-mutex 'acquire)))
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell)
  (set-mcar! cell false))

(define (test-and-set! cell)
  (if (id (mcar cell))
      true
      (begin (set-mcar! cell true)
             false)))

(module+ test
  (define default-x-value 10)

  (define (process-test)
    (define expected-values 3)
    (define x default-x-value)
    (define s (make-serializer))
    
    (define (reset) (set! x default-x-value))
    (define (get-value) x)

    (define process-list
      (make-process-list
       (lambda () ((s (lambda () (set! x (id (+ (id x) 1)))))))
       (lambda () ((s (lambda () (set! x (id (- (id x) 1)))))))))

    (make-process-group heuristics expected-values process-list reset get-value)
    )

  "SERIALIZER USING TEST-AND-SET! to implement mutex"
  (parallel-execute (process-test))
  'done)

(module+ export
  (provide test-and-set!
           clear!))
