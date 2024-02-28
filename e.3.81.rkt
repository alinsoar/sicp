#lang racket

(require "sicp.rkt")
(GETMOD 3 50)
(GETMOD 3 6)

(define random-init (rand 'get))
(define rand-update
  (lambda (op)
    (rand op)
    (rand 'get)))

(define (u-stream)
  (define update (cons-stream 'generate update))
  (cons-stream 'reset update))

(define update-stream (u-stream))

(define random-numbers
  (stream-map rand-update update-stream))

(define (map-successive-pairs f s)
  (cons-stream
   (f (stream-car s) (stream-car (stream-cdr s)))
   (map-successive-pairs f (stream-cdr (stream-cdr s)))))

(define cesaro-stream
  (map-successive-pairs (lambda (r1 r2) (= (gcd r1 r2) 1))
                        random-numbers))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define pi
  (stream-map (lambda (p) (sqrt (/ 6 p)))
              (monte-carlo cesaro-stream 0 0)))

(module+ test
  "Cesaro Stream"
  (print-row-n cesaro-stream 30)
  "update stream"
  (print-row-n update-stream 30)
  "random numbers stream"
  (print-row-n random-numbers 30)
  "the bias in PI is due to the improvised random number generator"
  (stream-ref pi 3000)

  'done)

