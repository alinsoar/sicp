#lang racket

(require "sicp.rkt")
(GETMOD 3 50)
(GETMOD 3 6)

;; (define random-init (rand 'get))
;; (define rand-update
;;   (lambda (op)
;;     (rand op)
;;     (rand 'get)))

;; (define (u-stream)
;;   (define update (cons-stream 'generate update))
;;   (cons-stream 'reset update))

;; (define update-stream (u-stream))

;; (define random-numbers
;;   (stream-map rand-update update-stream))

;; (define (map-successive-pairs f s)
;;   (cons-stream
;;    (f (stream-car s) (stream-car (stream-cdr s)))
;;    (map-successive-pairs f (stream-cdr (stream-cdr s)))))

;; ;; same as in section 3.1.3
;; (define (make-simplified-withdraw balance)
;;   (lambda (amount)
;;     (set! balance (- balance amount))
;;     balance))

;; (define (stream-withdraw balance amount-stream)
;;   (cons-stream
;;    balance
;;    (stream-withdraw (- balance (stream-car amount-stream))
;;                     (stream-cdr amount-stream))))

(module+ test
  ;; "Cesaro Stream"
  ;; (print-row-n cesaro-stream 30)
  ;; "update stream"
  ;; (print-row-n update-stream 30)
  ;; "random numbers stream"
  ;; (print-row-n random-numbers 30)
  ;; "the bias in PI is due to the improvised random number generator"
  ;; (stream-ref pi 3000)

  'TODO
  
  'done)

