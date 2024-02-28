#lang racket

(require "sicp.rkt")
(GETMOD 4 1)

(module+ test
  (define (test-eval e) (eval e the-global-environment))
  (require rackunit)
  (require racket/sandbox)
  (test-eval '(define (unless condition usual-value exceptional-value)
               (if condition exceptional-value usual-value)))
  (test-eval '(define (factorial n)
               (if (0? (% n 1001)) (print n "..."))
               (unless (= n 1)
                 (* n (factorial (- n 1)))
                 1)))

  "Factorial 5: infinite loop"
  (with-handlers (((lambda (v) (exn? v)) (lambda (v) (exn-message v))))
    (call-with-limits .1 1
                      (lambda ()
                        (test-eval
                         '(factorial 5)))))


  'done)

