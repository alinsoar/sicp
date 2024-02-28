#lang racket

(require "sicp.rkt")
(GETMOD 4 1)

(define (halts? p a)
  (if (p a)
      'halted
      'not-halted))

(define (run-forever) (run-forever))

(define (try p)
  (if (halts? p p)
      (run-forever)
      'halted))

(module+ test
  (require rackunit)
  (require racket/sandbox)
  
  (define (test-eval e) (eval e the-global-environment))

  (d (with-handlers (((lambda (v) (exn? v))
                      (lambda (v)
                        (string-append "run-forever: never stops -- "
                                       (exn-message v))))
                     ((lambda (_) true) (lambda (_) (error "SHOULD NOT HAPPEN"))))
       (call-with-limits .3 1
                         (lambda () (run-forever)))))
  
  (d "--- THE DEFINITION OF THE FUNCTION ``HALTS?`` IS A WEEL-POSED TAKS"
     "THAT CANNOT BE COMPUTED.")
  (d (with-handlers (((lambda (v) (exn? v))
                      (lambda (v)
                        (string-append "halt: never halts -- "
                                       (exn-message v))))
                     ((lambda (_) true) (lambda (_) (error "SHOULD NOT HAPPEN"))))
       
       (call-with-limits 1 10
                         (lambda ()
                           (try try)))))
  
  'done)

