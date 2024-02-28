#lang racket

(require "sicp.rkt")
(GETMOD 4 1)

(module+ test
  (require rackunit)
  (define MAP
    (eval '(begin (define (Map f l)
                    (cond ((null? l) '())
                          (else (cons (f (car l))
                                      (Map f (cdr l))))))
                  Map)
          the-global-environment))
  (define map-extended-environment
    (extend-environment '(map Map)
                        (list (list 'primitive map)
                              MAP)
                        the-global-environment))

  (print-environment map-extended-environment)
  
  (define (test-eval e) (eval e map-extended-environment))

  (test-case
   "system application with implemented language data structure"
   (check-exn
    exn:fail?
    (lambda ()
      (test-eval '(map (lambda (x) (+ x 1)) '(1 2 3)))
      'no-error-caught!))
   "ERROR == SYSTEM APPLY called with 'compound-procedure structure")

  (test-eval '(Map (lambda (x) (+ x 1)) '(1 2 3)))
  
  'done)
