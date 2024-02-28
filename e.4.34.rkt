#lang racket

(require "sicp.rkt")
(GETMOD 4 1)
(GETMOD 4 27)
(GETMOD 4 32)

(define (cons? o)
  ;; THIS IS A CONS -- A WEAK TEST
  (and (compound-procedure? o)
       (equal? (procedure-body o)
               (procedure-body
                (actual-value '(cons 1 '()) the-global-environment)))))

(define DISPLAY-STREAM
  '(define (display-stream s)
    (define (iter s n)
      (cond ((not (pair? s))       (print s "\n"))
            ((equal? (cdr s) '())  (print (car s) "\n"))
            ((= n 0)               (print ".." "\n"))
            (else                  (print (car s)
                                          (if (pair? (cdr s))
                                              (if (< 1 n)
                                                  ","  ; stream continue
                                                  ".") ; stream finished
                                              "."))    ; cons cell
                                   (iter (cdr s) (- n 1)))))
    (iter s 20)))

(define (display-value output input)
  (cond ((cons? output)
         (actual-value `(display-stream ,input) the-global-environment)
         "")
        (else output)))

(define (*REPL* expr)
  (if (null? expr)
      'done
      (begin 
        (prompt-for-input input-prompt)
        (let ((input (car expr)))
          (o "> " input)
          (let ((output
                 (actual-value input the-global-environment)))
            (announce-output output-prompt)
            (o (display-value output input) "\n")))
        (*REPL* (cdr expr)))))

(module+ test
  (define (test-lazy-eval e) (actual-value e the-global-environment))
  (map
   test-lazy-eval
   (list CONS CAR CDR
         ONES INTEGERS
         ADD-LISTS SCALE-LIST
         DISPLAY-STREAM
         MAP))

  (*REPL* '((+ 1 2 3)
            integers
            ones
            (scale-list integers 3)
            (cons 'a 'b)
            (cons 'a (cons 'b '()))))

  
  'done)



