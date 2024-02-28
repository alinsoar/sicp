#lang racket

(require "sicp.rkt")

(GETMOD 4 55)

(define microshaft-without-outrank
  (filter
   (lambda (x)
     (not (and (eq? (car x) 'rule)
               (eq? (caadr x) 'outranked-by))))
   microshaft-data-base))

(module+ test

  (initialize-data-base
   (append
    microshaft-without-outrank
    '( (rule (outranked-by ?staff-person ?boss)
             (or (supervisor ?staff-person ?boss)
                 ;; REVERSED FIELDS -- MEMOIZED EVALUATOR DOES NOT LOOP
                 (and (outranked-by ?middle-manager ?boss)
                      (supervisor ?staff-person ?middle-manager)))) )))

  (test/memo '((outranked-by (Bitdiddle Ben) ?who) ) )  

  'done)


