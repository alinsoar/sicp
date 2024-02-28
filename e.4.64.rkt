#lang racket

(require "sicp.rkt")
(require "e.4.55.S.rkt")
(require racket/sandbox)
(require rackunit)

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
    '( (rule (outranked-byX ?staff-person ?boss)
             (or (supervisor ?staff-person ?boss)
                 ;; INITIAL ORDER -- EVALUATOR DOES NOT LOOPS
                 (and (supervisor ?staff-person ?middle-manager)
                      (outranked-byX ?middle-manager ?boss))))

       (rule (outranked-by ?staff-person ?boss)
             (or (supervisor ?staff-person ?boss)
                 ;; REVERSED FIELDS -- EVALUATOR LOOPS
                 (and (outranked-by ?middle-manager ?boss)
                      (supervisor ?staff-person ?middle-manager)))) )))

  (test/ '((outranked-byX (Bitdiddle Ben) ?who)))

  (infinite-loop (lambda ()
                   (test/ '((outranked-by (Bitdiddle Ben) ?who)))
                   'no-error-caught!)
                 "time expired -- infinite loop"
                 .0001
                 "query")
  'done)

