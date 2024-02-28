#lang racket

(require "sicp.rkt")

(GETMOD 4 55)

(module+ test
  (initialize-data-base microshaft-data-base)

  (test/ '((assert!
            (rule (big-shot? ?x)
                  (and (job ?x (?dx . ?_))
                       (or (not (supervisor ?x ?y))
                           (and
                            (supervisor ?x ?y)
                            (job ?y (?dy . ?__))
                            (not (same ?dx ?dy))
                            )))))

           (big-shot? ?x)

           (and (job ?x (?dx . ?_))
                (supervisor ?x ?y)
                (job ?y (?dy . ?__))
                (not (same ?dx ?dy)))

           (and (big-shot? ?x)
                (not (supervisor ?x ?y)))

           
           (job ?x ?j)
           (supervisor ?x ?s)))
  
  'done)

