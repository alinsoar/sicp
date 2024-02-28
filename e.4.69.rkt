#lang racket

(require "sicp.rkt")
(GETMOD 4 55)
(require "e.4.55.DB.rkt")

(module+ test
  (initialize-data-base genesis-data-base)

  (test/ '((assert! (rule (== ?x ?x)))

           (assert! (rule ((great ?rel . ?rest) ?x ?y)
                          (and (== ?rel grandson)
                               (== () ?rest)
                               (son ?x ?u)
                               (grandson ?u ?y))))

           (assert! (rule ((great ?rel . ?rest) ?x ?y)
                          (and (== ?rel great)
                               (son ?x ?u)
                               ((great . ?rest) ?u ?y))))
           
           (son Adam Irad)
           (son Adam Cain)
           (grandson Cain Irad)
           
           ((great grandson) Adam Irad)
           ((great grandson) Cain Irad)
           ((great great grandson) Cain Methushael)
           ((great great great grandson) Cain Methushael)
           ((great great great grandson) Cain Lamech)

           (?relationship Adam Irad)
           (?relationship Enoch Lamech)
           (?relationship Enoch Jabal)
           (?relationship Adam Jabal)))
  'done)



