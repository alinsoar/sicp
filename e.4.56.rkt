#lang racket

(require "sicp.rkt")

(GETMOD 4 55)

(module+ test
  (initialize-data-base microshaft-data-base)

  (test/ '((and
            (supervisor ?x (Bitdiddle Ben))
            (address ?x . ?addr))
           
           (and
            (salary (Bitdiddle Ben) ?sal-ben)
            (salary ?x ?amount)
            (lisp-value < ?amount ?sal-ben))

           (and
            (supervisor ?x ?y)
            (not (job ?y (computer . ?w)))
            (job ?y ?j))))
  
  'done)




