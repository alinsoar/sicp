#lang racket

(require "sicp.rkt")

(GETMOD 4 55)

(module+ test
  (initialize-data-base microshaft-data-base)

  (test/ '(
           (job ?x (computer wizard))
           
           (unique (job ?x (computer wizard)))

           (unique (job ?x (computer programmer)))

           (and (job ?x ?j) (unique (job ?anyone ?j)))

           ))
  
  'done)




