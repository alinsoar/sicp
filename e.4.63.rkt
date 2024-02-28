#lang racket

(require "sicp.rkt")

(GETMOD 4 55)
(require "e.4.55.DB.rkt")

(module+ test
  (initialize-data-base genesis-data-base)

  (test/ '((grandson ?x ?y)

           (grandson Cain ?y)
           (son Lamech ?y)
           (grandson Methushael ?y)

           (son ?x ?y)
           (and (wife ?M ?W)
                (son  ?W ?S))))
  'done)



