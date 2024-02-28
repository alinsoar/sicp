#lang racket

(require "sicp.rkt")

(GETMOD 4 55)

(module+ test

  (initialize-data-base microshaft-data-base)

  (test/ '((wheel ?who)

           (and (same ?person ?person)
                (supervisor ?middle-manager ?person)
                (supervisor ?x ?middle-manager))))

  'done)


