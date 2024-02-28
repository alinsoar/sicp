#lang racket

(require "sicp.rkt")

(GETMOD 4 55)

(module+ test
  (initialize-data-base
   '( (rule (?x next-to ?y in (?x ?y . ?u)))
      (rule (?x next-to ?y in (?v . ?z))
            (?x next-to ?y in ?z))
      (same ?a ?a)))
  (test/ '((COMM "NEXT-TO")
           (?x next-to ?y in (1 (2 3) 4))
           (?x next-to ?y in (2 1 3 1))

           (COMM "SAME")
           (same ?a ?b)
           ))
  'done)


