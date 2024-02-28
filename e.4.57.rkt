#lang racket

(require "sicp.rkt")

(GETMOD 4 55)

(module+ test
  (initialize-data-base microshaft-data-base)

  (test/ '((assert! (rule (replace ?x ?y)
                          (and (job ?x ?jx)
                               (job ?y ?jy)
                               (or (same ?jx ?jy)
                                   (can-do-job ?jx ?jy))
                               (not (same ?x ?y)))))

           (replace ?x ?y)
           (replace (Fect Cy D) ?y)
           (replace ?y (Fect Cy D))

           (and (replace ?x ?y)
                (salary ?x ?sx)
                (salary ?y ?sy)
                (lisp-value < ?sx ?sy))

           (and (salary ?x ?sx)
                (salary ?y ?sy)
                (replace ?x ?y)
                (lisp-value < ?sx ?sy))

           (COMM "----")

           (job ?x ?j)
           (can-do-job ?j1 ?j2)))

  'done)
