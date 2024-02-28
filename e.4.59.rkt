#lang racket

(require "sicp.rkt")

(GETMOD 4 55)

(module+ test
  (initialize-data-base microshaft-data-base)


  (test/ '((assert! (meeting accounting (Monday 9am)))
           (assert! (meeting administration (Monday 10am)))
           (assert! (meeting computer (Wednesday 3pm)))
           (assert! (meeting administration (Friday 1pm)))
           (assert! (meeting whole-company (Wednesday 4pm)))

           (meeting ?w ?t)

           (meeting ?w (Friday ?t))

           (assert!
            (rule (meeting-time ?person ?day-and-time)
                  (or (meeting whole-company ?day-and-time)
                      (and (job ?person (?division . ?_))
                           (meeting ?division ?day-and-time)))))

           (meeting-time (Hacker Alyssa P) (Wednesday 4pm))
           (meeting-time (Hacker Alyssa P) (Wednesday 3pm))
           (meeting-time (Hacker Alyssa P) (Wednesday 2am))
           (meeting-time (Hacker Alyssa P) (Wednesday ?_))

           ;; (job ?x ?j)
           ))
         
  'done)


