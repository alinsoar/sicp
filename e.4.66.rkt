#lang racket

(require "sicp.rkt")

(GETMOD 4 55)

(module+ test

  (initialize-data-base microshaft-data-base)

  (test/ '(

           (assert! (rule (sum ?var ?pat)))

           (COMM "total salary for computer programmers")
           (and (job ?x (computer programmer))
                (salary ?x ?amount) )

           (accumulation +
                         0
                         ?amount
                         (and (job ?x (computer programmer) )
                              (salary ?x ?amount) ) )

           (COMM "total salary for wheels")
           
           (and (wheel ?who)
                (salary ?who ?amount))
           
           (accumulation +
                         0
                         ?amount
                         (and (wheel ?who)
                              (salary ?who ?amount)))

           (COMM "total salary for wheels, no duplicates.")
           
           (accumulation
            (lambda (acc x)
              (list 'quote
                    (if (eq? false (member (cdr x) (cdr acc)))
                        (cons (+ (car acc) (car x))
                              (cons (cdr x)
                                    (cdr acc)))
                        acc)))
            '(0)
            '(?amount . ?who)
            (and (wheel ?who)
                 (salary ?who ?amount) ) ) ) )
  'done)


