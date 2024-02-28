#lang racket

(require "sicp.rkt")

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

(infinite-loop
 (lambda ()
   (test 1 (p)))
 "infinite loop -- normal order evaluation"
 .2
 "looping")

(infinite-loop
 (lambda ()
   (test 0 (p)))
 "0 -- normal order evaluation"
 .2
 "looping")
