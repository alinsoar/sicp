#lang racket

(require "sicp.rkt")
(GETMOD 3 50)

(define factorials (cons-stream 1
                                (mul-streams (integers-starting-from 2)
                                             factorials)))

(module+ test
  
  (print-row-n factorials 30)
  
  'done)

