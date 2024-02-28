#lang racket

(define (sum-of-squares a b c)
  (if (and (<= a b) (<= a c) ) (+ (* b b) (* c c))
      (if (and (<= b a) (<= b c) ) (+ (* a a) (* c c))
          (+ (* a a) (* b b)))))

(sum-of-squares 1 0 4)
