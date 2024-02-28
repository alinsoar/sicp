#lang racket


;; n and m must be >= 1.
;; n is COLUMN and m is ROW.
;; ROW must be > COLUMN
(define (pascal n m)
  (if (or (= n 1) (= m n))
      1
      (+ (pascal (- n 1) (- m 1))
         (pascal n (- m 1)))))

(pascal 1 1)

(pascal 3 5)
