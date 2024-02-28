#lang racket

(define (count-pairs l)
  (let ((accumulator '()))
    (define (loop l)
      "l is all the time a pair"
      (or (member l accumulator)
          (begin
            (set! accumulator (cons l accumulator))
            (and (pair? (car l)) (loop (car l)))
            (and (pair? (cdr l)) (loop (cdr l))))))
    (loop l)
    (length accumulator)))

;; THREE pairs
(define a3 (cons 'a 'a))
(define b3 (cons 'b 'b))
(define c3 (cons a3 b3))

;; SEVEN pairs
(define a7 (cons 'a 'a))
(define b7 (cons a7 a7))
(define c7 (cons b7 b7))

;; FOUR pairs
(define a4 (cons 'a 'a))
(define b4 (cons a4 a4))
(define c4 (cons b4 'c))

(module+ test
  c3
  (count-pairs c3)
  c7
  (count-pairs c7)
  c4
  (count-pairs c4)
  '(a b c)
  (count-pairs '(a b c)))

