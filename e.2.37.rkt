#lang racket

(require (submod "e.2.33.rkt" export))
(require (submod "e.2.36.rkt" export))

(define (dot-product v w)
  (accumulate + 0 (accumulate-n * 1 (list v w))))

(define (constant-*-vector c v)
  (map (lambda (x) (* x c)) v))

(define (add-vectors v w)
  (accumulate-n + 0 (list v w)))

(define (matrix-*-vector0 m v)
  (map (lambda (x) (dot-product x v)) m))

(define (transpose m)
  (accumulate-n cons '() m))

(define (matrix-*-vector1 m v)
  (accumulate-n +
                0
                (map (lambda (x) (constant-*-vector (car x) (cadr x)))
                     (accumulate-n cons '() (list v (transpose m))))))

(module+ test

  (define v '(3 4 3 4))

  (define w '(1 2 3 0))

  (define m '((1 2 3 4)
              (4 5 6 6)
              (6 7 8 9)))

  (add-vectors v w)

  (constant-*-vector 3 v)

  (dot-product v w)

  (transpose m)

  (matrix-*-vector0 m v)
  (matrix-*-vector1 m v))


