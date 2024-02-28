#lang racket

(require (submod "e.2.38.rkt" export))

(define (reverse0 sequence)
  (fold-right0 (lambda (x y) (append y (list x)))
               '()
               sequence))

(define (reverse1 sequence)
  (fold-left0 (lambda (x y) (cons y x))
              '()
              sequence))

(define (reverse2 sequence)
  (fold-right1 (lambda (x y) (append y (list x)))
               '()
               sequence))

(define (reverse3 sequence)
  (fold-left1 (lambda (x y) (cons y x))
              '()
              sequence))

(module+ test
  (reverse0 '(1 2 3))
  (reverse1 '(1 2 3))
  (reverse2 '(1 2 3))
  (reverse3 '(1 2 3)))


