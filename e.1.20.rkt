#lang racket/base

(require (submod "e.2.24.rkt" export))

(define (gcd2 a b)
  (if (zero? b)
      (list (format "=> ~a" a))
      (list (string-append "[" (number->string a)
                           ":" (number->string b)
                           "]" )
            (gcd2 b (remainder a b)))))

(module+ test
  (define exec-tree (gcd2 206 40))
  (tree-view exec-tree))


;;; On Lame's Theorem -- the order of grow of gcd
;;; http://www.fq.math.ca/Scanned/5-2/brown.pdf

