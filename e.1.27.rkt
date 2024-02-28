#lang racket


;;; The Caramichael numbers are not prime. However, they fool the
;;; Fermat test, whatever is the chosen value of a -- a^n = a for each
;;; a < n.

(define (expmod base exp m)
  (define square sqr)                   ; sqr is internal to racket
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define caramichael-list
  '(561 1105 1729 2465 2821 6601 8911 10585 15841 29341 41041))

(define (foreach-fermat-test n)
  "test whether a^n = a mod n for each a < n"
  (define (rec k)
    (if (= k 2)
        true
        (and (= (expmod k n n) k)
             (rec (- k 1)))))
  (rec (- n 1)))

(define (test-caramichael l)
  (cond ((null? l) 'ok)
        (else
         (display (format "~a\t~a\n"
                          (car l)
                          (foreach-fermat-test (car l))))
         (test-caramichael (cdr l)))))

(test-caramichael caramichael-list)

;;; http://de.wikibooks.org/wiki/Pseudoprimzahlen:_Tabelle_Carmichael-Zahlen
;;; 561 1105 1729 2465 2821 6601 8911 10585 15841 29341 41041 46657
;;; 52633 62745 63973 75361 101101 115921 126217

;; Note 47 SICP, ch 1: In testing primality of very large numbers
;; chosen at random, the chance of stumbling upon a value that fools
;; the Fermat test is less than the chance that cosmic radiation will
;; cause the computer to make an error in carrying out a ``correct''
;; algorithm. Considering an algorithm to be inadequate for the first
;; reason but not for the second illustrates the difference between
;; mathematics and engineering.

