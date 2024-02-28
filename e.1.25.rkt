#lang racket


(require racket/trace)

(define square sqr)                     ; racket provides sqr

;;; it stucks for large base and exp -- out of memory, and/or the time
;;; is large to multiply large numbers.
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

;;; all arithmetics in this version is done with numbers less than `m`.
(define (expmod0 base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod0 base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod0 base (- exp 1) m))
                    m))))

;;; it it correct, because (a*b) (mod p) = a(mod p)*b(mod p)
(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

(trace fast-expt)

(trace expmod0)

;;; compute 1 remainder of a large number
(expmod 1001 101 10)

;;; compute many remainders of small numbers
(expmod0 1001 101 10)

(fast-prime? 100000019 1)


