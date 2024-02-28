#lang racket


(require racket/trace)

(define runtime current-milliseconds)   ; in racket there are no
                                        ; microseconds, just milli

;;; master method -- each node is split in 2, and there are log(exp)
;;; nodes. 2^log_2(n) = O(n) operations per exponentiation when n is
;;; power of 2. In the case when n is odd, there is also linear in
;;; n=exp.
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

'(trace expmod)

(expmod 2 5 11)

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  ;; the algorithm of the random module in racket constraints the
  ;; programmer to limit the maximum. this might bias the fermat test
  ;; for large numbers.
  (try-it (+ 1 (random (min 4294967087 (- n 1))))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (timed-prime-test n times)
  (display n)
  (start-prime-test n (runtime) times))

(define (start-prime-test n start-time times)
  (and (fast-prime? n times)
       (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (display-fermat-test l times)
  (cond ((null? l) 'ok)
        (else
         (timed-prime-test (car l) times)
         (newline)
         (display-fermat-test (cdr l) times))))

(define primes-list                     ;
  (append
   '(1009 1013 1019)
   ;; '(100003 100019 100043)
   ;; '(1000003 1000033 1000037)
   ;; '(10000019 10000079 10000103)        ; more than 1 sec per test
   ;; '(100000007 100000037 100000039)
   ;; '(1000000007 1000000009 1000000021)
   ;; '(10000000019 10000000033 10000000061)
   ;; '(100000000003 100000000019 100000000057)
   ;; '(1000000000039 1000000000061 1000000000063)
   ;; '(10000000000037 10000000000051 10000000000099)
   ))

(display-fermat-test primes-list 1)

