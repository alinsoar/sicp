#lang racket


(require racket/trace)

(define runtime current-milliseconds)   ; in racket there are no
                                        ; microseconds, just milli

(define (expmod base exp m)
  (define square sqr)
  (cond ((= exp 0) 1)
        ((even? exp)
         ((lambda (v)
            ((lambda (rem)
               (if (and (= 1 rem)
                        (> v 1)
                        (< v (- m 1)))
                   0
                   rem))
             (remainder (square v) m)))
          (expmod base (/ exp 2) m)))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

'(trace expmod)

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n)
       1))
  ;; the algorithm of the random module in racket constraints the
  ;; programmer to limit the maximum. this might bias the test
  ;; for large numbers.
  (try-it (+ 1 (random (min 4294967087 (- n 1))))))

(define (fast-prime? n times)
  (if (= times 0)
      true
      (and (miller-rabin-test n)
           (fast-prime? n (- times 1)))))

(define (timed-prime-test n times)
  (display n)
  (start-prime-test n (runtime) times))

(define (start-prime-test n start-time times)
  (and (fast-prime? n times)
       (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (display-miller-rabin-test l times)
  (cond ((null? l) 'ok)
        (else
         (timed-prime-test (car l) times)
         (newline)
         (display-miller-rabin-test (cdr l) times))))

(define primes-list                     ; numbers found in 1.22
  (append
   '(100000007 100000037 100000039)
   '(1000000007 1000000009 1000000021)
   '(10000000019 10000000033 10000000061)
   '(100000000003 100000000019 100000000057)
   '(1000000000039 1000000000061 1000000000063)
   '(10000000000037 10000000000051 10000000000099)
   ))

(display-miller-rabin-test primes-list 2000)

;;; 561 is Caramichael -- it cannot fool Miller-Rabin
(display-miller-rabin-test '(1009 561 9999999999) 2000)


