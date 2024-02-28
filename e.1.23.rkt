#lang racket

(define runtime current-milliseconds)   ; in racket there are no
                                        ; microseconds, just milli

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (and (prime? n)                       ; `and` instead of `if`
       (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (next x) (if (= x 2) 3 (+ x 2)))

(define (find-divisor n test-divisor)
  (define (square x) (* x x))
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n ;; (+ test-divisor 1)
                            (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (search-for-primes k)
  (define (rec N res)
   (cond ((= (length res) 3)
          (display (format "\n\n=> ~a" res )))
         ((timed-prime-test N)
          (rec (+ N 1) (append res (list N))))
         (else
          (rec (+ N 1) res))))
  (rec k '()))

(module+ test

  ;; it is indeed (140/80)x faster
  (search-for-primes 1000)
  (search-for-primes 100000)
  (search-for-primes 1000000)
  (search-for-primes 10000000)
  (search-for-primes 100000000)
  (search-for-primes 1000000000)
  (search-for-primes 10000000000)
  (search-for-primes 100000000000)
  (search-for-primes 1000000000000)
  (search-for-primes 10000000000000)
  )

(module+ export
  (provide prime?))
