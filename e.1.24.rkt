#lang racket

(define runtime current-milliseconds)

(define (expmod base exp m)
  (define (square x) (* x x))
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

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

(define primes-list                     ; numbers found in 1.22
  (append
   '(100000007 100000037 100000039)
   '(1000000007 1000000009 1000000021)
   '(10000000019 10000000033 10000000061)
   '(100000000003 100000000019 100000000057)
   '(1000000000039 1000000000061 1000000000063)
   '(10000000000037 10000000000051 10000000000099)
   ))

(module+ test
  (display-fermat-test primes-list 100))

(module+ export
  (provide fast-prime?))

;;; | 
;;; | -- if N is prime, and A is any number 1 < A <= N-1, then { A, A^2, A^3
;;; | -- ... A ^{N-1} } is a permutation of {1, 2, 3, ... N-1}.
;;; | --
;;; | -- If N is not prime, then { A, A^2, A^3 ... A ^{N-1} } may be or may
;;; | -- not be a permutation of {1, 2, 3, ... N-1}.
;;; | --
;;; | -- the probability not to be a permutation is quite low.
;;; | 
;;; | (define (fermat-pow a i n c)
;;; |   (if (= i 0)
;;; |       (if (= c 1)
;;; |           '(("\t\t "))
;;; |           (list (list "\t" c "  ")))
;;; |       (let ((v (expmod a i n)))
;;; |         (cons (if (= v 1) '** v)
;;; |               (if (= v 1)
;;; |                   (fermat-pow a (- i 1) n (+ c 1))
;;; |                   (fermat-pow a (- i 1) n c))))))
;;; | 
;;; | ;;'(trace fermat-pow)
;;; | 
;;; | (define (show-powers a n)
;;; |   (reverse (fermat-pow a (- n 1) n 0)))
;;; | 
;;; | (define (display-all-combinations n)
;;; |   (define (loop a)
;;; |     (if (= a 1)
;;; |         'ok
;;; |         (begin
;;; |           (display a)
;;; |           (display " : ")
;;; |           (display (show-powers a n))
;;; |           (newline)
;;; |           (loop (- a 1)))))
;;; |   (loop (- n 1)))
;;; | 
;;; | '(display-all-combinations 19)
;;; | 
;;; | -- proof of Fermat's theorem
;;; | --
;;; | -- Useful: Euclid-Elements.Book-VII.Proposition-30:
;;; | -- (p is prime and p div a*b ) => (p div a or p div b)
;;; | --
;;; | -- Using Euclid's proposition, one can deduce Fernat's little theorem
;;; | --
;;; | -- [x] means x (mod p). div means "is divisible by" = "|".
;;; | --
;;; | -- Let 0 < a < p. Then [a], [2*a], ... [(p-1)*a] are different.
;;; | --
;;; | -- Suppose ( 0 < n < m < p and a*n = a*m). Then [(m-n)*a] = [0]. So
;;; | -- (m-n)*a = p*e , hence (p div a*(m-n)).  From Euclid's proposition,
;;; | -- p div (m-n) or p div a. But a < p and m-n < p. False.
;;; | --
;;; | -- Using the fact that [a], [2a], ... [(p-1)a] is a permutation of
;;; | -- {1,2, ... p-1}, we have [(p-1)!] = [1]*[2]*...*[p-1] =
;;; | -- [a]*[2a]*...*[(p-1)a] = [a^ (p-1)] * [(p-1)!]. So [a^(p-1)] = [1]
;;; | 
;;; |

