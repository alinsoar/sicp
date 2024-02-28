#lang racket


(require racket/trace)

(define (fast-exp b n a)
  "a * b^n is invariant at each step. the purpose is to make b go
toward 1, to have as limit case a*1^n."
  ;; (display (* a (expt b n)))
  (newline)
  (cond ((zero? n)
         ;; B^N = A * B ^ 0
         a)
        ((even? n)
         ;; B^(2 * N) = (B^2)^N
         (fast-exp (* b b) (/ n 2) a))
        (else
         ;; B^(2 * N + 1) = B * B^(2 * N)
         (fast-exp b (- n 1) (* b a)))))

(trace fast-exp)

(fast-exp 3 22 1)

;;; === b^n*a is invariant all the time
;;; 
;;; b         ^   n       *    a
;;; ___________________________________
;;; a         ^  22       *    1
;;; (a^2)     ^  11       *    1
;;; (a^2)     ^  (1+10)   *    1
;;; (a^2)     ^  10       *    a^2
;;; (a^2^2)   ^  5        *    a^2
;;; a^2^2     ^  (1+4)    *    a^2
;;; a^2^2     ^  4        *    a^8
;;; a^2^2^2   ^  2        *    a^8
;;; a^2^2^2^2 ^  1        *    a^8
;;; a^16      ^  0        *    a^(8+16)

