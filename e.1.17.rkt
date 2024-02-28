#lang racket


(define (double x) (* x 2))
(define (halve x) (/ x 2))

(define (mul a b)
  "RECURSIVE process"
  (cond ((= b 0) 0)
        ((even? b) (double (mul a (halve b))))
        (else (+ a (mul a (- b 1))))))

(trace mul)

(mul 11 11)

(define (fast-mul2 a b k)
  "ITERATIVE process. We keep invariant a*b+k all the time, and we try
to make b go towards 1, such that eventually we have a*1+k = a+k."
  (cond ((= b 1)
         ;; a*1 + k => a+k -- invariant
         (+ a k))
        ((even? b)
         ;; a*(m=b/2) + k => 2a*m + k -- invariant
         (fast-mul2 (double a)
                    (halve b)
                    k))
        (else
         ;; a*(b=2*m+1) + k => a*(2m=b-1) + k -- invariant
         (fast-mul2 a
                    (- b 1)
                    (+ k a)))))

(trace fast-mul2)

(fast-mul2 5 6 0)


