#lang racket


(require racket/trace)

(define (A x y)
  (cond ((zero? y) 0)
        ((zero? x) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(trace A)

;; x=1 -> 2^y
;; (A 1 10)
;; (A 2 4)
;; (A 3 3)

(A 2 1)


(define (f n) (A 0 n))

(define (g n) (A 1 n))

(define (h n) (A 2 n))

(define (k n) (* 5 n n))

;; f(n) = 2*n
;; (f 10)

;; g(n) = 2^n
;; (g 10)

;;; __ Computing h __

;; h (n)
;; = A (2, n)
;; = A (1, A (2, n-1))
;; = g (A (2, n-1))
;; = g (h (n-1))

;;; implementing h0 as h. use g0 as g and f0 as f.

(define (h0 n)
  (define (f0 n) (* n 2))
  (define (g0 n) (if (= n 0) 1 (f0 (g0 (- n 1)))))
  (cond ((= n 0) 0)
        ((= n 1) 2)
        (else (g0 (h0 (- n 1))))))

(define (test-h n)
  (= (h n)
     (h0 n)))

;; (test-h 5)

(display "error\n" (current-error-port))



