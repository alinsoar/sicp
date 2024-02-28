#lang racket


;;; a real proof is done by computing the eigenvalues of the diagonal
;;; decomposition of the matrix that defines the recursion. write
;;; M=S^{-1}{lambda}S and compute eigenvalues of lambda. Here I am
;;; just going to make a test for some values.

(define sq-root-5 (sqrt 5))

(define psi (/ (- 1 sq-root-5) 2))

(define theta (/ (+ 1 sq-root-5) 2))

;;; if we change the definition of the function pow(x,y)=x^y, there
;;; will still be another error for large N -- the error depends on
;;; definition and on how floating point is implemented in hardware.
(define (pow const n)
  (cond ((zero? n) 1)
        ((= 0 (remainder n 2))
         (let ((k (pow const (/ n 2))))
           (* k k)))
        (else (let ((k (pow const (/ (- n 1) 2))))
                (* const k k)))))

(define (fib n) (/ (- (pow theta n)
                      (pow psi n))
                   sq-root-5))

(define (fib2 n)
  "this computation is always precise"
  (define (fib-iter a b count)
    (if (zero? count)
        b
        (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))

;; floating numbers will overflow.
(define (test-fib n)
  (if (zero? n)
      'ok
      (begin
        (printf "~a\t~a\t ~a\n" n (fib2 n) (- (fib2 n) (fib n)))
        (test-fib (- n 1)))))

(test-fib 100)




