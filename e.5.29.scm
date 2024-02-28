
(load "aux/msim")

(set! make-stack make-stack2)

(load "aux/eceval")

(define the-global-environment (setup-environment))

(start eceval)

;;; k = a*n+b
;;; k1 = a*n1+b
;;; k2 = a*n2+b
;;; k2-k1 = a(n2-n1)
;;; a = (k2-k1)/(n2-n1)
;;; b = k1-a*n1 = k2-a*n2
(define compute-linear-coeff
  (lambda (n1 k1 n2 k2)
    ((lambda (a) ((lambda (b)
               (display "\na=") (display a)
               (display "\nb=") (display b)
               (display "\n"))
             (- k1 (* a n1))))
     (/ (- k2 k1) (- n2 n1)))))

;;; ********** S(n) can be expressed as a*Fib(n + 1) + b
;;; k1 = a*fib(n1+1)+b
;;; k2 = a*fib(n2+1)+b
;;; a = (k1-k2)/(fib(n1+1)-fib(n2+1))
;;; b = k1 - a*fib(n1+1) = k2 - a*fib(n2+1)
(define compute-exponential-coeff
  (lambda (n1 k1 n2 k2)
    (compute-linear-coeff (fib (+ 1 n1)) k1 (fib (+ 1 n2)) k2)))

;;; this time we have 2 linear equations, for recursive process.
(define stats
  (lambda (n)
    (define a1 5)
    (define b1 3)
    (define a2 56)
    (define b2 -40)
    (display "\nexpected pushes = ") (display (+ b1 (* a1 n)))
    (display "   ") (display (+ b2 (* a2 (fib2 (+ 1 n))))) ; S(n)=a*Fib(n + 1) + b
    (display "\n")))

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(define (fib2 n)
  (define (iter n a b)
    (if (= n 0)
        a
        (iter (- n 1) b (+ a b))))
  (iter n 0 1))

;;; because we cannot get in the environment of eceval the statistics
;;; we precompute it here and insert the values in `stats`.
(compute-linear-coeff 1 8 10 53)
(compute-exponential-coeff 1 16 10 4944)

;;; because the code of stats influences the stack we cannot combine
;;; the both in the `factorial` function.  eceval resets the stack
;;; after each input.

;; maximum depth required to evaluate n! is 10 for each n.
(fib 1)  (stats 1)
(fib 2)  (stats 2)
(fib 3)  (stats 3)
(fib 4)  (stats 4)
(fib 5)  (stats 5)
(fib 6)  (stats 6)
(fib 7)  (stats 7)
(fib 8)  (stats 8)
(fib 9)  (stats 9)
(fib 10) (stats 10)
(fib 11) (stats 11)
(fib 12) (stats 12)
