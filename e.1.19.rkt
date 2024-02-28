#lang racket


(require racket/format)

(define (fib n) (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  "we are using the notation p <- fib_{n-1} and q <- fib_n, and we are
trying to compute p' = fib_{2n+1} and q' = fib_2n in function of p and
q.

We have invariant all the time 

   [ p+q q ]^counter * [a]
   [ q   p ]           [b]

a and b are the initial values fib_1 and fib_0.

The product T^n * [a; b] is invariant all the time. Decrementing
counter either by half, or by 1, in the moment when counter reaches 0,
the b will contain fib_n and a will contain fib_{n+1}.

See the bottom analysis.
"
  (cond ((= count 0) a)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q))        ;  p' = fib_{2n+1}
                   (+ (* 2 p q) (* q q))      ;  q' = fib_2n
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))


(define (test i)
  (define (print x y)
    (printf "~a\t~a\t~a\n" i x (+ x y)))
  (if (= i 0)
      'ok
      (begin
        (print (fib i) (fib (- i 1)))
        (test (- i 1)))))



(test 10)


;;; HOW to deduce the fast fibonacci formula.
;;; 
;;; Step 1...............................
;;;
;;; Express recursion via matrix computations. Compute the matrix of
;;; recursion.
;;;
;;; [x y] [a] = [a+b]
;;; [z t] [b]   [ a ]
;;;
;;; This equation can be re-written in the form
;;;
;;; a[x] + b[y] = [a+b]
;;;  [z]    [t]   [ a ]
;;;
;;;; Hence, we have
;;;
;;; x = y = z = 1 and t = 0
;;;
;;; Hence, the recursion can be expressed via the matrix.
;;;
;;; T = [1 1] = [fib_2 fib_1]
;;;     [1 0]   [fib_1 fib_0]
;;;
;;; T*T = [2 1] = [fib_3 fib_2]
;;;       [1 1]   [fib_2 fib_1]
;;;
;;; More general, we have
;;;
;;; T^n = [ fib_{n+1} fib_n     ]
;;;       [ fib_n     fib_{n-1} ]
;;; 
;;; Step 2. counter is even...............................
;;;
;;; The central idea is to try to express the matrix T^2n in funcition
;;; of the matrix T^n
;;;
;;; T^2n = [ fib_{2n+1} fib_2n     ]
;;;        [ fib_2n     fib_{2n-1} ]
;;;
;;; We have T^2n = T^n * T^n
;;; 
;;; T^n * T^n = [ fib_{n+1} fib_n   ] * [ fib_{n+1} fib_n     ] = ...
;;;             [ fib_n   fib_{n-1} ]   [ fib_n     fib_{n-1} ]
;;; 
;;;  = [(fib_{n+1})^2 + (fib_n)^2          fib_{n+1}*fib_n + fib_n*fib_{n-1} ]
;;;    [fib_n*fib_{n+1} + fib_{n-1}*fib_n  (fib_n)^2 + (fib_{n-1})^2         ]
;;;
;;; If we equalize this matrix with T^2n we get
;;;
;;; fib_2n = fib_{n+1}*fib_n + fib_n*fib_{n-1}
;;;        = fib_n*(fib_{n+1} + fib_{n-1})
;;;        = fib_n*(fib_n + f_{n-1} + fib_{n-1})
;;;        = fib_n*(fib_n + 2*fib_{n-1})
;;; fib_{2n-1} = (fib_n)^2 + (fib_{n-1})^2
;;;
;;; If n is even, these 2 formulas can be applied to compute the square
;;; of the matrix.
;;; 
;;; In the code, we use the notation
;;;   
;;;   p <- fib_{n-1}
;;;   q <- fib_n
;;;
;;; In this notation, we can form the matrices:
;;;
;;; T^n = [fib_{n+1} fib_n    ] = [ p+q  q ]
;;;       [fib_n     fib_{n-1}]   [  q   p ]
;;; 
;;; T^2n = [ fib_{2n+1} fib_2n     ] = [ p'+q'  q' ] = [  \  q^2+2pq ]
;;;        [ fib_2n     fib_{2n-1} ]   [ q'     p' ]   [  \  q^2+p^2 ]
;;; 
;;; Step 3. counter is odd...............................
;;;
;;; This was the case when we rise T to an even power. Now we cope
;;; with the case when we have (T^w)^(2k+1). T^w is the matrix of p,q.
;;; 
;;; (T^w)^(2k+1) = (T^w)^2k * (T^w)
;;;
;;; In this case we multiply the T^w with the initial values [ a b ],
;;; and do:
;;;
;;; [a'] = [ p+q q ] [a] = [ ap + aq + bp ]
;;; [b']   [  q  p ] [b]   [ aq + bp      ]
;;;
;;;  Like this, the counter k is decremented by 1, and [a ; b] is
;;;  multiplied only once by T^w.
;;;



