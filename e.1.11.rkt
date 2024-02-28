#lang racket


(require racket/trace)

;;; f(n) = f(n-1) + 2f(n-2) + 3f(n-3)

(define (f n)
  (if (< n 3) n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))


;;; defining g = f

;;; a <- b
;;; b <- c
;;; c <- 3*a + 2*b + c

(define (g n a b c)
  (if (zero? n) a
      (g (- n 1)
         b
         c
         (+ (* 3 a)
            (* 2 b)
            c)))) 

;; (trace f)

(f 25)

;; (trace g)

(g 25 0 1 2)

(define (test n)
  (= (f n)
     (g n 0 1 2)))


(test 33)


