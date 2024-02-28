#lang racket


(define (fast-mul b n a)
  (cond ((= n 0) a)
        ((even? n) (fast-mul (* 2 b) (/ n 2) a))
        (else (fast-mul b (- n 1) (+ a b) ))))

(fast-mul 7 20 0)


;; at each step we have (b * n + a) constant

;;; B * N + A invariant :
;;;
;;; B * N + 0 =
;;; 7 * 20 + 0 =
;;; (7 * 2) * 10 + 0 =
;;; (14 * 2) * 5 + 0 =
;;; (28) * 4 + (28) =
;;; (28*2) * 2 + (28) =
;;; (56*2) * 1 + (28) =
;;; 112 * 0 + (28+112) => 28+112

