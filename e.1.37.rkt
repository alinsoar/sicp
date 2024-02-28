#lang racket/base

(require racket/trace)

(define 1/theta (/ 2 (+ 1 (sqrt 5)))) ; 0.6180339887498948

;;; recursive process
(define (cont-frac-rec n d k max)
  (if (zero? max)
      0
      (/ (n k)
         (+ (d k)
            (cont-frac-rec n d (+ k 1) (- max 1))))))

;;; iterative process
(define cont-frac-iter-rec-co
  (lambda (n d k max col)
    (if (zero? max)
        (col 0)
        (cont-frac-iter-rec-co
         n d (+ k 1) (- max 1)
         (lambda (w)
           '(display (format "~a\t~a\t~a\t~a\n"
                            k w (n k) (d k)))
           (col (/ (n k)
                   (+ w (d k)))))))))

;;; iterative process
(define cont-frac-iter
  (lambda (n d max)
    (define (iter k res)
      '(display (format "*~a\t~a\t~a\t~a\n"
                       k (n k) (d k) res))
      (if (zero? k)
          (/ (n 0) (+ res (d 0)))
          (iter
           (- k 1)
           (/ (n k)
              (+ res
                 (d k))))))
    (iter max 0)))

;;; cannot instrument functions from submodules, because mutation is
;;; not possible
;; (trace cont-frac-iter)
;; (trace cont-frac-rec)

(module+ test
  (- 1/theta
     (cont-frac-rec (lambda (x) 1.0)
                    (lambda (x) 1.0)
                    0
                    10))
  (- 1/theta
     (cont-frac-iter-rec-co (lambda (x) 1.0)
                            (lambda (x) 1.0)
                            0
                            10
                            (lambda (x) x)))
  (- 1/theta
     (cont-frac-iter (lambda (x) 1.0)
                     (lambda (x) 1.0)
                     10)))

(module+ export
    (provide cont-frac-iter-rec-co
             cont-frac-iter
             cont-frac-rec))
