#lang racket

(require (submod "e.2.40.rkt" export))

(define (pairs-sum-s n s)
  (filter (lambda (x)
            (let ((i (car x)) (j (cadr x)) (k (caddr x)))
              (and (positive? k)
                   (not (= i k))
                   (not (= j k)))))
          (flatmap (lambda (i)
                     (flatmap (lambda (j) (list (list j i (- s (+ i j)))
                                                (list i j (- s (+ i j)))))
                              (enumerate-interval 1 (- i 1))))
                   (enumerate-interval 1 n))))

(module+ test

  (pairs-sum-s 10 10))

