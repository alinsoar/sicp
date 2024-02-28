#lang racket

(require (submod "e.2.33.rkt" export))

(define (count-leaves0 x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves0 (car x))
                 (count-leaves0 (cdr x))))))

(define (count-leaves1 t)
  "count-leaves returns the number of leaves from t"
  (accumulate +
              0
              (map (lambda (x)
                     ;; map counts here the number of leaves of each
                     ;; element of t
                     (if (pair? x)
                         (count-leaves1 x)
                         1))
                   t)))

(module+ test

  (define x (cons (list 1 2) (list 3 4)))
  (length x)
  (count-leaves0 x)

  (count-leaves0 '(1 2 (3 4 5 (6 7) 8)))

  (count-leaves1 x)

  (count-leaves1 '(1 2 (3 4 5 (6 7) 8))))

