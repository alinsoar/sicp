#lang racket


(define (iterative-improve good-enough? improve-guess)
  (lambda (guess)
    (lambda (x)
      (define (iter g w n)
        (if (zero? n)
            'failed
            (begin
              (display (format "~a\n" g))
              (if (good-enough? g w x)
                  g
                  (iter w (improve-guess g w) (- n 1))))))
      (iter guess (improve-guess guess x) 1000))))

;;; SQRT
(define (sqrt x)
  (define square (lambda (x) (* x x)))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (average x y)
    (/ (+ x y) 2))
  (define (good-enough? guess _ x)
    (< (abs (- (square guess) x)) 0.001))
  (((iterative-improve good-enough? improve) 1.0) x))

(sqrt 9)

;;; Fixed point

(define (fixed-point f first-guess)
  (define tolerance 1e-20)
  (define (close-enough? v1 v2 'a) (< (abs (- v1 v2)) tolerance))
  (define (try x y) (f y))
  (((iterative-improve close-enough? try) 1.0) 1))

(define (fixed-point-cosine n c)
  "for testing the real fixed point of cos"
  (if (= n 0) (c 1) (fixed-point-cosine
                     (- n 1)
                     (lambda (x) (c (cos x))))))

(-
 (fixed-point-cosine 1000 (lambda (x) x))
 (fixed-point cos 1.0))

