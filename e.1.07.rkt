#lang racket


(define (square x) (* x x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;;; **********************************************************************

;;; ORIGINAL VERSION -- 2 tests

(define (good-enough? guess x)
  (< (abs (- (square guess) x))
     accuracy))

;;; LARGE number, small accuracy compared to the number: this never
;;; stops -- for example, for accuracy = 0.1 or accuracy = 1000 and
;;; big N
(define accuracy 1000)
(sqrt 123283298392839289382983298320)

;;; SMALL numbers, small accuracy compared to N: this never stops
;;; example, accuracy = 1e-60 and N = .2e-10.
(define accuracy 1e-60)
(sqrt .000000000000000022)


;;; **********************************************************************

;;; MODIFIED VERSION -- redefining the test ``good-enough?``

(define frac 1e-90)

(define (good-enough? guess x)
  (display guess)
  (newline)
  (display (abs (- guess (improve guess x))))
  (newline)
  (display (abs (- (square guess) x)))
  (newline)
  (newline)
  (< (abs (- guess (improve guess x)))  ; stop if the improvement
     (* guess frac)))                   ; is smaller than a `little`
                                        ; fraction of ``guess``.

(define (test N)
  "return the error of square root"
  (abs (- (square (sqrt N)) N)))

;;; taking a SMALL number and a fraction of 1e-25 times old-guess,
;;; seems to work very well
(test .000000000000000000000000002)

;;; taking a LARGE number, with 26 digits, and a fraction of 1e-25 of
;;; old guess, so an error of maximum 10 when inverting the operation,
;;; seems to stop all the time, but the error is large (2e10).
(test 12121212121212121212111219)


