#lang racket


(define (square x) (* x x))

(define (cube x) (* x x x))

(define (expt x n)
  (define (iter i co)
    ;; (display (format "~a\n" i))
    (if (zero? i)
        (co 1)
        (iter (- i 1)
              (lambda (w)
                ;; (display (format ">> ~a ~a\n" w (* x w)))
                (co (* x w))))))
  (iter n (lambda (x) x)))

(define (average x y) (/ (+ x y) 2))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess limit)
    (display (format "~a\n" guess))
    (if (zero? limit)
        'failed
        (let ((next (f guess)))
          (if (close-enough? guess next)
              next
              (try next (- limit 1))))))
  (try first-guess 1000))

;;; naive fixed point does not work at all for roots

(define (sqrt x)
  (fixed-point (lambda (y) (/ x y))
               1.0))

(define (cube-root x)
  (fixed-point (lambda (y) (/ x (square y)))
               1.0))

(sqrt 10)

(cube-root 10)

;;; average-damp works for 2 and 3 roots

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

(sqrt 25)

(cube-root 125)

;;; starting from the 4th root it no more works

(define (fourth-root x)
  (fixed-point (average-damp (lambda (y) (/ x (cube y))))
               1.0))

(fourth-root 16)

;;; average-damp applied 2 times works for 4th roots
(define (fourth-root x)
  (fixed-point (average-damp (average-damp (lambda (y) (/ x (cube y)))))
               1.0))

(fourth-root 16)

;;; now Nth root...

;;; we use this variant of repeat
(define (repeated s n)
  (define (iter i col)
    ;; (display (format ">~a\n" i))
    (cond 
     ((zero? i) col)
     (else (iter (- i 1)
                 (lambda (f)
                   ;; (display (format "* ~a --\n" f))
                   (col (s f)))))))
  (iter n (lambda (x) x)))

;;; average-damp applied 2 times works for 4th roots
(define (nth-root x n)
  (define (lg a b)
    "logarithm in base a of b"
    (ceiling (/ (log b) (log a))))
  (fixed-point ((repeated average-damp
                          (lg 2 n))
                (lambda (y) (/ x (expt y (- n 1)))))
               1.0))


(nth-root 1024 10)

(define (test n k)
  (- n
     (expt (nth-root n k) k)))

(test 1024 10)




