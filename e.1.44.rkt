#lang racket

;;; we need to compute a combination of the form:
;;; s(s(s(s(s...s(f(x))...))))

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

;;; simple visual test:
(define test-s (lambda (x) (+ 2 x)))
(define test-f (lambda (x) (* x 3)))

;;; 1*3 + 2 + 2 + 2 + .. + 2 = s(s(s(s(s(..s(f(x))..))))) = 23
(define rep-s (repeated test-s 10))

;;; now n-fold-smooth:

(define (reduce op init l)
  (if (null? l)
      init
      (op (car l)
          (reduce op init (cdr l)))))

;;; smooth : (number -> number) -> (number -> number)
(define smooth
  (lambda (f)
    (define (avg3 l) (/ (reduce + 0 l) 3))
    (lambda (x)
      (avg3 (map f (list (- x dx) x (+ x dx)))))))

(define n-fold-smooth
  (lambda (f n)
    ((repeated smooth n) f)))

(define square (lambda (x) (* x x)))
(define dx 1e-5)

(module+ test
  (rep-s (test-f 1))                    ; value as expected

  ;; 3 ways to compute the same value
  ((n-fold-smooth square 5) 10)

  (((repeated smooth 5) square) 10)

  ((smooth
    (smooth
     (smooth
      (smooth
       (smooth square)))))
   10))

(module+ export
  (provide reduce))

