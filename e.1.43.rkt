#lang racket

(require (submod "e.1.42.rkt" export))

(define (square x) (* x x))

;;; recursive
(define (repeated0 f n)
  (lambda (x)
    (define (rec i)
      (if (zero? i)
          x
          (f (rec (- i 1)))))
    (rec n)))


;;; recursive
(define (repeated1 f n)
  (lambda (x)
    (if (zero? n)
        x
        ((compose f (repeated1 f (- n 1)))
         x))))

;;; iterative-recursive
(define (repeated2 f n)
  (lambda (x)
    (define (iter i co)
      '(display (format "~a ~a\n" i x))
      (if (zero? i)
          (co x)
          (iter (- i 1)
                (lambda (x)
                  (co (f x))))))
    (iter n (lambda (x) x))))


;;; iterative-recursive
(define (repeated3 f n)
  (define (iter i co)
    '(display (format "~a ~a\n" i x))
    (if (zero? i)
        co
        (iter (- i 1)
              (lambda (x)
                (co (f x))))))
  (iter n (lambda (x) x)))

(module+ test
  ((repeated0 square 5) 10)
  ((repeated1 square 5) 10)
  ((repeated2 square 5) 10)
  ((repeated3 square 5) 10))


