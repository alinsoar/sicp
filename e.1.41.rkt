#lang racket


(define inc (lambda (x) (+ x 1)))

(define (double f)
  (lambda (x)
    (f (f x))))

((double inc) 3)

;;; re-written as:
(((lambda (f)
    (lambda (x)
      (f (f x))))
  inc)
 3)

(((double double)
  inc) 3)

;;; re-written as:
((((lambda (f)
     (lambda (x)
       (f (f x))))
   (lambda (f)
     (lambda (x)
       (f (f x)))))
  inc)
 3)

(((double (double double))
  inc)
 5)

;;; re-written as:
((((lambda (f)
     (lambda (x)
       (f (f x))))
   ((lambda (f)
      (lambda (x)
        (f (f x))))
    (lambda (f)
      (lambda (x)
        (f (f x))))))
  inc)
 5)

