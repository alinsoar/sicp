#lang racket

(require "sicp.rkt")
(GETMOD 3 50)
(GETMOD 3 70)

(module+ test
  (d "--")

  (define (2** x) (* x x))
  (define (f p) (+ (2** (car p)) (2** (cadr p))))
  
  (define S (weighted-pairs integers
                            integers
                            f))

  (define group-3-pairs
    (stream-map
     (lambda (x) (list (car x) (caadr x) (cadadr x)))
     (pair-streams
      S
      (pair-streams
       (stream-cdr S)
       (stream-cdr (stream-cdr S))))))

  (define s-numbers
    (stream-map
     (lambda (x) (format "~a = ~a^2+~a^2 = ~a^2+~a^2 = ~a^2+~a^2"
                         (f (car x))
                         (caar x) (cadar x)
                         (caadr x) (cadadr x)
                         (caaddr x) (cadar (cddr x))))
     (stream-filter
      (lambda (x) (= (f (car x))
                     (f (cadr x))
                     (f (caddr x))))
      group-3-pairs)))
  
  (d "--")
  (print-row-n S 30)
  (d "--")
  (print-row-n group-3-pairs 100)
  "sum of 2 squares in 3 different ways"
  (print-column-n s-numbers 20)
  
  'done)

(module+ export
  (provide weighted-pairs))
