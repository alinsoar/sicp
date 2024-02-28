#lang racket

(require "sicp.rkt")
(GETMOD 3 50)

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define 2**
  (lambda (n)
    "computes 2^n"
    (define (iter k co)
      (if (= k 0)
          (co 1)
          (iter (- k 1)
                (lambda (x)
                  (co (* 2 x))))))
    (iter n (lambda (x) x))))

(define general-position
  (lambda (k n)
    "brute formula, deduced using step by step analysis from test."
    (if (= n 1)
        (- (2** k) 1)
        (+ (+ (- (2** k) 1)
              (2** (- k 1)))
           (* (2** k)
              (- n 2))))))

(module+ test
  (define limit 7)
  (define (test s)
    (print-row-n s limit)
    "---")
  (define (test-position k)
    (define (iter n)
      (if (> n limit)
          (d)
          (begin (display-row (general-position k n))
                 (iter (+ n 1)))))
    (iter 1))
  (define (test-nth k)
    "a test of positions for the pairs like (K N)"
    (d "positions for K =" k)
    (define pos (stream-map
                 car
                 (stream-filter
                  (lambda (x) (= (caadr x) k))
                  (pair-streams integers
                                (pairs integers integers)))))
    (test pos)
    (test-position k)
    (let* ((first (stream-car pos))
           (second (stream-car (stream-cdr pos)))
           (third (stream-car (stream-cdr (stream-cdr pos))))
           (diff (- third second)))
      (or (= first (- (2** k) 1))
          (error "Error! -- FIRST"))
      (or (= second (+ first (2** (- k 1))))
          (error "Error! -- SECOND"))
      (or (= diff (2** k))
          (error "Error! -- DIFF"))
      (d (~a "General formula for K=" k
             "-- FIRST= " (- (2** k) 1)
             " AND REST = " (+ first (2** (- k 1))) "+ " (2** k) "*K"
             )))
    "---")
  (test integers)
  (test (pair-streams integers
                      (pairs integers integers)))
  (test-nth 1)
  (test-nth 2)
  (test-nth 3)
  (test-nth 4)
  (test-nth 5)
  (test-nth 6)
  (test-nth 7)
  "position of the pair (500, 1000) in the interleaved stream"
  (general-position 500 1000)
  'done)

(module+ export
  (provide interleave
           pairs))
