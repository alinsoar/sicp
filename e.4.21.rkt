#lang racket

(require "sicp.rkt")
(GETMOD 4 1)

(module+ test
  (require rackunit)
  (require racket/sandbox)
  (define (test-eval e) (eval e the-global-environment))
  (define (test-eval-check-err e)
    (with-handlers (((lambda (v) (exn? v)) (lambda (v) (exn-message v))))
      (call-with-limits .01 1
                        (lambda ()
                          (test-eval e)))))

  (test-eval '(define (bottom) (bottom)))
  (test-eval '(define (fact0 n) (if (= n 0) 1 (bottom))))
  (test-eval '(define (fact1 n) (if (= n 0) 1 (* n (fact0 (- n 1))))))
  (test-eval '(define (fact2 n) (if (= n 0) 1 (* n (fact1 (- n 1))))))
  (test-eval '(define (fact3 n) (if (= n 0) 1 (* n (fact2 (- n 1))))))
  (test-eval '(define (fact4 n) (if (= n 0) 1 (* n (fact3 (- n 1))))))

  "fact0"
  (test-eval '(fact0 0))
  "fact1"
  (test-eval '(fact1 0))
  (test-eval '(fact1 1))
  "fact2"
  (test-eval '(fact2 0))
  (test-eval '(fact2 1))
  (test-eval '(fact2 2))
  "fact3"
  (test-eval '(fact3 0))
  (test-eval '(fact3 1))
  (test-eval '(fact3 2))
  (test-eval '(fact3 3))
  "fact4"
  (test-eval '(fact4 0))
  (test-eval '(fact4 1))
  (test-eval '(fact4 2))
  (test-eval '(fact4 3))
  (test-eval '(fact4 4))
  "catch bottom"
  (test-eval-check-err '(fact0 1))
  (test-eval-check-err '(fact1 2))
  (test-eval-check-err '(fact2 3))
  (test-eval-check-err '(fact3 4))
  (test-eval-check-err '(fact4 5))

  "general form"
  (test-eval
   '((lambda (n)
       ((lambda (fact)
          (fact fact n))
        (lambda (ft k)
          (if (= k 1)
              1
              (* k (ft ft (- k 1)))))))
     10))

  "even?"
  (test-eval
   '(define (f x)
      (define (even? n)
        (if (= n 0)
            true
            (odd? (- n 1))))
      (define (odd? n)
        (if (= n 0)
            false
            (even? (- n 1))))
      (even? x)))
  (cons (test-eval '(f 10))
        (test-eval '(f 11)))
  "even-odd using combinators"
  (test-eval
   '(define (g x)
      ((lambda (even? odd?)
         (even? even? odd? x))
       (lambda (ev? od? n)
         (if (= n 0) true (od? ev? od? (- n 1))))
       (lambda (ev? od? n)
         (if (= n 0) false (ev? ev? od? (- n 1)))))))

  (cons (test-eval '(g 100))
        (test-eval '(g 101)))
  (test-eval
   '(println "factorial(5) ="
           ((lambda (x)
              (println 'ONE x)
              (if (= x 0)
                  1
                  (* x
                     ((lambda (x)
                        (println 'TWO x)
                        (if (= x 0)
                            1
                            (* x
                               ((lambda (x)
                                  (println 'THREE x)
                                  (if (= x 0)
                                      1
                                      (* x
                                         ((lambda (x)
                                            (println 'FOUR x)
                                            (if (= x 0)
                                                1
                                                (* x
                                                   ((lambda (x)
                                                      (println 'FIVE x)
                                                      (if (= x 0)
                                                          (begin
                                                            (print-environment 'env)
                                                            1)
                                                          (bottom)))
                                                    (- x 1)))))
                                          (- x 1)))))
                                (- x 1)))))
                      (- x 1)))))
            4)))
  'done)


