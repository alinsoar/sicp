#lang racket

(require "sicp.rkt")
(GETMOD 4 35)

(module+ test
  "===================="
  (amb-test
   '((define (member? a l)
       (define (iter l)
         (cond ((null? l) false)
               ((equal? a (car l)) true)
               (else
                (iter (cdr l)))))
       (iter l))
     (define (distinct? l)
       (define (iter l)
         (cond ((null? l) true)
               ((member? (car l) (cdr l)) false)
               (else
                (iter (cdr l)))))
       (iter l))
     (define (xor a1 a2)
       (if a1
           (not a2)
           a2))
     (define (liars)
       (let ((betty (amb 1 2 3 4 5))
             (ethel (amb 1 2 3 4 5))
             (joan (amb 1 2 3 4 5))
             (kitty (amb 1 2 3 4 5))
             (mary (amb 1 2 3 4 5)))
         (require (distinct? (list mary kitty joan ethel betty)))
         (require (xor (= betty 3) (= kitty 2))) ; betty
         (require (xor (= ethel 1) (= joan 2)))  ; ethel
         (require (xor (= joan 3) (= ethel 5)))  ; joan
         (require (xor (= kitty 2) (= mary 4)))  ; kitty
         (require (xor (= mary 4) (= betty 1)))  ; mary
         (list (cons 'mary mary)
               (cons 'kitty kitty)
               (cons 'joan joan)
               (cons 'ethel ethel)
               (cons 'betty betty))))))
  (amb-test '((liars)
              try-again
              try-again
              ))
  'done)




