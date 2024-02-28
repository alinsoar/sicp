#lang racket

(require "sicp.rkt")

(GETMOD 4 55)

(module+ test
  (initialize-data-base microshaft-data-base)

  (test/ '((lives-near ?x (Hacker Alyssa P))
           (lives-near ?x ?y)

           (and (job ?x (?y ?z))
                (lisp-value (lambda (x) (= 1 2))
                            '?z))
           
           (and (lives-near ?x ?y)
                (lisp-value (lambda (x y) (equal? x y))
                            '?x
                            '?y
                            ))
           
           (and (lives-near ?x ?y)
                (lisp-value (lambda (p1 p2 map)
                              ((lambda (s)
                                 (s s
                                    (map symbol-name p1)
                                    (map symbol-name p2)))
                               (lambda (s n1 n2)
                                 (cond ((null? n1) true)
                                       ((null? n2) false)
                                       ((string<? (car n1) (car n2)) true)
                                       ((string>? (car n1) (car n2)) false)
                                       (else (s s (cdr n1) (cdr n2)))))))
                            '?x
                            '?y
                            (lambda (f ls)
                              ((lambda (f)
                                 (f f ls))
                               (lambda (m l)
                                 (if (null? l)
                                     '()
                                     (cons (f (car l))
                                           (m m (cdr l)))))))))

           (same ?a ?b)))
  
  'done)



