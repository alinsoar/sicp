#lang racket

(require (rename-in (submod "e.2.18.rkt" export)
                    (rev0 reverse0)
                    (rev1 reverse1)))

(define (deep-reverse0 l)
  (cond ((null? l) '())
        ((pair? (car l))
         (append (deep-reverse0 (cdr l))
                 (list (deep-reverse0 (car l)))))
        (else
         (append (deep-reverse0 (cdr l))
                 (list (car l))))))

(define (deep-reverse1 l)
  (define (iter l res)
    (cond ((null? l) res)
          ((pair? (car l))
           (iter (cdr l)
                 (cons (deep-reverse1 (car l))
                       res)))
          (else
           (iter (cdr l)
                 (append (list (car l)) res)))))
  (iter l '()))

(define (deep-reverse2 l)
  (define (iter l co)
    (cond ((null? l) (co '()))
          ((pair? (car l))
           (iter (cdr l)
                 (lambda (x)
                   (cons (deep-reverse2 (car l))
                         (co x)))))
          (else
           (iter (cdr l)
                 (lambda (x)
                   (cons (car l)
                         (co x)))))))
  (iter l (lambda (x) x)))

(module+ test
  (define x (list (list 1 2) (list 3 4)))
  (reverse0 x)
  (reverse1 x)
  
  (deep-reverse0 x)
  (deep-reverse0 '(1 2 (3 (9 8 4) 5) 0))

  (deep-reverse1 x)
  (deep-reverse1 '(1 2 (3 (9 8 4) 5) 0))

  (deep-reverse2 x)
  (deep-reverse2 '(1 2 (3 (9 8 4) 5) 0)))

