#lang racket


(define (foreach fun items)
  (if (null? items)
      #t
      (begin
        (fun (car items))
        (foreach fun (cdr items)))))

(define (for-each f items)
  (define (iter l co)
    (if (null? l)
        (co)
        (iter (cdr l)
              (lambda (x)
                (co x)
                (f (car l))))))
  (iter items (lambda (x) #t)))

(foreach (lambda(x) (display x) (newline)) '(1 2 3 4))



