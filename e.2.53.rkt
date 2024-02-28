#lang racket


(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(define (memq item x)
  (define (iter l co)
    (cond ((null? l) (co false))
          ((eq? (car l) item)
           (co l))
          (else (iter (cdr l)
                      (lambda (w)
                        (co w))))))
  (iter x (lambda (x) x)))


(memq 'apple '(pear banana prune))
(memq 'apple '(x (apple sauce) y apple pear))

(list 'a 'b 'c)

(list (list 'george))

(cdr '((x1 x2) (y1 y2)))

(cadr '((x1 x2) (y1 y2)))

(pair? (car '(a short list)))

(memq 'red '((red shoes) (blue socks)))

(memq 'red '(red shoes blue socks))




