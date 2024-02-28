#lang racket


(define square (lambda(x) (* x x)))

(define (square-list items)
  (define (iter things answer)
    (display answer)
    (newline)
    (if (null? things)
        answer
        (iter (cdr things)
              ;; current element is inserted before the
              ;; already-processed elements that are kept in `answer`
              (cons (square (car things))
                    answer))))
  (iter items '()))

(define (square-list2 items)
  (define (iter things answer)
    (display answer)
    (newline)
    (if (null? things)
        answer
        (iter (cdr things)
              ;; should (append answer (list (square (car things))))
              ;; because answer is a list. Here, it conses a list to
              ;; an element.
              (cons answer
                    (square (car things))))))
  (iter items '()))

(square-list '(2 8 3 9 4))

(square-list2 '(2 8 3 9 4))
