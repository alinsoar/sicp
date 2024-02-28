#lang racket


(define (subsets s)
  "for each set, compute the subsets of the tail, and add the head in
each of the subsets of the tail. At limit case, the subsets of the
void set are the list that contain only a void set"
  (if (null? s)
      '(())
      (let ((rest (subsets (cdr s))))
        (append rest
                (map (lambda (x) (cons (car s) x))
                     rest)))))

(define (subsets s)
  (define (iter s co)
    (if (null? s)
        (co '(()))
        (iter (cdr s)
              (lambda (x)
                (co (append x
                            (map (lambda (y) (cons (car s) y)) x)))))))
  (iter s (lambda (x) x)))

(subsets '(1 2 3 4))



