#lang racket

(require (submod "e.2.40.rkt" export))
(require (submod "e.2.36.rkt" export))

(define empty-board '(()) )

(define (nth k l)
  "returns nth element of a list"
  (define (iter l k co)
    (if (= k 0)
        (co (car l))
        (iter (cdr l)
              (- k 1)
              (lambda (x) x))))
  (iter l (- k 1) (lambda (x) x)))

(define (any-but-for-last l)
  "returns true when any of the first n-1 elements (but for the last
one) is true, false in all the other cases."
  (if (null? (cdr l))
      false
      (or (car l)
          (any-but-for-last (cdr l)))))

(define (zip a b) (accumulate-n cons '() (list a b)))

(define (adjoin-position new-row k rest-of-queens)
  "for each element of `rest-of-queens` add at its end the
`new-row`. So, for example, from the rest-of-queens=((1 3 6) (2 7 1))
and new-row=4, it will return ((1 3 6 4) (2 7 1 4). Note that the
first element of the returned value is not a valid combination of
queens, and it will be eliminated by the safe? filter"
  (map (lambda (x) (append x (list new-row)))
       rest-of-queens))

(define (safe? k positions)
  "returns true when the element of the last column (the last element
of `positions`) does not check any of the elements from the first
columns, and false otherwise."
  (let ((last (nth k (car positions))))
    (let ((z (zip (enumerate-interval 1 k)
                  (car positions))))
      (not (any-but-for-last
            (map (lambda (x)
                   (let ((i (car x)) (p (cadr x)))
                     (let ((w (- last p)))
                       (or (= w 0)
                           (= w (- k i))
                           (= w (- i k))))))
                 z))))))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(module+ test
  (define (out l)
    (define (iter l co)
      (if (null? l) (co "")
          (iter (cdr l)
                (lambda (x)
                  (format "\n~a~a" (caar l) (co x))))))
    (iter l (lambda (x) x)))
  ;; the are 92 solutions in dimension 8
  (let ((s (queens 8)))
    (display (format "~a\n---\ntotal ~a"
                     (out s)
                     (length s)))))

(module+ export
  (provide adjoin-position
           safe?
           empty-board))
