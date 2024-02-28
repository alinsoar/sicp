#lang racket

(require (submod "e.2.40.rkt" export))
(require (submod "e.2.42.rkt" export))

"
In both versions I inserted a printer of the size of the set of
combinations of queens that enter in the `safe?` filter, and the size
of the set of elements that come out from the `safe?` filter.

*** In the case of Louis, he does so for solving (queen-cols N): He
generates all possible new-rows from 1 to N, and for each row he does
so:

For the new-row = 1, he generates all solutions (queen-cols (N-1)).
For the new-row = 2, he generates all solutions (queen-cols (N-1)).
  ...
For the new-row = N, he generates all solutions (queen-cols (N-1)).

In total, he computes (queen-cols (N-1)) N times, and after that he
filters the list of all these combinations resulted by unifying all
these N steps -- in total a list in which each element of the
set (queen-cols (N-1)) appears N times, so K*N elements.

This has complexity N^N times the complexity to solve the filters,
which is N.  Apart from that, there are K*N steps to filter the
result, where K is the number of elements returned by
`queen-cols (N-1)`.  The returned elements are unified, and they are
duplicated N times in the set that is to be filtered.  So, in the case
of Louis, the complexity is a sum of powers, the greatest element
being (N^N + K*N).

*** In the first version that works fast, one does so to compute
`queen-cols (N-1)` :

   For each element of (queen-cols (N-1)) one adds a new possible row,
filter and return the results.  This has complexity N^2. 1 N comes
from filtering, and another N comes from computing recursively
previous sets `queen-cols (N-i)`, in total N such sets.

So, the difference of complexity between first version and Louis'
version has the order N^N.
"

(define (queens0 board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (let ((v (filter
                  (lambda (positions) (safe? k positions))
                  (let ((v (flatmap
                            (lambda (rest-of-queens)
                              (map (lambda (new-row)
                                     (adjoin-position new-row k rest-of-queens))
                                   (enumerate-interval 1 board-size)))
                            (queen-cols (- k 1)))))
                    (display (format "<-- ~a ~a\n" k (length v)))
                    v))))
          (display (format "----> ~a ~a\n" k (length v)))
          v)))
  (queen-cols board-size))

;; Louis's version of queens
(define (queens1 board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (let ((v (filter
                  (lambda (positions) (safe? k positions))
                  ;; next expression changed
                  (let ((v (flatmap
                            (lambda (new-row)
                              (map (lambda (rest-of-queens)
                                     (adjoin-position new-row k rest-of-queens))
                                   (queen-cols (- k 1))))
                            (enumerate-interval 1 board-size))))
                    (display (format "<-- ~a ~a\n" k (length v)))
                    v))))
          (display (format "----> ~a ~a\n" k (length v)))
          v)))
  (queen-cols board-size))

(module+ test
  (queens1 5)
  (queens0 5))

