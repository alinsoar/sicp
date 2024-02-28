
(setup-propagator-system numeric-arithmetic)

(define (pythagorean integers)
  (let-cells (x y z x2 y2 z2)
    (p:amb x integers)
    (p:amb y integers)
    (p:amb z integers)
    (p:* x x x2)
    (p:* y y y2)
    (p:* z z z2)
    (p:+ x2 y2 z2)
    (list x y z)))
;Value: pythagorean

(initialize-scheduler)
;Value: ok

(all-results (pythagorean (iota 20 1))
             (lambda (strongest-contents)
               (write *number-of-calls-to-fail*)
               (display "  ")
               (pp (map get-base-value strongest-contents))))
1400  (3 4 5)
1779  (4 3 5)
2376  (5 12 13)
2690  (6 8 10)
3449  (8 6 10)
3646  (8 15 17)
3982  (9 12 15)
5034  (12 5 13)
5121  (12 9 15)
5274  (12 16 20)
6304  (15 8 17)
6792  (16 12 20)
;Value: no-more

*number-of-calls-to-fail*
;Value: 8565

