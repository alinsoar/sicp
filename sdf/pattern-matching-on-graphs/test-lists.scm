
(define-test 'greedy-graph
  (lambda ()
    (let ((graph (list->graph '(a b c))))
      (test-graph graph #f)
      (assert-true (g:null? (g:cdr (g:cdr (g:cdr graph))))))))

(define-test 'lazy-graph
  (lambda ()
    (let ((graph (list->lazy-graph '(a b c))))
      (test-graph graph #t)
      (assert-true (g:null? (g:cdr (g:cdr (g:cdr graph))))))))

(define-test 'extensible-lazy-graph
  (lambda ()
    (let ((graph (list->extensible-lazy-graph '(a b c))))
      (test-graph graph #t)
      (g:append! graph (list->extensible-lazy-graph '(d e)))
      (assert-eqv (g:cdr (g:cdr (g:cdr graph)))
                  (g:cdr (g:cdr (g:cdr graph))))
      (assert-eqv 'd (g:car (g:cdr (g:cdr (g:cdr graph)))))
      (assert-eqv (g:cdr (g:cdr (g:cdr (g:cdr graph))))
                  (g:cdr (g:cdr (g:cdr (g:cdr graph)))))
      (assert-eqv (g:cdr (g:cdr (g:cdr (g:cdr graph))))
                  (g:last-pair graph))
      (assert-eqv 'e (g:car (g:cdr (g:cdr (g:cdr (g:cdr graph))))))
      (assert-eqv 'e (g:last graph)))))

(define (test-graph graph lazy?)
  (assert-eqv 'a (g:car graph))
  (if lazy?
      (assert-false ((graph 'get-edge 'cdr) 'forced?)))
  (assert-eqv (g:cdr graph)
              (g:cdr graph))
  (if lazy?
      (assert-true ((graph 'get-edge 'cdr) 'forced?)))
  (assert-eqv 'b (g:car (g:cdr graph)))
  (if lazy?
      (assert-false (((g:cdr graph) 'get-edge 'cdr) 'forced?)))
  (assert-eqv (g:cdr (g:cdr graph))
              (g:cdr (g:cdr graph)))
  (if lazy?
      (assert-true (((g:cdr graph) 'get-edge 'cdr) 'forced?)))
  (assert-eqv 'c (g:car (g:cdr (g:cdr graph))))
  (assert-eqv 'c (g:last graph)))