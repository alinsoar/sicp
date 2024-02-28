
(define-test 'match:pattern-names
  (lambda ()
    (assert-equal '(a b)
                  (match:pattern-names '((? a) (?? b))))))

(define-test 'matcher
  (lambda ()
    (assert-equal '((b 1 ?))
                  ((matcher '(a ((? b) 2 3) 1 c))
                   '(a (1 2 3) 1 c)))
    (let ((m (matcher '(a ((? b) 2 3) (? b) c))))
      (assert-false (m '(a (1 2 3) 2 c)))
      (assert-equal '((b 1 ?))
                    (m '(a (1 2 3) 1 c))))
    (assert-equal
     '(((y (b b b b b b) ??) (x () ??))
       ((y (b b b b) ??) (x (b) ??))
       ((y (b b) ??) (x (b b) ??))
       ((y () ??) (x (b b b) ??)))
     (let ((m (match:compile-pattern '(a (?? x) (?? y) (?? x) c)))
           (results '()))
       (m '((a b b b b b b c))
          (match:new-dict)
          (lambda (dictionary n)
            (set! results
                  (cons (match:bindings dictionary) results))
            #f))
       (reverse results)))
    (assert-equal '((b 1 ?))
                  ((matcher '(a ((? b) 2 3) (? b) c))
                   '(a (1 2 3) 1 c)))))