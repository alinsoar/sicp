

;; coderef: first-compose-value
((compose (lambda (x)
            (list 'foo x))
          (lambda (x)
            (list 'bar x)))
 'z)
'expect-value: '(foo (bar z))

;; coderef: full-compose-value
((compose (lambda (a b)
            (list 'foo a b))
          (lambda (x)
            (values (list 'bar x)
                    (list 'baz x))))
 'z)
'expect-value: '(foo (bar z) (baz z))

;; coderef: first-parallel-combine-value
((parallel-combine list
                   (lambda (x y z)
                     (list 'foo x y z))
                   (lambda (u v w)
                     (list 'bar u v w)))
 'a 'b 'c)
'expect-value: '((foo a b c) (bar a b c))

;; coderef: second-parallel-combine-value
((parallel-combine list
                   (lambda (x y z)
                     (values x y z))
                   (lambda (u v w)
                     (values w v u)))
 'a 'b 'c)
'expect-value: '(a b c c b a)

;; coderef: first-spread-combine-value
((spread-combine list
                 (lambda (x y)
                   (list 'foo x y))
                 (lambda (u v w)
                   (list 'bar u v w)))
 'a 'b 'c 'd 'e)
'expect-value: '((foo a b) (bar c d e))

;; coderef: second-spread-combine-value
((spread-combine list
                 (lambda (x y)
                   (values x y))
                 (lambda (u v w)
                   (values w v u)))
 'a 'b 'c 'd 'e)
'expect-value: '(a b e d c)

;; coderef: discard-argument-value
(((discard-argument 2)
  (lambda (x y z)
    (list 'foo x y z)))
 'a 'b 'c 'd)
'expect-value: '(foo a b d)

;; coderef: curry-argument-value
((((curry-argument 2)
   'a 'b 'c)
  (lambda (x y z w)
    (list 'foo x y z w)))
 'd)
'expect-value: '(foo a b d c)

;; coderef: permute-arguments-value
(((permute-arguments 1 2 0 3)
  (lambda (x y z w)
    (list 'foo x y z w)))
 'a 'b 'c 'd)
'expect-value: '(foo b c a d)
