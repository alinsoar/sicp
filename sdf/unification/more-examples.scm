
;;; Backtracking is missing some assignments!

(unify-test '((?? x))
            '()
            '(dict (x () ??)))
'expect-value: 'matches-including-expected

(unify-test '((?? x) 1)
            '(1)
            '(dict (x () ??)))
'expect-value: 'matches-including-expected

(unify-test '(1)
            '((?? x) 1)
            '(dict (x () ??)))
'expect-value: 'matches-including-expected

(unify-test '(1 (?? x))
            '(1)
            '(dict (x () ??)))
'expect-value: 'matches-including-expected

(unify-test '(1)
            '(1 (?? x))
            '(dict (x () ??)))
'expect-value: 'matches-including-expected

(unify-test '((?? x))
            '(3 (?? y))
            '(dict (x (3 (?? y)) ??)))
'expect-value: 'matches-including-expected

(unify-test '(3 (?? y))
            '((?? x))
            '(dict (x (3 (?? y)) ??)))
'expect-value: 'matches-including-expected

(unify-test '((?? x) (?? y) (?? x))
            '(a b b a))
;; verbose output:
;; (((dict (y (a b b a) ??) (x () ??)) (a b b a) (a b b a) #t #f)
;;  ((dict (y (b b) ??) (x (a) ??)) (a b b a) (a b b a) #t #f))
'expect-value: 'matches-and-expected-some

(unify-test '((?? x) (?? y) (?? x))
            '(a b a b))
;; verbose output:
;; (((dict (y (a b a b) ??) (x () ??)) (a b a b) (a b a b) #t #f)
;;  ((dict (y () ??) (x (a b) ??)) (a b a b) (a b a b) #t #f))
'expect-value: 'matches-and-expected-some

(unify-test '((?? x) (?? y) (?? x))
            '(a b c a b))
;; verbose output:
;; (((dict (y (a b c a b) ??) (x () ??)) (a b c a b) (a b c a b) #t #f)
;;  ((dict (y (c) ??) (x (a b) ??)) (a b c a b) (a b c a b) #t #f))
'expect-value: 'matches-and-expected-some

(unify-test '((?? x) (?? y) (?? x) (?? y))
            '(a b c a b)
            #f)
'expect-value: 'no-matches-as-expected

(unify-test '((?? x) (?? y) (?? x) (?? x))
            '(a b c a b)
            '(dict (y (a b c a b) ??) (x () ??)))
'expect-value: 'matches-including-expected

(unify-test '((?? x) (?? x) (?? y) (?? x))
            '(a b c a b)
            '(dict (y (a b c a b) ??) (x () ??)))
'expect-value: 'matches-including-expected
