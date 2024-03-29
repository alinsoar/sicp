
;;; To fix problem with dict header
(define (result-receiver dict n-eaten)
  `(success ,(match:bindings dict) ,n-eaten))

(define x-matcher (match:eqv 'x))

;; coderef: x-matcher-1
(x-matcher '(x) (match:new-dict) result-receiver)
;Value: (success () 1)

;; coderef: x-matcher-2
(x-matcher '(y) (match:new-dict) result-receiver)
;Value: #f

;; coderef: match-example-3
((match:element '(? x)) '(a) (match:new-dict) result-receiver)
;Value: (success ((x a ?)) 1)

;; coderef: match-example-4
((match:element '(? x)) '(a b) (match:new-dict) result-receiver)
;Value: (success ((x a ?)) 1)

;; coderef: match-example-5
((match:element '(? x)) '((a b) c)
                (match:new-dict) result-receiver)
;Value: (success ((x (a b) ?)) 1)


(define (print-all-results dict n-eaten)
  (pp `(success ,(match:bindings dict) ,n-eaten))
  ;; by returning #f we force backtracking.
  #f)

;; coderef: match-example-6
((match:segment '(?? a)) '(z z z) (match:new-dict) result-receiver)
;Value: (success ((a () ??)) 0)

;; coderef: match-example-7
((match:segment '(?? a)) '(z z z) (match:new-dict) print-all-results)
#|
(success ((a () ??)) 0)
(success ((a (z) ??)) 1)
(success ((a (z z) ??)) 2)
(success ((a (z z z) ??)) 3)
|#
;Value: #f

;; coderef: match-example-8
((match:list (list (match:eqv 'a)
                   (match:segment '(?? x))
                   (match:eqv 'b)))
 '((a 1 2 b))
 (match:new-dict)
 result-receiver)
;Value: (success ((x (1 2) ??)) 1)

;; coderef: match-example-9
((match:list (list (match:eqv 'a)
                   (match:segment '(?? x))
                   (match:eqv 'b)))
 '((a 1 2 b 3))
 (match:new-dict)
 result-receiver)
;Value: #f


;; coderef: match-example-10
(run-matcher
 (match:compile-pattern '(a ((? b) 2 3) (? b) c))
 '(a (1 2 3) 2 c)
 match:bindings)
;Value: #f

;; coderef: match-example-11
(run-matcher
 (match:compile-pattern '(a ((? b) 2 3) (? b) c))
 '(a (1 2 3) 1 c)
 match:bindings)
;Value: ((b 1 ?))

;; coderef: match-example-12
(run-matcher
 (match:compile-pattern '(a (?? x) (?? y) (?? x) c))
 '(a b b b b b b c)
 print-all-matches)
#|
((y (b b b b b b) ??) (x () ??))
((y (b b b b) ??) (x (b) ??))
((y (b b) ??) (x (b b) ??))
((y () ??) (x (b b b) ??))
|#
;Value: #f

;; coderef: match-example-13
(run-matcher
 (match:compile-pattern '(a ((? b) 2 3) (? b) c))
 '(a (1 2 3) 1 c)
 match:bindings)
;Value: ((b 1 ?))

#;
;;; These are to be built by students.
;; coderef: match-example-14
(run-matcher
 (match:compile-pattern '(?:choice a b (? x) c))
 'z
 match:bindings)
;Value ((x z ?))

#;
;; coderef: match-example-15
(run-matcher
 (match:compile-pattern
 `((? y) (?:choice a b (? x ,string?) (? y ,symbol?) c)))
 '(z z)
 match:bindings)
;Value ((y z ?))

#;
;; coderef: match-example-16
(run-matcher
 (match:compile-pattern `(?:choice b (? x ,symbol?)))
  'b
  print-all-matches)
#|
()
((x b ?))
|#
;Value: #f
